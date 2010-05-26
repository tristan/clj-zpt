(ns clj-zpt.tal
  (:require hiccup.core
	    clojure.xml
	    [clj-zpt.tales :as tales]
	    clojure.stacktrace)
  (:import org.apache.commons.io.IOUtils
	   clj_zpt.KeyError
	   clj_zpt.tal.TALError)
  (:use clojure.contrib.str-utils))

(defn dissoc-in [m [k & ks]]
  (if ks
    (assoc m k (dissoc-in (get m k) ks))
    (dissoc m k)))

(declare process-element)
(declare compile-template)

(defn finalise-element [element defining-macro? macros]
  (let [tag `[~(:tag element)
	      (let [~'attrs (remove nil? (list
					  ~@(for [attr (or (dissoc (:tal:attrs element) :tal:on-error :tal:omit-tag) 
							   (dissoc (:attrs element) :tal:content :tal:on-error :tal:omit-tag))]
					      (if (seq? (val attr))
						`(let [~'r ~(val attr)]
						   (cond (= ~'r :nothing)
							 nil
							 (= ~'r :default)
							 ~(if (get-in element [:attrs (key attr)])
							    `[~(key attr) ~(get-in element [:attrs (key attr)])]
							    `nil)
							 :else
							 [~(key attr) ~'r]))
						`[~(key attr) ~(val attr)]))))]
		(zipmap (map first ~'attrs) (map second ~'attrs)))
	      ~(if (contains? (:attrs element) :tal:content) ; no need to check if :tal:replace is present, that case should have been handled by tal:replace
		 (let [structure (re-find #"^structure " (:tal:content (:attrs element)))
		       expression (re-gsub #"^text |^structure " "" (:tal:content (:attrs element)))]
		   `(let [~'r (tales/evaluate ~expression (conj ~'@global ~'local))]
		      (cond (= ~'r :nothing) ; replace with nothing
			    nil
			    (= ~'r :default) ; ignore replace
			    (list ~@(for [c (:content element)]
				      (process-element c defining-macro? macros)))
			    :else ; replace this element
			    ~(if structure
			       `(str ~'r)
			       `(hiccup.core/h (str ~'r))))))
		 ;(if (= (:tag element) :script) ; TODO: this is a quick work around for js problems, need something better
		 ;  `(list ~@(for [c (:content element)]
		;	      c))
		   `(list ~@(for [c (:content element)]
			      (process-element c defining-macro? macros))))]]
    (if (contains? (:attrs element) :tal:omit-tag)
      (if (= (:tal:omit-tag (:attrs element)) "")
	(cons 'list (drop 2 tag))
	`(let [~'tag ~tag]
	   (if (tales/coerce-bool (tales/evaluate ~(:tal:omit-tag (:attrs element)) (conj ~'@global ~'local)))
	     (drop 2 ~'tag)
	     ~'tag)))
      tag)))

(defn get-macro [path global local]
  (println @global)
  (let [m (tales/path (str path "|nothing") (conj @global local))]
    (if (= :nothing m)
      (let [sp (re-split #"\/" path)
	    template (str-join "/" (take-while #(not (= "macros" %)) sp))
	    input (.getResourceAsStream (clojure.lang.RT/baseLoader) template)] ; attempt to find the template on the classpath
	;(println "trying to get template:" template "......" (if input "success!" "failed :("))
	(if (nil? input)
	  (throw (KeyError. (str "unable to resolve path: " path)))
	  (let [rp (str (str-join "/" (drop-while #(not (= "macros" %)) sp)) "|nothing")
		tmp (compile-template input)
		m (tales/path rp tmp)]
	    ;(println "trying to find path:" rp)
	    ;(println "looking in:" tmp)
	    ;(println "got:" m)
	    (if (= :nothing m)
	      (throw (KeyError. (str "unable to resolve path: " path)))
	      (do 
		(dosync ; store macros so they don't have to be retrieved from the recource again
		 (alter global
			assoc-in
			(conj (vec (map keyword (take-while #(not (= "macros" %)) sp))) :macros)
			(:macros tmp)))
		m)))))
      m)))

(defn get-slots [element macros]
  (loop [kids (:content element) slots {}]
    (if (empty? kids)
      slots
      (let [e (first kids)]
	(if (map? e)
	  (if (contains? (:attrs e) :metal:fill-slot)
	    (recur (rest kids)
		   (assoc slots
		     (keyword (get-in e [:attrs :metal:fill-slot]))
		     (process-element (dissoc-in e [:attrs :metal:fill-slot]) false macros))) ; TODO: defining-macro? need to actually be set here?
	    (recur (rest kids)
		   (conj slots (get-slots e macros))))
	  (recur (rest kids) slots))))))

(defn metal:use-macro [element defining-macro? macros]
  (if (contains? (:attrs element) :metal:use-macro)
    `(let [~'m (get-macro ~(:metal:use-macro (:attrs element)) ~'global ~'local)]
       (~'m ~'global ~'local ~(get-slots element macros)))
    (finalise-element element defining-macro? macros)))

(defn tal:attributes [element defining-macro? macros]
  (if (contains? (:attrs element) :tal:attributes)
    (loop [arguments (re-split #"(?<!;);(?!;)" (:tal:attributes (:attrs element))) attrs (dissoc (:attrs element) :tal:attributes)]
      (if (empty? arguments)
	(metal:use-macro (dissoc-in (assoc element :tal:attrs attrs) [:attrs :tal:attributes]) defining-macro? macros)
	(let [attribute_statement (first arguments)
	      [attribute_name expression] (re-split #" " attribute_statement 2)
	      ; since hiccups doesn't know about namespaces yet,
	      ; ignoring difference between namespace and name for now
	      result `(tales/evaluate ~expression (conj ~'@global ~'local))
	      ]
	  (recur (rest arguments)
		 (assoc attrs (keyword attribute_name) result)))))
    (metal:use-macro element defining-macro? macros)))
      
(defn tal:replace [element defining-macro? macros]
  (if (contains? (:attrs element) :tal:replace)
    (if (contains? (:attrs element) :tal:content)
      (throw (TALError. "tal:content and tal:replace are mutually exclusive"))
      (let [structure (re-find #"^structure " (:tal:replace (:attrs element)))
	    expression (re-gsub #"^text |^structure " "" (:tal:replace (:attrs element)))]
      `(let [~'r (tales/evaluate ~expression (conj ~'@global ~'local))]
	 (cond (= ~'r :nothing) ; replace with nothing
	       ""
	       (= ~'r :default) ; ignore replace
	       ~(tal:attributes (dissoc-in element [:attrs :tal:replace]) defining-macro? macros)
	       :else ; replace this element
	       ~(if structure
		  `(str ~'r)
		  `(hiccup.core/h (str ~'r)))))))
    (tal:attributes element defining-macro? macros)))

(defn tal:repeat [element defining-macro? macros]
  (if (contains? (:attrs element) :tal:repeat)
    (let [[variable_name expression] (re-split #" " (:tal:repeat (:attrs element)) 2)
	  variable_name (keyword variable_name)]
      `(let [~'repeat (tales/evaluate ~expression (conj ~'@global ~'local))]
	 (for [~'index (range (count ~'repeat))]
	   (let [~'local (conj ~'local {:repeat {~variable_name
						 {:index ~'index
						  :number (inc ~'index)
						  :even (even? ~'index)
						  :odd (odd? ~'index)
						  :start (= 0 ~'index)
						  :end (= (dec (count ~'repeat)) ~'index)
						  :length (count ~'repeat)}}
					~variable_name (nth ~'repeat ~'index)})]
	     ~(tal:replace (dissoc-in element [:attrs :tal:repeat]) defining-macro? macros)))))
    (tal:replace element defining-macro? macros)))

(defn tal:condition [element defining-macro? macros]
  (if (contains? (:attrs element) :tal:condition)
    `(if (tales/coerce-bool (tales/evaluate ~(:tal:condition (:attrs element)) (conj ~'@global ~'local)))
       ~(tal:repeat (dissoc-in element [:attrs :tal:condition]) defining-macro? macros)
       "")
    (tal:repeat element defining-macro? macros)))

(defn tal:define [element defining-macro? macros]
  (if (contains? (:attrs element) :tal:define) ; process tal:define
    (letfn [(do-arg [arguments]
		    (if (empty? arguments)
		      (list (tal:condition (dissoc-in element [:attrs :tal:define]) defining-macro? macros))
		      (let [argument (first arguments)
			    scope (re-find #"^local|^global" argument)
			    define_var (re-gsub #"^local |^global " "" argument)
			    [variable_name expression] (re-split #" " define_var 2)
			    result `(tales/evaluate ~expression (conj ~'@global ~'local))]
			(if (= scope "global")
			  (cons `(dosync (alter ~'global assoc ~(keyword variable_name) ~result))
				`(~@(do-arg (rest arguments))))
			  (list `(let [~'r ~result
				       ~'local (if (= ~'r :default)
						~'local
						(assoc ~'local ~(keyword variable_name) ~'r))]
				   ~@(do-arg (rest arguments))))))))]
      (do-arg (re-split #"(?<!;);(?!;)" (get-in element [:attrs :tal:define]))))
    (tal:condition element defining-macro? macros))) ; need list incase initial element is the vector

(defn tal:on-error [element defining-macro? macros] ; TODO: on error should go before the metal stuff
  (if (contains? (:attrs element) :tal:on-error)
    `(try
      ~(tal:define (dissoc-in element [:attrs :tal:on-error]) defining-macro? macros)
      (catch KeyError ~'e
	~(if (= (:tal:on-error (:attrs element)) "")
	   ""
	   `(tales/evaluate ~(:tal:on-error (:attrs element)) (conj ~'@global ~'local)))))
    (tal:define element defining-macro? macros)))

(defn metal:define-slot [element defining-macro? macros]
  (if (contains? (:attrs element) :metal:define-slot)
    (if defining-macro? ; TODO: since define-slot is skipped unless defining-macro? is true, this is unneccessary
      (let [slot (keyword (get-in element [:attrs :metal:define-slot]))]
	`(if (contains? ~'slots ~slot)
	   (get ~'slots ~slot)
	   ~(tal:on-error (dissoc-in element [:attrs :metal:define-slot]) defining-macro? macros)))
      (throw (Exception. "metal:define-slot without metal:define-macro doesn't make sense to me!")))
    (tal:on-error element defining-macro? macros)))

(defn metal:define-macro [element defining-macro? macros]
  (if (contains? (:attrs element) :metal:define-macro)
    (if defining-macro?
      (throw (Exception. "defining a macro inside another macro doesn't make sense to me!")) ; TODO: maybe i should figure it out though?
      (do
	(let [macro (metal:define-slot (dissoc-in element [:attrs :metal:define-macro])
				     true
				     macros)]
	  (dosync (alter macros assoc (keyword (get-in element [:attrs :metal:define-macro]))
			 (eval `(fn [~'global ~'local ~'slots] ~macro))))
	  `(let [~'slots {}] ~macro))))
    (if defining-macro?
      (metal:define-slot element defining-macro? macros)
      (tal:on-error element defining-macro? macros))))

(defn process-element [element defining-macro? macros]
  (cond (map? element)
	(let [x (metal:define-macro element defining-macro? macros)]
	  (if (seq? (first x))
	    (cons 'do x)
	    x))
	(string? element)
	`(tales/string ~element (conj ~'@global ~'local)) ; TODO: should only call this for strings that have $ or ${ in them
	:else
	element))

; TODO: template caching
(defn compile-template [#^java.io.InputStream input]
  (println "compiling template...")
  (let [x (clojure.xml/parse input)
	m (ref {})
	h (process-element x false m)
	r {:renderer ; NOTE: had to make sure the eval was done first otherwise the macros ref isn't set
	   (eval `(fn [~'global]
		    (let [~'global (ref ~'global) ~'local {}
			  ~'res ~h]
		      (hiccup.core/html ~'res))))}]
    (assoc r :macros @m)))     

(defmulti compile-html-template 
  (fn [input context]
    [(cond (instance? java.io.InputStream input)
	   0
	   (instance? String input)
	   1
	   (instance? java.io.File input)
	   2
	   :else
	   (throw (Exception. "unsupported input type"))
	   )
	   true]))

(defmethod compile-html-template [0 true] [#^java.io.InputStream input 
					   #^clojure.lang.PersistentArrayMap context] ; input is inputstream
  (let [func (compile-template input)]
    (func context))
)

(defmethod compile-html-template [1 true] [#^String input 
					   #^clojure.lang.PersistentArrayMap context] ; input is string
  (compile-html-template
   (let [f (java.io.File. input)]
     (if (.exists f)
       (java.io.FileInputStream. f)
       (IOUtils/toInputStream input)))
   context))

(defmethod compile-html-template [2 true] [#^java.io.File input
					   #^clojure.lang.PersistentArrayMap context]
  (compile-html-template (java.io.FileInputStream. input) context))