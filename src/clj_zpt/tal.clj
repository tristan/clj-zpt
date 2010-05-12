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

(defn finalise-element [element]
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
				    (process-element c)))
			  :else ; replace this element
			  ~(if structure
			     `(str ~'r)
			     `(hiccup.core/h (str ~'r))))))
		 `(list ~@(for [c (:content element)]
			    (process-element c))))]]
    (if (contains? (:attrs element) :tal:omit-tag)
      (if (= (:tal:omit-tag (:attrs element)) "")
	(cons 'list (drop 2 tag))
	`(let [~'tag ~tag]
	   (if (tales/coerce-bool (tales/evaluate ~(:tal:omit-tag (:attrs element)) (conj ~'@global ~'local)))
	     (drop 2 ~'tag)
	     ~'tag)))
      tag)))

(defn tal:attributes [element]
  (if (contains? (:attrs element) :tal:attributes)
    (loop [arguments (re-split #"(?<!;);(?!;)" (:tal:attributes (:attrs element))) attrs (dissoc (:attrs element) :tal:attributes)]
      (if (empty? arguments)
	(finalise-element (dissoc-in (assoc element :tal:attrs attrs) [:attrs :tal:attributes]))
	(let [attribute_statement (first arguments)
	      [attribute_name expression] (re-split #" " attribute_statement 2)
	      ; since hiccups doesn't know about namespaces yet,
	      ; ignoring difference between namespace and name for now
	      result `(tales/evaluate ~expression (conj ~'@global ~'local))
	      ]
	  (recur (rest arguments)
		 (assoc attrs (keyword attribute_name) result)))))
    (finalise-element element)))
      
(defn tal:replace [element]
  (if (contains? (:attrs element) :tal:replace)
    (if (contains? (:attrs element) :tal:content)
      (throw (TALError. "tal:content and tal:replace are mutually exclusive"))
      (let [structure (re-find #"^structure " (:tal:replace (:attrs element)))
	    expression (re-gsub #"^text |^structure " "" (:tal:replace (:attrs element)))]
      `(let [~'r (tales/evaluate ~expression (conj ~'@global ~'local))]
	 (cond (= ~'r :nothing) ; replace with nothing
	       ""
	       (= ~'r :default) ; ignore replace
	       ~(tal:attributes (dissoc-in element [:attrs :tal:replace]))
	       :else ; replace this element
	       ~(if structure
		  `(str ~'r)
		  `(hiccup.core/h (str ~'r)))))))
    (tal:attributes element)))

(defn tal:repeat [element]
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
	     ~(tal:replace (dissoc-in element [:attrs :tal:repeat]))))))
    (tal:replace element)))

(defn tal:condition [element]
  (if (contains? (:attrs element) :tal:condition)
    `(if (tales/coerce-bool (tales/evaluate ~(:tal:condition (:attrs element)) (conj ~'@global ~'local)))
       ~(tal:repeat (dissoc-in element [:attrs :tal:condition]))
       "")
    (tal:repeat element)))

(defn tal:define [element]
  (if (contains? (:attrs element) :tal:define) ; process tal:define
    (letfn [(do-arg [arguments]
		    (if (empty? arguments)
		      (list (tal:condition (dissoc-in element [:attrs :tal:define])))
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
    (tal:condition element))) ; need list incase initial element is the vector

(defn tal:on-error [element]
  (if (contains? (:attrs element) :tal:on-error)
    `(try
      ~(tal:define (dissoc-in element [:attrs :tal:on-error]))
      (catch KeyError ~'e
	~(if (= (:tal:on-error (:attrs element)) "")
	   ""
	   `(tales/evaluate ~(:tal:on-error (:attrs element)) (conj ~'@global ~'local)))))
    (tal:define element)))

(defn process-element [element]
  (cond (map? element)
	(let [x (tal:on-error element)]
	  (if (seq? (first x))
	    (cons 'do x)
	    x))
	(string? element)
	`(tales/string ~element (conj ~'@global ~'local)) ; TODO: should only call this for strings that have $ or ${ in them
	:else
	element))

(defn compile-template [#^java.io.InputStream input]
  (println "compiling template...")
  (let [x (clojure.xml/parse input)
	h (process-element x)]
    ;(println "..................")
    ;(println x)
    ;(println "..................")
    ;(println h)
    ;(println "..................")
    (eval `(fn [~'global]
	      (let [~'global (ref ~'global) ~'local {}
		    ~'res ~h]
		;(println ~h)
		;(println "............")
		(hiccup.core/html ~'res))))))

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