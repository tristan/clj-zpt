(ns tal
  (:require hiccup.core
	    clojure.xml
	    tales
	    clojure.stacktrace)
  (:import org.apache.commons.io.IOUtils
	   KeyError)
  (:use clojure.contrib.str-utils))

(defn tal:replace [argument global local]
; argument ::= (['text'] | 'structure') expression
  (let [path (tales/evaluate (re-gsub #"^text |^structure " "" argument) (conj @global local))]
    (cond (= path :nothing)
	  ""
	  (= path :default)
	  nil
	  :else
	  (if (re-find #"^structure " argument)
	    (str path)
	    (hiccup.core/h (str path))))))

(defn tal:content [argument global local]
; argument ::= (['text'] | 'structure') expression
  (tal:replace argument global local))

(defn tal:define [argument global local]
; argument       ::= define_scope [';' define_scope]*
; define_scope   ::= (['local'] | 'global') define_var
; define_var     ::= variable_name expression
; variable_name  ::= Name
  (loop [arguments (re-split #"(?<!;);(?!;)" argument) local local]
    (if (empty? arguments)
      local
      (let [argument (first arguments)
	    scope (re-find #"^local|^global" argument)
	    define_var (re-gsub #"^local |^global " "" argument)
	    [variable_name expression] (re-split #" " define_var 2)
	    result (tales/evaluate expression (conj @global local))
	    ]
	(when (= scope "global") (dosync (alter global assoc (keyword variable_name) result)))
	(recur (rest arguments)
	       (if (= scope "global") ; TODO: is (= (keyword "string") :string) more efficient
		 local
		 (assoc local (keyword variable_name) result)))))))

(defn tal:condition [argument global local]
;argument ::= expression
  (let [result (tales/evaluate argument (conj @global local))]
    (if (and result (not (= result :nothing)))
      true
      false)))

(defn tal:attributes [argument attrs global local]
; argument             ::= attribute_statement [';' attribute_statement]*
; attribute_statement  ::= attribute_name expression
; attribute_name       ::= [namespace ':'] Name
; namespace            ::= Name
  (loop [arguments (re-split #"(?<!;);(?!;)" argument) attrs attrs]
    (if (empty? arguments)
      attrs
      (let [attribute_statement (first arguments)
	    [attribute_name expression] (re-split #" " attribute_statement 2)
	    ; since hiccups doesn't know about namespaces yet,
	    ; ignoring difference between namespace and name for now
	    result (tales/evaluate expression (conj @global local))
	    ]
	(recur (rest arguments)
	       (cond (= result :nothing)
		     (dissoc attrs (keyword attribute_name))
		     (= result :default)
		     attrs
		     :else
		     (assoc attrs (keyword attribute_name) result)))))))


; TODO: throw error on unknown tal commands
(defn process-element [element 
		       global local]
  (try
  (let [unsupported (filter #(re-find #"^:tal:" (str (key %)))
			    (dissoc (:attrs element) :tal:define :tal:repeat :tal:replace :tal:content :tal:attributes :tal:omit-tag :tal:on-error))
	element
	(cond
	 (empty? (:attrs element)) ; nothing to do if we have no attrs
	 element
	 ; error checking
	 (> (count (filter #(= (key %) :tal:define) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'define'"))
	 (> (count (filter #(= (key %) :tal:repeat) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'repeat'"))
	 (> (count (filter #(= (key %) :tal:replace) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'replace'"))
	 (> (count (filter #(= (key %) :tal:content) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'content'"))
	 (> (count (filter #(= (key %) :tal:attributes) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'attributes'"))
	 (> (count (filter #(= (key %) :tal:omit-tag) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'omit-tag'"))
	 (not (empty? unsupported))
	 (throw (tal.TALError. (str "bad TAL attribute: '" (re-gsub #"^:tal:" "" (str (key (first unsupported)))) "'")))
	 (> (count (filter #(= (key %) :tal:on-error) (:attrs element))) 1)
	 (throw (tal.TALError. "duplicate TAL attribute 'on-error'"))
	 (and (contains? element :tal:replace) (contains? element :tal:content))
	 (throw (tal.TALError. "tal:content and tal:replace are mutually exclusive"))
	 :else ; no errors yet
	 (let [local ; tal:define
	       (if (contains? (:attrs element) :tal:define)
		 (tal:define (:tal:define (:attrs element)) global local)
		 local)
	       ]
	   (if (not-every? true? (map #(tal:condition (val %) global local)
				      (filter #(= (key %) :tal:condition) (:attrs element))))
	     "" ; one or more tal:conditions returned false, so omit this element
	     (if (contains? (:attrs element) :tal:repeat) ; process tal:repeat if present
	       (let [[variable_name expression] (re-split #" " (:tal:repeat (:attrs element)) 2)
		     variable_name (keyword variable_name)
		     repeat (tales/evaluate expression (conj @global local))
		     attrs (dissoc (:attrs element) :tal:repeat :tal:condition :tal:define)
		     element (assoc element :attrs attrs)]
		 (vec
		  (for [index (range (count repeat))]
		    (first
		     (process-element element
				      global
				      (conj local {:repeat 
						   {variable_name
						    {:index index
						     :number (inc index)
						     :even (even? index)
						     :odd (odd? index)
						     :start (= 0 index)
						     :end (= (dec (count repeat)) index)
						     :length (count repeat)}}
						   variable_name (nth repeat index)}))))))
	       (let [element (if (contains? (:attrs element) :tal:replace) ; process tal:replace
			       ; NOTE: the current replace implementation will ignore tal:attributes
			       ; unless the replace is cancelled
			       (or (tal:replace (:tal:replace (:attrs element)) global local) element)
			       (if (contains? (:attrs element) :tal:content) ; process tal:content
				 (let [new-content (tal:content (:tal:content (:attrs element)) global local)]
				   (if new-content
				     (assoc element 
				       :content [new-content])
				     element))
				 element))]
		 (if (string? element)
		   element ; if the element has been replaced with a string, just return it
		   (let [element (if (contains? (:attrs element) :tal:attributes) ; tal:attributes
				   (assoc element :attrs (tal:attributes (:tal:attributes (:attrs element))
									 (:attrs element)
									 global local))
				   element)]
		     (if (and (contains? (:attrs element) :tal:omit-tag) ; tal:omit-tag
			      (or (= (:tal:omit-tag (:attrs element)) "")
				  (tal:condition (:tal:omit-tag (:attrs element)) global local)))
		       (vec
			(apply concat (for [e (:content element)]
					(process-element e global local))))
		       element))
		   ))))))]
    (cond (string? element)
	  [element]
	  (vector? element)
	  element
	  :else
	  (vector ; TODO: i'm not fond of all this vector wrapping, is it really necessary? 
	  (vec
	   (cons (:tag element)
		 (cons (apply dissoc (:attrs element) (filter #(re-find #"^:tal:" (str %)) (keys (:attrs element))))
		       (apply concat (for [e (:content element)]
				       (process-element e global local)
				       ))))))))
  (catch Exception e
    ; TODO: handle cases such as "(if) nothing"
    (if (and (contains? (:attrs element) :tal:on-error) 
	     (not (= tal.TALError (class (clojure.stacktrace/root-cause e)))))
      (process-element (assoc element :attrs {:tal:content (:tal:on-error (:attrs element))})
		       global local)
      (throw (clojure.stacktrace/root-cause e)))) ; TODO: figure out where java.lang.reflect.InvocationTargetException comes from
  ))

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
  ;(println (clojure.xml/parse input))
  (hiccup.core/html 
   (let [r (first (process-element (clojure.xml/parse input) (ref context) {}))]
     (println r)
     r)
  )
)

(defmethod compile-html-template [1 true] [#^String input 
					   #^clojure.lang.PersistentArrayMap context] ; input is string
  (compile-html-template
   (let [f (java.io.File. input)]
     (if (.exists f)
       (java.io.FileInputStream f)
       (IOUtils/toInputStream input)))
   context))

(defmethod compile-html-template [2 true] [#^java.io.File input
					   #^clojure.lang.PersistentArrayMap context]
  (compile-html-template (java.io.FileInputStream. input) context))