(ns tal
  (:require hiccup.core
	    clojure.xml
	    tales)
  (:import org.apache.commons.io.IOUtils)
  (:use clojure.contrib.str-utils))

;{:tag :html, :attrs nil, :content [{:tag :head, :attrs nil, :content [{:tag :title, :attrs {:tal:content context/title}, :content [Page Title]}]} {:tag :body, :attrs nil, :content [{:tag :h1, :attrs nil, :content [This is  {:tag :b, :attrs {:tal:replace template/title}, :content [the Title]}]} {:tag :table, :attrs {:id theTable, :border 1, :width 100%}, :content [{:tag :tr, :attrs nil, :content [{:tag :th, :attrs nil, :content [#]} {:tag :th, :attrs nil, :content [Id]} {:tag :th, :attrs nil, :content [Meta-Type]} {:tag :th, :attrs nil, :content [Title]}]} {:tag :tbody, :attrs {:tal:repeat item container/objectValues}, :content [{:tag :tr, :attrs {:bgcolor #EEEEEE, :tal:condition repeat/item/even}, :content [{:tag :td, :attrs {:class number, :tal:content repeat/item/number}, :content [#]} {:tag :td, :attrs {:tal:content item/id}, :content [Id]} {:tag :td, :attrs {:tal:content item/meta_type}, :content [Meta-Type]} {:tag :td, :attrs {:tal:content item/title}, :content [Title]}]} {:tag :tr, :attrs {:tal:condition repeat/item/odd}, :content [{:tag :td, :attrs {:tal:content repeat/item/number}, :content [#]} {:tag :td, :attrs {:tal:content item/id}, :content [Id]} {:tag :td, :attrs {:tal:content item/meta_type}, :content [Meta-Type]} {:tag :td, :attrs {:tal:content item/title}, :content [Title]}]}]}]}]}]}

(defn tal:replace [value context]
  (let [path (tales/path (->> value (re-split #"^text ") (last) (re-split #"^structure ") (last)) context)]
    (println (str value ": " path))
    (cond (= path :nothing)
	  :nothing
	  (= path :default)
	  ;(recur (rest attrs) (assoc element :attrs (dissoc (:attrs element) :tal:replace)))
	  :default
	  :else
	  (if (re-find #"^structure " value)
	    (str path)
	    (hiccup.core/h (str path))))))

(defn process-attrs [#^clojure.lang.PersistentArrayMap element 
		     context]
  (if (nil? (:attrs element)) ; if there are no attrs there is nothing to do
    element
    (loop [attrs (:attrs element) element element]
      (if (empty? attrs)
	element
	(let [a (first attrs)]
	  (cond (= (key a) :tal:content) ;argument ::= (['text'] | 'structure') expression
		(recur (rest attrs) (assoc element 
				      :content (let [path (tal:replace (val a) @context)]
						 (cond (= path :nothing)
						       []
						       (= path :default)
						       (:content element)
						       :else
						       [path]))
				      :attrs (dissoc (:attrs element) :tal:content)))
		(= (key a) :tal:replace) ; argument ::= (['text'] | 'structure') expression
		(let [path (tal:replace (val a) @context)]
		  (cond (= path :nothing)
			""
			(= path :default)
			(recur (rest attrs) (assoc element :attrs (dissoc (:attrs element) :tal:replace)))
			:else
			path))
		))))))

(defn process-element [#^clojure.lang.PersistentArrayMap element 
		       context]
  ;(println element)
  (let [element (process-attrs element context)]
    ;(cond (= (:tag element) :title) ; needs to cause side-effects
;	  (dosync (alter context assoc :template (assoc (:template @context) :title (first (:content element)))))
;	  )
    (if (string? element)
      element
      (vec
       (cons (:tag element) 
	     (cons (:attrs element)
		   (for [e (:content element)]
		     (cond (string? e)
			   e
			   (map? e)
			   (process-element e context)))))))))
      

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
  (hiccup.core/html 
   (let [r (process-element (clojure.xml/parse input) (ref context))]
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