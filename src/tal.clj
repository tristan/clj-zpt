(ns tal
  (:require hiccup.core
	    clojure.xml
	    tales)
  (:import org.apache.commons.io.IOUtils))

;{:tag :html, :attrs nil, :content [{:tag :head, :attrs nil, :content [{:tag :title, :attrs {:tal:content context/title}, :content [Page Title]}]} {:tag :body, :attrs nil, :content [{:tag :h1, :attrs nil, :content [This is  {:tag :b, :attrs {:tal:replace template/title}, :content [the Title]}]} {:tag :table, :attrs {:id theTable, :border 1, :width 100%}, :content [{:tag :tr, :attrs nil, :content [{:tag :th, :attrs nil, :content [#]} {:tag :th, :attrs nil, :content [Id]} {:tag :th, :attrs nil, :content [Meta-Type]} {:tag :th, :attrs nil, :content [Title]}]} {:tag :tbody, :attrs {:tal:repeat item container/objectValues}, :content [{:tag :tr, :attrs {:bgcolor #EEEEEE, :tal:condition repeat/item/even}, :content [{:tag :td, :attrs {:class number, :tal:content repeat/item/number}, :content [#]} {:tag :td, :attrs {:tal:content item/id}, :content [Id]} {:tag :td, :attrs {:tal:content item/meta_type}, :content [Meta-Type]} {:tag :td, :attrs {:tal:content item/title}, :content [Title]}]} {:tag :tr, :attrs {:tal:condition repeat/item/odd}, :content [{:tag :td, :attrs {:tal:content repeat/item/number}, :content [#]} {:tag :td, :attrs {:tal:content item/id}, :content [Id]} {:tag :td, :attrs {:tal:content item/meta_type}, :content [Meta-Type]} {:tag :td, :attrs {:tal:content item/title}, :content [Title]}]}]}]}]}]}

(defn process-attrs [#^clojure.lang.PersistentArrayMap element 
		     context]
  (if (nil? (:attrs element)) ; if there are no attrs there is nothing to do
    element
    (loop [attrs (:attrs element) element element]
      (if (empty? attrs)
	element
	(let [a (first attrs)]
	  (cond (= (key a) :tal:content)
		(recur (rest attrs) (assoc element :content [(tales/path (val a) @context)] 
					           :attrs (dissoc (:attrs element) :tal:content)))

		))))))

(defn process-element [#^clojure.lang.PersistentArrayMap element 
		       context]
  ;(println element)
  (let [element (process-attrs element context)]
    (cond (= (:tag element) :title) ; needs to cause side-effects
	  (dosync (alter context assoc :template (assoc (:template @context) :title (first (:content element)))))
	  )
    (vec
     (cons (:tag element) 
	   (cons (:attrs element)
		 (for [e (:content element)]
		   (cond (string? e)
			 e
			 (map? e)
			 (vec (process-element e context)))))))))
      

(defmulti compile-html-template 
  (fn [input context]
    [(cond (instance? java.io.InputStream input)
	   0
	   (instance? String input)
	   1
	   :else
	   (throw (Exception. "unsupported input type"))
	   )
	   true]))

(defmethod compile-html-template [0 true] [#^java.io.InputStream input 
					   #^clojure.lang.PersistentArrayMap context] ; input is inputstream
  (hiccup.core/html (process-element (clojure.xml/parse input) (ref {:context context :template {}}))))

(defmethod compile-html-template [1 true] [#^String input 
					   #^clojure.lang.PersistentArrayMap context] ; input is string
  (compile-html-template
   (let [f (java.io.File. input)]
     (if (.exists f)
       (java.io.FileInputStream f)
       (IOUtils/toInputStream input)))
   context))