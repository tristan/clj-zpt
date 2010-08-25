(ns clj-zpt
  (:require [clj-zpt.tal :as tal]))

(defn get-template [#^String file] 
  (tal/compile-template 
   (let [f (.getResourceAsStream (clojure.lang.RT/baseLoader) file)]
     (if (nil? f)
       (throw (java.io.FileNotFoundException. file))
       f))))

(def template-cache (ref {}))
(defn get-template-from-cache [#^String file]
  (let [f (java.io.File. file)
	t (@template-cache file)]
    (if (and (not (nil? t)) ; if the template has been modified, recompile it
	     (.exists f)
	     (= (.lastModified f) (t :lm)))
      (t :fn)
      (let [template (get-template file)
	    last-modified (.lastModified (java.io.File. file))]
	(dosync
	 (alter template-cache assoc file {:fn template :lm last-modified}))
	template))))