(ns tales
  (:use clojure.contrib.str-utils))

(defn path [p context]
  (println "path:" p)
  (cond (= (first p) \space)
	(path (.trim p) context)
	(re-find #"^path:" p)
	(path (last (re-split #"^path:" p)) context)
	:else
	(or
	 (first 
	  (remove nil? 
		  (map #(cond (= (first %) :default)
			      :default
			      (= (first %) :nothing)
			      :nothing
			      :else
			      (get-in context %))
		       (for [p (re-split #"\|" p)]
			 (map keyword (re-split #"\/" (.trim p)))))))
	 :nothing)))
	     