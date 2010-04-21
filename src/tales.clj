(ns tales
  (:use clojure.contrib.str-utils))

(defn path [p context]
  (cond (= (first p) \space)
	(path (.trim p) context)
	(re-find #"^path:" p)
	(path (last (re-split #"^path:" p)) context)
	:else
	(first 
	 (remove nil? 
		 (map #(get-in context %) 
		      (for [p (re-split #"\|" p)]
			(map keyword (re-split #"\/" (.trim p)))))))))
	     