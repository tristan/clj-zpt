(ns clj-zpt.tales
  (:use clojure.contrib.str-utils)
  (:import clj_zpt.KeyError))

(def extensions (ref {}))

(defn register-extension [key handler]
  (dosync (alter extensions assoc (str key ":") handler))
  (println "registered TALES extension:" key))

(defn missing-key-in?
  "similar to get-in except returns the first key which isn't in the map or false if all keys exist"
  [m ks]
  (if (empty? ks)
    false
    (if (contains? m (first ks))
      (recur (get m (first ks))
	     (rest ks))
      (first ks))))

(defn path [p context]
  (let [results 
	(map #(cond (= (first %) :default)
		    :default
		    (= (first %) :nothing)
		    :nothing
		    :else
		    (let [r (get-in context %)]
		      (if (nil? r)
			(let [mk (missing-key-in? context %)]
			  (if mk
			    (KeyError. (.substring (str mk) 1))
			    :nothing)) ; replace nil values with :nothing
			r)))
	     (for [p (re-split #"\|" p)]
	       (map keyword (re-split #"\/" (.trim p)))))
	r (first (drop-while #(instance? Exception %) results))]
    (if (nil? r)
      (throw (first results))
      r)))

(defn string [s context]
  (reduce #(re-gsub (re-pattern (str "(?<!\\$)\\$" %2 "(?=[ ]+|$)|(?<!\\$)\\$\\{" %2 "\\}")) 
		    (path %2 context) %1)
	  (cons s
		(distinct 
		 (concat
		  (map #(.substring % 1) (re-seq #"(?<!\$)\$(?!\{|\$)[^ ]+" s))
		  (map #(.substring % 2 (dec (count %))) (re-seq #"(?<!\$)\$\{[^\}^\{]*\}{1}" s)))))))

(defn evaluate [s context]
  (cond (re-find #"^string:" s)
	(string (re-gsub #"^string:[ ]*" "" s) context)
	(re-find #"^python:" s)
	(throw (Exception. "python prefix not supported")) ; TODO: jython?
	(re-find #"^not:" s)
	(let [r (evaluate (re-gsub #"^not:[ ]*" "" s) context)]
	  (cond (or (true? r) (false? r))
		(not r)
		(integer? r)
		(= r 0)
		(or (nil? r) (= :nothing r))
		true
		:else
		(empty? r)))
	(re-find #"^path:" s)
	(path (.trim (re-gsub #"^path:" "" s)) context)
	(get @extensions (re-find #"^[\w]+:" s))
	((get @extensions (re-find #"^[\w]+:" s)) (.trim (re-gsub #"^[\w]+:" "" s)) context)
	:else ; assume path
	(path (.trim s) context)
	))