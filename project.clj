; leiningen project file
(defproject clj-zpt "0.0.1-SNAPSHOT"
  :description ""
  :url ""
  :dependencies [[hiccup "0.2.5" :exclusions [org.clojure/clojure
					      org.clojure/clojure-contrib]]
		 [commons-io/commons-io "1.4"]
		 ]
  :dev-dependencies [[org.clojure/clojure "1.1.0"]]
  :namespaces [clj-zpt.KeyError clj-zpt.tal.TALError] ; only compiling 
  )
