(defproject land-of-lisp "0.1.0-SNAPSHOT"
  :description "Games from 'Land of Lisp' rewritten in Clojure (just for funsies)"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure       "1.6.0"]
                 [fenrir                    "0.1.1"]
                 [clojure-lanterna          "0.9.4"]
                 [ring/ring-core            "1.4.0-RC1"]
                 [ring/ring-jetty-adapter   "1.3.2"]
                 [javax.servlet/servlet-api "2.5"]
                 [compojure                 "1.3.4"]]
  :main land-of-lisp.web-server)
