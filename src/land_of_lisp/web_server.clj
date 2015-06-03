(ns land-of-lisp.web-server
  (:require [clojure.string :as str]))

(defn- hex->int
  "Attempt to parse a hex number string (e.g. '3f') to an integer.
   If argument is  not a valid hex number, returns nil."
  [s]
  (when (every? #(contains? (set "0123456789abcdefABCDEF") %) s)
    (read-string (str "0x" s))))

(defn http-char [c1 c2 & {:keys [default]
                          :or {default \space}}]
  (if-let [code (hex->int (str c1 c2))] 
    (char code)
    default))

(defn decode-param [[c1 c2 c3 & cs]]
  (when c1
    (case c1
      \% (str (http-char c2 c3) (decode-param cs))
      \+ (str \space (decode-param (apply str c2 c3 cs)))
      (str c1 (decode-param (apply str c2 c3 cs))))))

(defn parse-params [s]
  (->> s
       (#(str/split % #"&"))
       (map #(str/split % #"="))
       (into {})))

(defn parse-url [s]
  (let [url-and-params     #"([^\s/]+)\?(\S+)"
        url-without-params #"[\s/]+([^\s\?]+)\s+"]
    (if-let [[_ url params] (re-find url-and-params s)]
      [url (parse-params params)]
      (if-let [[_ url] (re-find url-without-params s)]
        [url]
        []))))

(defn get-headers [rdr]
  (when-let [line (.readLine rdr)]
    (when-let [[_ k v] (re-find #"(.*):\s*(.*)" line)]
      (merge {k v} (get-headers rdr)))))

(defn get-content-params [stream headers]
  (when-let [content-length (headers "Content-Length")]
    (let [n (Integer/parseInt content-length)
          chars (->> (repeatedly n #(.read stream))
                     (map #(when-not (= % -1) (char %))))]
     (parse-params (apply str chars))))) 

