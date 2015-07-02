(ns land-of-lisp.svg
  (:require [clojure.string :as str]))

(defn print-tag [tag-name attrs closing?]
  (print \<)
  (when closing? (print \/))
  (print (.toLowerCase (str tag-name)))
  (doseq [[k v] attrs]
    (printf " %s=%s" (.toLowerCase (str k)) (str \" v \")))
  (print \>))

(defmacro tag [name attrs & body]
  `(do 
     (print-tag '~name
                (list ~@(map (fn [[x# y#]] `(vector '~x# ~y#))
                             (partition-all 2 attrs)))
                false)
     ~@body
     (print-tag '~name [] true)))

(defmacro svg [& body]
  `(tag ~'svg [~'xmlns       "http://www.w3.org/2000/svg"
               ~'xmlns:xlink "http://www.w3.org/1999/xlink"
               ~'height      "1000"
               ~'width       "1000"]
        ~@body))

(defn brightness [color amount]
  (map #(-> (+ % amount) (max 0) (min 255))
       color))

(defn svg-style [color]
  (apply format "fill:rgb(%s,%s,%s);stroke:rgb(%s,%s,%s)"
                (concat color (brightness color -100))))

(defn circle [[cx cy] radius color]
  (tag circle [cx    cx
               cy    cy
               r     radius
               style (svg-style color)]))

(defn polygon [points color]
  (tag polygon [points (str/join \space
                                 (map (fn [[x y]] (format "%s,%s" x y)) 
                                      points))
                style  (svg-style color)]))

(defn random-walk [value length]
  (when-not (zero? length)
    (cons value
          (random-walk (if (zero? (rand-int 2))
                         (dec value)
                         (inc value))
                       (dec length)))))

(defmacro printing-to-file [filename & body]
  `(let [content# (with-out-str ~@body)]
     (spit ~filename content#)))

(defn random-graph []
  (printing-to-file "random_walk.svg"
    (svg (dotimes [_ 10] 
           (polygon (concat [[0 200]] 
                            (map vector (range) 
                                        (random-walk 100 400))
                            [[400 200]])
                    (repeatedly 3 (fn [] (rand-int 256))))))))
