(ns land-of-lisp.the-wizards-adventure)

;; places and their descriptions

(def descriptions
  {'living-room '(you are in the living room. a wizard is snoring loudly on the couch.)
   'garden      '(you are in a beautiful garden. there is a well in front of you.)
   'attic       '(you are in the attic. there is a giant welding torch in the corner.)})

(def edges
  {'living-room '[(garden west door) (attic upstairs ladder)]
   'garden      '[(living-room east door)]
   'attic       '[(living-room downstairs ladder)]})

(defn describe-path [edge]
  (let [path (last edge)
        direction (second edge)]
    (concat '(there is a) [path] '(going) [direction] '(from here.))))

(defn describe-paths [location edges]
  (apply concat (map describe-path (edges location))))

(describe-paths 'living-room edges)

;; objects and their locations

(def object-locations
  (atom {'whiskey 'living-room
         'bucket  'living-room
         'chain   'garden
         'frog    'garden}))

(defn objects-at [loc obj-locs]
  (for [[obj obj-loc] obj-locs
        :when (= obj-loc loc)]
    obj))

(defn describe-objects [loc obj-locs]
  (letfn [(describe-object [obj] (concat '(you see a) [obj] '(on the floor.)))]
    (apply concat (map describe-object (objects-at loc obj-locs)))))

;; looking around

(def location (atom 'living-room))

(defn look []
  (concat (descriptions @location)
          (describe-paths @location edges)
          (describe-objects @location @object-locations)))

;; walking around

(defn walk [direction]
  (if-let [edge (first (filter #(= (second %) direction) (edges @location)))]
    (do (reset! location (first edge)) (look))
    '(you cannot go that way.)))

;; picking up objects

(defn pick-up [object]
  (if ((set (objects-at @location @object-locations)) object)
    (do
      (swap! object-locations assoc object 'body)
      (concat '(you are now carrying the) [object]))
    '(you cannot get that.)))

;; checking inventory

(defn inventory []
  (cons 'items- (objects-at 'body @object-locations)))
