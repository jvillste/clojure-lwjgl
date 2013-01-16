(ns flow-gl.gui.layout
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])))

(defprotocol Layout
  (layout [layout requested-width requested-height])
  (children [layout]))

(defprotocol Layoutable
  (preferred-width [element])
  (preferred-height [element]))




;; LAYOUTS

(defn describe-property [layoutable property]
  (str (name property) ": " (property layoutable)))

(defn describe-properties [layoutable properties]
  (apply str (interpose " " (map (partial describe-property layoutable)
                                 properties))))

(defn describe-layoutable [layoutable name & properties]
  (str "(" name " " (describe-properties layoutable (concat [:x :y :width :height] properties)) ")"))

(defrecord Box [margin outer inner]
  Layout
  (layout [box requested-width requested-height]
    [(assoc outer
       :x 0
       :y 0
       :width requested-width
       :height requested-height)
     (assoc inner
       :x margin
       :y margin
       :width (preferred-width inner)
       :height (preferred-height inner))])

  Layoutable
  (preferred-width [box] (+ (* 2 margin)
                            (preferred-width inner)))

  (preferred-height [box] (+ (* 2 margin)
                             (preferred-height inner)))

  Object
  (toString [this] (describe-layoutable this "Box" :margin :children)))

(defrecord VerticalStack [layoutables]
  Layout
  (layout [vertical-stack requested-width requested-height]
    (let [width (apply max (conj (map preferred-width layoutables)
                                 0))]
      (loop [layouted-layoutables []
             y 0
             layoutables layoutables]
        (if (seq layoutables)
          (let [height (preferred-height (first layoutables))]
            (recur (conj layouted-layoutables (assoc (first layoutables)
                                                :x 0
                                                :y y
                                                :width width
                                                :height height))
                   (+ y height)
                   (rest layoutables)))
          layouted-layoutables))))

  Layoutable
  (preferred-height [vertical-stack] (reduce + (map preferred-height layoutables)))

  (preferred-width [vertical-stack] (apply max (conj (map preferred-width layoutables)
                                                     0)))


  Object
  (toString [this] (describe-layoutable this "VerticalStack" :layoutables)))

(defrecord Stack [layoutables]
  Layout
  (layout [stack requested-width requested-height]
    (map (fn [layoutable]
           (assoc layoutable
             :x 0
             :y 0
             :width requested-width
             :height requested-height))
         layoutables))

  Layoutable
  (preferred-width [stack]
    (apply max (conj (map preferred-width layoutables)
                     0)))

  (preferred-height [stack]
    (apply max (conj (map preferred-height layoutables)
                     0)))

  Object
  (toString [this] (describe-layoutable this "Stack" :layoutables)))


(defrecord Translation [translate-x translate-y layoutable]
  Layout
  (layout [translation requested-width requested-height]
    [(assoc layoutable
       :x translate-x
       :y translate-y
       :width (preferred-width layoutable)
       :height (preferred-height layoutable))])

  Layoutable
  (preferred-width [translation] (+ translate-x (preferred-width layoutable)))

  (preferred-height [translation] (+ translate-y (preferred-height layoutable)))

  Object
  (toString [this] (describe-layoutable this "Translation" :translate-x :translate-y :layoutable)))

(defrecord DockBottom [layoutable]
  Layout
  (layout [dock-bottom requested-width requested-height]
    (let [height (preferred-height layoutable)]
      [(assoc layoutable
         :x 0
         :y (- requested-height
               height)
         :width (preferred-width layoutable)
         :height height)]))

  Layoutable
  (preferred-width [dock-bottom] (preferred-width layoutable))

  (preferred-height [dock-bottom] (preferred-height layoutable))

  Object
  (toString [this] (describe-layoutable this "DockBottom" :layoutable)))
