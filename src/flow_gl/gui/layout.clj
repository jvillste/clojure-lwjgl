(ns flow-gl.gui.layout
  (:require  (flow-gl.graphics.command [translate :as translate]
                                       [scale :as scale]
                                       [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview])
             (flow-gl.gui [layoutable :as layoutable]
                          [drawable :as drawable])))

(defprotocol Layout
  (layout [layout requested-width requested-height])
  (children [layout]))

(extend Object
  Layout {:layout (fn [layout requested-width requested-height] layout)
          :children (fn [layout] [])})


;; UTILITIES

(defn set-dimensions-and-layout [layout-instance x y width height]
  (-> layout-instance
      (assoc :x x
             :y y
             :width width
             :height height)
      (layout width
              height)))

(defn layout-drawing-commands [layout]
  (vec (concat [(push-modelview/->PushModelview)]
               (loop [commands []
                      x 0
                      y 0
                      drawables (children layout)]
                 (if (seq drawables)
                   (let [drawable (first drawables)]
                     (recur (concat commands
                                    (concat (if (or (not (= (:x drawable) x))
                                                    (not (= (:y drawable) y)))
                                              [(translate/->Translate (- (:x drawable)
                                                                         x)
                                                                      (- (:y drawable)
                                                                         y))]
                                              [])
                                            (drawable/drawing-commands drawable)))
                            (:x drawable)
                            (:y drawable)
                            (rest drawables)))
                   commands))
               [(pop-modelview/->PopModelview)])))

;; LAYOUTS

(defrecord Box [margin outer inner]
  Layout
  (layout [box requested-width requested-height]
    (-> box
        (update-in [:outer] set-dimensions-and-layout 0 0 requested-width requested-height)
        (update-in [:inner] set-dimensions-and-layout margin margin (layoutable/preferred-width inner) (layoutable/preferred-height inner))))

  (children [box] [outer inner])

  layoutable/Layoutable
  (layoutable/preferred-width [box] (+ (* 2 margin)
                                       (layoutable/preferred-width inner)))

  (layoutable/preferred-height [box] (+ (* 2 margin)
                                        (layoutable/preferred-height inner)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Box" :margin :outer :inner)))


(defrecord Absolute [layoutables]
  Layout
  (layout [absolute requested-width requested-height]
    (assoc absolute :layoutables
           (vec (map #(set-dimensions-and-layout % (:x %) (:y %) (layoutable/preferred-width %) (layoutable/preferred-height %))
                     layoutables))))

  (children [box] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-width [absolute] (apply max (map (fn [layoutable]
                                                           (+ (:x layoutable)
                                                              (layoutable/preferred-width layoutable))))))

  (layoutable/preferred-height [box] (apply max (map (fn [layoutable]
                                                       (+ (:y layoutable)
                                                          (layoutable/preferred-height layoutable))))))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Absolute" :layoutables)))

(defrecord VerticalStack [layoutables]
  Layout
  (layout [vertical-stack requested-width requested-height]
    (assoc vertical-stack :layoutables
           (let [width (apply max (conj (map layoutable/preferred-width layoutables)
                                        0))]
             (loop [layouted-layoutables []
                    y 0
                    layoutables layoutables]
               (if (seq layoutables)
                 (let [height (layoutable/preferred-height (first layoutables))]
                   (recur (conj layouted-layoutables (set-dimensions-and-layout (first layoutables) 0 y width height))
                          (+ y height)
                          (rest layoutables)))
                 layouted-layoutables)))))

  (children [vertical-stack] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-height [vertical-stack] (reduce + (map layoutable/preferred-height layoutables)))

  (layoutable/preferred-width [vertical-stack] (apply max (conj (map layoutable/preferred-width layoutables)
                                                                0)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "VerticalStack" :layoutables)))

(defrecord Stack [layoutables]
  Layout
  (layout [stack requested-width requested-height]
    (assoc stack :layoutables
           (vec (map #(set-dimensions-and-layout % 0 0 requested-width requested-height)
                     layoutables))))

  (children [stack] layoutables)

  layoutable/Layoutable
  (layoutable/preferred-width [stack]
    (apply max (conj (map layoutable/preferred-width layoutables)
                     0)))

  (layoutable/preferred-height [stack]
    (apply max (conj (map layoutable/preferred-height layoutables)
                     0)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Stack" :layoutables)))


(defrecord Translation [translate-x translate-y layoutable]
  Layout
  (layout [translation requested-width requested-height]
    (assoc translation :layoutable
           (set-dimensions-and-layout layoutable
                                      translate-x
                                      translate-y
                                      (layoutable/preferred-width layoutable)
                                      (layoutable/preferred-height layoutable))))

  (children [translation] [layoutable])

  layoutable/Layoutable
  (layoutable/preferred-width [translation] (+ translate-x (layoutable/preferred-width layoutable)))

  (layoutable/preferred-height [translation] (+ translate-y (layoutable/preferred-height layoutable)))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "Translation" :translate-x :translate-y :layoutable)))

(defrecord DockBottom [layoutable]
  Layout
  (layout [dock-bottom requested-width requested-height]
    (let [height (layoutable/preferred-height layoutable)]
      (assoc dock-bottom :layoutable
             (set-dimensions-and-layout layoutable
                                        0
                                        (- requested-height
                                           height)

                                        (layoutable/preferred-width layoutable)
                                        height))))
  (children [dock-bottom] [layoutable])

  layoutable/Layoutable
  (layoutable/preferred-width [dock-bottom] (layoutable/preferred-width layoutable))

  (layoutable/preferred-height [dock-bottom] (layoutable/preferred-height layoutable))

  drawable/Drawable
  (drawing-commands [this] (layout-drawing-commands this))

  Object
  (toString [this] (layoutable/describe-layoutable this "DockBottom" :layoutable)))
