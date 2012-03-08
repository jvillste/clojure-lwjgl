(ns clojure-lwjgl.applications.traditional
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [visual-list :as visual-list]
                           [visual :as visual]
                           [image-list :as image-list]
                           [input :as input]
                           [group :as group]
                           [text :as text]
                           [free-layout :as free-layout]
                           [layoutable :as layoutable]
                           [rectangle :as rectangle])
            [clojure.zip :as zip]
            [clojure.contrib.dataflow :as dataflow])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))



;; miten välitetään component-manageri funktioille joilla lisätään komponentteja?
;; thread local binding
;; currytään bufferi funktiolle ennen kuin funktio annetaan sille funktiolle joka sitä kutsuu
;; state monad


(defn box [margin parent child]
  [(assoc parent
     :width (+ (* 2 margin)
               (layoutable/preferred-width child))
     :height (+ (* 2 margin)
                (layoutable/preferred-height child)))
   (assoc child
     :x (+ margin
           (:x parent))
     :y (+ margin
           (:y parent)))])

(fact "box adds a margin"
  (box 10
       {:x 5 :y 10}
       (text/create "Foo"))
  => [{:height 33, :width 40, :y 10, :x 5}
      #clojure_lwjgl.text.Text{:content "Foo", :y 20, :x 15}])

(defn vertical-stack [x0 y0 visuals]
  (loop [visuals visuals
         layouted-visuals []
         y y0]
    (if (seq visuals)
      (let [visual (assoc (first visuals)
                     :y y
                     :x x0)]
        (recur (rest visuals)
               (conj layouted-visuals
                     visual)
               (+ y (layoutable/preferred-height visual))))
      layouted-visuals)))

(defrecord TestLayoutable [height]
  layoutable/Layoutable
  (layoutable/preferred-height [test-layoutable] (:height test-layoutable)))

(fact "vertical stack sets x and y coordinates"
  (vertical-stack 10
                  20
                  [(TestLayoutable. 10)
                   (TestLayoutable. 15)
                   (TestLayoutable. 10)])
  => [#clojure_lwjgl.applications.traditional.TestLayoutable{:height 10, :x 10, :y 20}
      #clojure_lwjgl.applications.traditional.TestLayoutable{:height 15, :x 10, :y 30}
      #clojure_lwjgl.applications.traditional.TestLayoutable{:height 10, :x 10, :y 45}])

(defn ids-to-visuals [gui ids]
  (reduce (fn [visuals visual-id] (conj visuals ((:visuals gui) visual-id)))
          []
          ids))

(defn update-visuals [gui visuals]
  (assoc gui
    :visuals (reduce (fn [visuals visual] (assoc visuals (:id visual) visual))
                     (:visuals gui)
                     visuals)))

(defn layout [gui]
  (let [labels (vertical-stack 5
                               5
                               (ids-to-visuals gui (:labels gui)))]
    (doseq [label labels]
      (image-list/move-image (:image-list gui)
                             (:id label)
                             (:x label)
                             (:y label)))

    (image-list/move-image (:image-list gui)
                           (:selection-rectangle gui)
                           5
                           (:y (nth labels (:selection gui))))

    (update-visuals gui labels)))

(defn generate-id [] (rand-int 100000000))

(defn render-visual [visual image-list]
  (visual/render visual (image-list/get-graphics image-list (:id visual))))

(defn add-visual-to-image-list [image-list visual x y]
  (let [image-list (image-list/add-image image-list
                                         (:id visual)
                                         x
                                         y
                                         (layoutable/preferred-width visual)
                                         (layoutable/preferred-height visual))]

    (render-visual visual image-list)

    image-list))

(defn create-visual [visual]
  (assoc visual
    :id (generate-id)))

(defn add-visual [gui visual x y]
  (assoc gui
    :image-list (add-visual-to-image-list (:image-list gui)
                                          visual
                                          x
                                          y)
    :visuals (assoc (:visuals gui) (:id visual) visual)))

(defn add-label [gui message]
  (let [label (create-visual (text/create message))]
    (-> gui
        (assoc :labels (conj (:labels gui)
                             (:id label)))
        (add-visual label
                    10
                    10)
        (layout))))


(defn create-gui [window]
  (let [selection-rectangle (create-visual (rectangle/create {:red 0 :green 1 :blue 1 :alpha 1}
                                                             100
                                                             15
                                                             10))
        gui {:window window
             :visuals {}
             :selection-rectangle (:id selection-rectangle)
             :labels []
             :image-list (image-list/create)
             :selection 0}]
    (-> gui
        (add-visual selection-rectangle 5 5)
        (add-label "Foo 1")
        (add-label "Foo 2")
        (add-label "Foo 3")
        (add-label "Foo 4")
        (add-label "Foo 5")
        (add-label "Foo 6")
        )))

(defn update-window [gui]
  (assoc gui :window (window/update (:window gui)
                                    30)))

(defn clear [gui]
  (let [scale 3]
    (GL11/glClearColor 1 1 1 0)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (GL11/glLoadIdentity)
    (GL11/glScalef scale (- scale) 1)
    (GL11/glTranslatef 0 (- (* (/ 1 scale) @(:height (:window gui)))) 0))

  gui)

(defn render [gui]
  (image-list/draw (:image-list gui))
  gui)

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn add-character [label character]
  (assoc label :content
         (str (:content label)
              character)))

(defn resize-visual [image-list visual]
  (image-list/resize-image image-list
                           (:id visual)
                           (layoutable/preferred-width visual)
                           (layoutable/preferred-height visual)))

(defn handle-event [gui keyboard-event]
  (cond
   (key-pressed keyboard-event input/down)
   (assoc gui
     :selection (min (+ 1
                        (:selection gui))
                     (- (count (:labels gui))
                        1)))
   (key-pressed keyboard-event input/up)
   (assoc gui
     :selection (max (- (:selection gui)
                        1)
                     0))
   (re-find #"\w" (str (:character keyboard-event)))
   (let [label-id (nth (:labels gui) (:selection gui))
         label ((:visuals gui) label-id)
         new-label (add-character label (:character keyboard-event))]
     (println "character " label new-label)
     (resize-visual (:image-list gui) new-label)
     (render-visual new-label (:image-list gui))
     (assoc gui
       :visuals (assoc (:visuals gui)
                  label-id
                  new-label)))

   :default
   gui))

(defn update-view [gui]
  (layout (reduce handle-event gui (input/unread-keyboard-events))))

(defn update [gui]
  (-> gui
      (clear)
      (update-view)
      (render)
      (update-window)))

(comment
(let [window (window/create 500 500)]
    (try
      (let [initial-gui (create-gui window)]
        (loop [gui initial-gui]
          (if (not @(:close-requested (:window gui)))
            (recur (update gui))
            (window/close window))))

      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close window)))))


