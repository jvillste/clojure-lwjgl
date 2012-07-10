(ns clojure-lwjgl.test.names
  (:require (clojure-lwjgl [vector-rectangle :as vector-rectangle]
                           [triangle-list :as triangle-list]
                           [image :as image]
                           [image-list :as image-list]
                           [texture :as texture]
                           [font :as font]
                           [buffered-image :as buffered-image]
                           [window :as window]
                           [input :as input]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]))

(defn render [application]

  (let [scale 1]
    (GL11/glClearColor 1 1 1 0)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (GL11/glLoadIdentity)
    (GL11/glScalef scale (- scale) 1)
    (GL11/glTranslatef 0 (- (* (/ 1 scale) @(:height (:window application)))) 0))


  (triangle-list/render (:triangle-list application))
  (image-list/draw (:image-list application))

  application)

(defn name-for-index [index ratings show-only-rated-names]
  (if show-only-rated-names
    (if (< index (count (seq ratings)))
      (str (first (nth (sort-by second (seq ratings))
                       index)))
      nil)
    (str index)))

(defn name-x [index name-width names-per-line]
  (* name-width
     (mod index names-per-line)))

(defn name-y [index name-height names-per-line]
  (* name-height
     (+ 1 (quot index names-per-line))))

(defn name-text-for-index [index start-index font names-per-line name-width name-height ratings show-only-rated-names]
  {:x (name-x (- index start-index) name-width names-per-line)
   :y (name-y (- index start-index) name-height names-per-line)
   :text (str (name-for-index index ratings show-only-rated-names) " " (get ratings (name-for-index index ratings show-only-rated-names)))
   :font font})

(defn name-width [font letters-in-name]
  (font/width font (apply str (repeat letters-in-name "W"))))

(defn name-height [font]
  (font/height font "W"))

(defn names-per-line [font letters-in-name page-width]
  (quot page-width
        (name-width font letters-in-name)))

(defn lines-per-page [page-height font]
  (quot page-height
        (name-height font)))

(defn names-per-page [font page-width page-height letters-in-name]
  (* (names-per-line font letters-in-name page-width)
     (lines-per-page page-width font)))

(defn names [start-index font letters-in-name width height ratings show-only-rated-names]
  (let [name-width (name-width font letters-in-name)]
    (->> (range start-index (+ start-index (names-per-page font width height letters-in-name)))
         (map #(name-text-for-index % start-index font (quot width name-width) name-width (name-height font) ratings show-only-rated-names)))))

(defn draw-text [graphics text]
  (doto graphics
    (.setColor Color/BLACK)
    (.setFont (font/graphics-font (:font text)))
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
    (.drawString (:text text) (:x text) (:y text))))


(defn draw-names [application]
  (image-list/clear-image (:image-list application) :names)
  (image-list/draw-on-image (:image-list application)
                            :names
                            (fn [graphics]
                              (doseq [text (names (:index application)
                                                  (:font application)
                                                  (:letters-in-name application)
                                                  (int @(:width (:window application)))
                                                  (int @(:height (:window application)))
                                                  (:name-ratings application)
                                                  (:show-only-rated-names application))]
                                (draw-text graphics text))))
  application)



(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn name-index-in-coordinates [x y page-width font letters-in-name]
  (+ (* (names-per-line font letters-in-name page-width)
        (quot y (name-height font)))
     (quot x (name-width font letters-in-name))))

(defn save-ratings [application]
  (spit "ratings.txt" (:name-ratings application))
  application)

(defn load-ratings [application]
  (assoc application :name-ratings (read-string (slurp "ratings.txt"))))

(defn change-name-rating [application name change]
  (->  application
       (update-in
        [:name-ratings name]
        (fn [value] (if value
                      (+ change value)
                      change)))
       (update-in [:name-ratings]
                  (fn [name-ratings]
                    (if (= 0 (name-ratings name))
                      (dissoc name-ratings name)
                      name-ratings)))
       (save-ratings)
       (draw-names)))

(defn highlight-name [application index]
  (assoc application [:triangle-list]
         (triangle-list/update-many  (:triangle-list application)
                                     0
                                     (let [width (name-width (:font application)
                                                             (:letters-in-name application))
                                           height (name-height (:font application))
                                           names-per-line (names-per-line (:font application)
                                                                          (:letters-in-name application)
                                                                          @(:width (:window application)))]
                                       (vector-rectangle/rectangle  (name-x index
                                                                            width
                                                                            names-per-line)
                                                                    (- (name-y  index
                                                                                height
                                                                                names-per-line)
                                                                       height)
                                                                    width
                                                                    height
                                                                    (map float [0.0 0.0 1.0]))))))

(defn handle-event [application event]
  (cond

   (= (:type event)
      :mouse-moved)
   (let [x (:mouse-x event)
         y (- (int @(:height (:window application)))
              (:mouse-y event))]
     (-> application
         (assoc :mouse-x x
                :mouse-y y)
         (highlight-name (name-index-in-coordinates x
                                                    y
                                                    (:page-width application)
                                                    (:font application)
                                                    (:letters-in-name application)))))




   (= (:type event)
      :left-mouse-button-up)
   (change-name-rating application
                       (name-for-index (name-index-in-coordinates (:mouse-x application)
                                                                  (:mouse-y application)
                                                                  (:page-width application)
                                                                  (:font application)
                                                                  (:letters-in-name application))
                                       (:name-ratings application)
                                       (:show-only-rated-names application))

                       1)

   (= (:type event)
      :right-mouse-button-up)
   (change-name-rating application
                       (name-for-index (name-index-in-coordinates (:mouse-x application)
                                                                  (:mouse-y application)
                                                                  (:page-width application)
                                                                  (:font application)
                                                                  (:letters-in-name application))
                                       (:name-ratings application)
                                       (:show-only-rated-names application)
                                       )
                       -1)

   (key-pressed event input/escape)
   (do (window/request-close (:window application))
       application)

   (key-pressed event input/space)
   (-> application
       (assoc :show-only-rated-names (not (:show-only-rated-names application)))
       (draw-names))

   (key-pressed event input/down)
   (-> application
       (assoc :index (min (+ 10
                             (:index application))
                          100))
       (draw-names))


   (key-pressed event input/up)
   (-> application
       (assoc :index (max (- (:index application)
                             10)
                          0))
       (draw-names))


   :default application))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application
      (-> (reduce handle-event application unread-events)
          render))))

(defn update [application]
  (-> application
      (handle-events)))


(defn create-application [window]
  (-> {:name-ratings {}
       :font (font/create "LiberationSans-Regular.ttf" 11)
       :letters-in-name 6
       :index 0
       :page-width (int @(:width window))
       :page-height (int @(:height window))
       :image-list (-> (image-list/create)
                       (image-list/add-image :names 0 0 (int @(:width window)) (int @(:height window))))
       :triangle-list (triangle-list/create 10)
       :window window}
      (load-ratings)
      (highlight-name 0)
      (draw-names)
      (render)))

(defn start []
  (window/start 700 500
                20
                create-application
                update
                (fn [_])))

(comment
  (start)
  )