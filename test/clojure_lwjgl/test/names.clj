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

(defrecord Grid [cell-width cell-height grid-width grid-height])

(defn rows [grid]
  (quot (:grid-height grid)
        (:cell-height grid)))

(defn columns [grid]
  (quot (:grid-width grid)
        (:cell-width grid)))

(defn cell-x [grid index]
  (* (:cell-width grid)
     (quot index (columns grid))))

(defn cell-y [grid index]
  (* (:cell-height grid)
     (+ 1 (mod index (rows grid)))))

(defn cells [grid]
  (* (rows grid)
     (columns grid)))

(defn cell-in-coordinates [grid x y]
  (+ (* (rows grid)
        (quot x (:cell-width grid)))
     (quot y (:cell-height grid))))

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

(defn letters-in-name [{:keys [show-only-rated-names name-ratings letters-in-name]}]
  (if show-only-rated-names
    (+ 3 (apply max (map count (keys name-ratings))))
    letters-in-name))

(defn name-for-index [index ratings show-only-rated-names name-generator]
  (if show-only-rated-names
    (if (< index (count (seq ratings)))
      (str (first (nth (sort-by #(str (second %) (first %)) (seq ratings))
                       index)))
      nil)
    (name-generator index)))

(defn name-x [index name-width lines-per-page]
  (* name-width
     (quot index lines-per-page)))

(defn name-y [index name-height lines-per-page]
  (* name-height
     (+ 1 (mod index lines-per-page))))

(defn name-text-for-index [index start-index font lines-per-page name-width name-height ratings show-only-rated-names name-generator]
  (let [name (name-for-index index ratings show-only-rated-names name-generator)
        rating (get ratings name)
        index-in-page (- index start-index)]
    {:x (name-x index-in-page name-width lines-per-page)
     :y (name-y index-in-page name-height lines-per-page)
     :text (str name " " rating )
     :font font
     :color (case rating
              1 [0.0 0.5 0.0 1.0]
              2 [0.5 0.0 0.0 1.0]
              3 [0.0 0.5 0.0 1.0]
              4 [0.0 0.8 0.0 1.0]
              5 [0.0 0.7 0.7 1.0]
              [0.0 0.0 0.0 1.0])}))

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
     (lines-per-page page-height font)))

(defn layout-grid [{:keys [font page-width page-height] :as application}]
  (assoc application :grid (->Grid (name-width font (letters-in-name application))
                                   (name-height font)
                                   page-width
                                   page-height)))

(defn names [start-index grid font letters-in-name width height ratings show-only-rated-names name-generator]
  (let [name-width (name-width font letters-in-name)
        start-index (if show-only-rated-names 0 start-index)]
    (->> (range start-index (+ start-index (names-per-page font width height letters-in-name)))
         (map #(name-text-for-index % start-index font (lines-per-page height font) name-width (name-height font) ratings show-only-rated-names name-generator)))))

(defn draw-text [graphics text]
  (let [[r g b a] (map float (:color text))]
    (doto graphics
      (.setColor (Color. r g b a))
      (.setFont (font/graphics-font (:font text)))
      (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HBGR )
      (.drawString (:text text) (:x text) (:y text)))))


(defn draw-page-number [application]
  (let [{:keys [index font page-width page-height letters-in-name maximum-index]} application
        names-per-page (names-per-page font
                                       page-width
                                       page-height
                                       letters-in-name)]
    (image-list/draw-on-image (:image-list application)
                              :names
                              (fn [graphics]
                                (draw-text graphics {:color (map float [0.0 0.0 0.0 1.0])
                                                     :font font
                                                     :x (int 5)
                                                     :y (int (- @(:height (:window application))
                                                                5))
                                                     :text (str (+ 1 (quot index names-per-page))
                                                                "/"
                                                                (+ 1 (quot maximum-index names-per-page))
                                                                )}))))

  application)

(defn draw-names [application]
  (image-list/clear-image (:image-list application) :names)
  (image-list/draw-on-image (:image-list application)
                            :names
                            (fn [graphics]
                              (doseq [text (names (:index application)
                                                  (:grid application)
                                                  (:font application)
                                                  (letters-in-name application)
                                                  (:page-width application)
                                                  (:page-height application)
                                                  (:name-ratings application)
                                                  (:show-only-rated-names application)
                                                  (:name-generator application))]
                                (draw-text graphics text))))
  (draw-page-number application)
  application)

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn name-index-in-coordinates [x y page-height font letters-in-name]
  (+ (* (lines-per-page page-height font)
        (quot x (name-width font letters-in-name)))
     (quot y (name-height font))))

(defn save-ratings [application]
  (spit "ratings.txt" (:name-ratings application))
  application)

(defn load-ratings [application]
  (if (.exists (java.io.File. "ratings.txt"))
    (assoc application :name-ratings (read-string (slurp "ratings.txt")))
    application))

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
                                                             (letters-in-name application))
                                           height (name-height (:font application))
                                           lines-per-page (lines-per-page (:page-height application)
                                                                          (:font application))]
                                       (vector-rectangle/rectangle  (name-x index
                                                                            width
                                                                            lines-per-page)
                                                                    (- (name-y  index
                                                                                height
                                                                                lines-per-page)
                                                                       height)
                                                                    width
                                                                    height
                                                                    (map float [0.8 0.8 0.8]))))))

(defn change-page [application delta]
  (-> application
      (assoc :index (max 0
                         (+ (* delta
                               (names-per-page  (:font application)
                                                (:page-width application)
                                                (:page-height application)
                                                (letters-in-name application)))
                            (:index application))))
      (draw-names)))

(defn name-in-mouse-coordinates [application]
  (name-for-index (+ (cell-in-coordinates (:grid application)
                                          (:mouse-x application)
                                          (:mouse-y application))
                     (:index application))
                  (:name-ratings application)
                  (:show-only-rated-names application)
                  (:name-generator application)))

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
         (highlight-name (cell-in-coordinates (:grid application)
                                              x
                                              y))))

   (= (:type event)
      :left-mouse-button-up)
   (change-name-rating application
                       (name-in-mouse-coordinates application)
                       1)

   (= (:type event)
      :right-mouse-button-up)
   (change-name-rating application
                       (name-in-mouse-coordinates application)
                       -1)

   (key-pressed event input/escape)
   (do (window/request-close (:window application))
       application)

   (key-pressed event input/space)
   (-> application
       (assoc :show-only-rated-names (not (:show-only-rated-names application)))
       (layout-grid)
       (draw-names))

   (key-pressed event input/page-down)
   (change-page application 10)

   (key-pressed event input/page-up)
   (change-page application -10)

   (key-pressed event input/down)
   (change-page application 1)

   (key-pressed event input/up)
   (change-page application -1)

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


(defn create-application [window name-generator maximum-index]
  (-> {:maximum-index maximum-index
       :name-generator name-generator
       :name-ratings {}
       :font (font/create "LiberationSans-Regular.ttf" 17)
       :letters-in-name (+ 3 (count (name-generator 0)))
       :index 0
       :page-width (int @(:width window))
       :page-height (- (int @(:height window)) 20)
       :image-list (-> (image-list/create)
                       (image-list/add-image :names 0 0 (int @(:width window)) (int @(:height window))))
       :triangle-list (triangle-list/create 10)
       :window window}
      (layout-grid)
      (load-ratings)
      (highlight-name 0)
      (draw-names)
      (render)))

(defn start [name-generator maximum-index]
  (window/start 1000 500
                20
                (fn [window] (create-application window name-generator maximum-index))
                update
                (fn [_])))

(comment
(start (fn [index] (str "name" index))
         300)
  )
