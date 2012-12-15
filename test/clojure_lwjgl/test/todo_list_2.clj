(ns clojure-lwjgl.test.todo-list-2
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input]
                           [logged-access :as logged-access]
                           [zipper-list :as zipper-list]
                           [layoutable :as layoutable])
            (clojure-lwjgl.command [text :as text]
                                   [image :as image]
                                   [command :as command]
                                   [translate :as translate]
                                   [scale :as scale]
                                   [push-modelview :as push-modelview]
                                   [pop-modelview :as pop-modelview])
            (clojure.java [shell :as shell]
                          [io :as io])
            (clojure.contrib [profile :as profile])
            [clojure-lwjgl.test.dataflow :as dataflow]
            (clojure [string :as string]
                     [set :as set]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))

;; DATAFLOW HELPERS

(defn property [element-path key]
  (dataflow/get-global-value (concat element-path (dataflow/as-path key))))

(defn property-from [dataflow element-path key]
  (dataflow/get-global-value-from dataflow (concat element-path (dataflow/as-path key))))

(defn debug [& messages]
  (apply println messages)
  (last messages))



(defprotocol Layout
  (layout [layout requested-width requested-height]))

(defprotocol Layoutable
  (preferred-width [element])
  (preferred-height [element]))

(defprotocol Drawable
  (drawing-commands [element width height]))

(defprotocol MouseEventHandler
  (handle-mouse-event [mouse-event-handler application-state event]))

(defprotocol MouseEventHandlerContainer
  (child-mouse-event-handlers [mouse-event-handler-container requested-width requested-height]))

(defrecord ViewPartCall [view-part-layout-path]
  command/Command
  (create-runner [view-part-call] view-part-call)
  command/CommandRunner
  (delete [view-part-call])
  (run [view-part-call]))

(defn element-path-to-layout-path [element-path]
  (concat [:layout] (rest element-path)))

(defrecord ViewPart [mouse-event-handler local-id root-element-path]
  Drawable
  (drawing-commands [view-part width height]
    [(->ViewPartCall (element-path-to-layout-path root-element-path))])

  Layoutable
  (preferred-width [view-part] (property root-element-path [:preferred-width]))
  (preferred-height [view-part] (property root-element-path [:preferred-height]))

  MouseEventHandler
  (handle-mouse-event [view-part application-state event] (mouse-event-handler application-state event))

  Object
  (toString [_] (str "(->ViewPart " root-element-path)))

(defn call-view-part
  ([local-id]
     (->ViewPart (fn [application-state event] application-state) local-id (dataflow/absolute-path local-id)))
  ([local-id mouse-event-handler]
     (->ViewPart mouse-event-handler local-id (dataflow/absolute-path local-id))))

(defn initialize-view-part [view-part-id view-part-element-function]
  (dataflow/initialize view-part-id view-part-element-function)
  (let [view-part-path (dataflow/absolute-path view-part-id)]
    (dataflow/initialize (concat (dataflow/as-path view-part-id) [:preferred-width]) #(preferred-width (dataflow/get-global-value view-part-path)))
    (dataflow/initialize (concat (dataflow/as-path view-part-id) [:preferred-height]) #(preferred-height (dataflow/get-global-value view-part-path)))))

(defn init-and-call [view-part-id view-part-element-function]
  (initialize-view-part view-part-id view-part-element-function)
  (call-view-part view-part-id))


;; RENDERING

(defn draw-view-part [application-state layout-path]
  (doseq [command-runner (get-in application-state [:view-part-command-runners layout-path])]
    (if (instance? ViewPartCall command-runner)
      (draw-view-part application-state (:view-part-layout-path command-runner))
      (command/run command-runner))))

(defn render [application-state]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (draw-view-part application-state [:layout])

  application-state)


;; EVENT HANDLING

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn invert-mouse-y [window-height mouse-event]
  (if (contains? mouse-event :mouse-y)
    (assoc mouse-event :mouse-y (- window-height
                                   (:mouse-y mouse-event)))
    mouse-event))

(defn update-mouse-position [application-state event]
  (if(= (:type event)
        :mouse-moved)
    (dataflow/define-to application-state
      :mouse-x (:mouse-x event)
      :mouse-y (:mouse-y event))
    application-state))

(defn call-handlers [application-state handler-key layoutables]
  (reduce (fn [application-state layoutable]
            (if-let [handler (get layoutable handler-key)]
              (handler application-state)
              application-state))
          application-state
          layoutables))

(defn in-coordinates [layoutable x y]
  (and (>= x
           (:x layoutable))
       (<= x
           (+ (:x layoutable) (:width layoutable)))
       (>= y
           (:y layoutable))
       (<= y
           (+ (:y layoutable) (:height layoutable)))))

(defn layoutables-in-coordinates
  ([application-state x y]
     (layoutables-in-coordinates application-state
                                 x
                                 y
                                 (get application-state [:layout])))

  ([application-state x y layoutable]
     (if (satisfies? Layout layoutable)
       (loop [result [layoutable]
              x x
              y y
              layoutables (:children layoutable)]

         (if (seq layoutables)
           (let [layoutable (first layoutables)]
             (if (in-coordinates layoutable x y)
               (recur (concat (conj result
                                    layoutable)
                              (if (satisfies? Layout layoutable)
                                (layoutables-in-coordinates application-state
                                                            (+ x (:x layoutable))
                                                            (+ y (:y layoutable))
                                                            layoutable)
                                (if (instance? ViewPart layoutable)
                                  (layoutables-in-coordinates application-state
                                                              (+ x (:x layoutable))
                                                              (+ y (:y layoutable))
                                                              (get application-state (element-path-to-layout-path (:root-element-path layoutable))))
                                  [])))
                      x
                      y
                      (rest layoutables))
               (recur result
                      x
                      y
                      (rest layoutables))))
           result))
       [layoutable])))

(defn update-layoutables-under-mouse [application-state]
  (let [current-layoutables-under-mouse (set (layoutables-in-coordinates application-state
                                                                         (get application-state [:mouse-x])
                                                                         (get application-state [:mouse-y])))
        mouse-left (clojure.set/difference (:layoutables-under-mouse application-state)
                                           current-layoutables-under-mouse)
        mouse-entered (clojure.set/difference current-layoutables-under-mouse
                                              (:layoutables-under-mouse application-state))]

    (-> application-state
        (call-handlers :mouse-entered-handler mouse-entered)
        (call-handlers :mouse-left-handler mouse-left)
        (assoc :layoutables-under-mouse current-layoutables-under-mouse))))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (map (partial invert-mouse-y (get @application [:height]))
                                   (input/unread-mouse-events)))]
    (when (not (empty? unread-events))
      (doseq [event unread-events]
        (swap! application (fn [application-state]
                             (let [application-state (-> application-state
                                                         (assoc :event-handled false)
                                                         (update-mouse-position event)
                                                         (update-layoutables-under-mouse))]
                               ((:event-handler application-state)
                                application-state
                                [:elements]
                                  event))))))))



;; VIEW PARTS

(defn view-part-is-defined? [application-state view-part-commands-path]
  (contains? (:view-part-layout-paths application-state) view-part-commands-path))

(defn unload-view-part [application-state layout-path]
  (dorun (map command/delete (get-in application-state [:view-part-command-runners layout-path])))

  (update-in application-state [:view-part-layout-paths] disj layout-path))

(declare layoutable-drawing-commands)

(defn layout-drawing-commands [layout]
  (vec (concat [(push-modelview/->PushModelview)]
               (loop [commands []
                      x 0
                      y 0
                      layoutables (:children layout)]
                 (if (seq layoutables)
                   (let [layoutable (first layoutables)]
                     (recur (concat commands
                                    (concat (if (or (not (= (:x layoutable) x))
                                                    (not (= (:y layoutable) y)))
                                              [(translate/->Translate (- (:x layoutable)
                                                                         x)
                                                                      (- (:y layoutable)
                                                                         y))]
                                              [])
                                            (layoutable-drawing-commands layoutable)))
                            (:x layoutable)
                            (:y layoutable)
                            (rest layoutables)))
                   commands))
               [(pop-modelview/->PopModelview)])))

(defn layoutable-drawing-commands [layoutable]
  (if (satisfies? Layout layoutable)
    (layout-drawing-commands layoutable)
    (if (satisfies? Drawable layoutable)
      (drawing-commands layoutable
                        (:width layoutable)
                        (:height layoutable))
      [])))

(defn load-view-part [application-state layout-path]
  (unload-view-part application-state layout-path)

  (let [drawing-commands (layoutable-drawing-commands (get application-state layout-path))
        application-state (reduce load-view-part
                                  application-state
                                  (->> (filter #(and (instance? ViewPartCall %)
                                                     (not (view-part-is-defined? application-state (:view-part-layout-path %))))
                                               drawing-commands)
                                       (map :view-part-layout-path)))]

    (-> application-state
        (assoc-in [:view-part-command-runners layout-path] (command/command-runners-for-commands drawing-commands))
        (update-in [:view-part-layout-paths] conj layout-path))))

(defn update-view-part [application-state layout-path]
  (if (dataflow/is-defined? application-state layout-path)
    (load-view-part application-state layout-path)
    (unload-view-part application-state layout-path)))

(defn update-view [application]
  (when (not (empty? (dataflow/changes @application)))
    (let [application-state (swap! application #(-> %
                                                    (assoc :changes-to-be-processed (dataflow/changes %))
                                                    (dataflow/reset-changes)))

          changed-view-part-layout-paths (filter #(view-part-is-defined? application-state %)
                                                 (:changes-to-be-processed application-state))]
      (render (swap! application
                     (fn [application-state]
                       (reduce update-view-part application-state changed-view-part-layout-paths)))))))



(defn add-fps [application-state time-now]
  (update-in application-state [:fpss]
             (fn [fpss]
               (let [fpss (conj fpss (/ 1
                                        (/ (- time-now
                                              (:last-update-time application-state))
                                           1E9)))]
                 (if (> (count fpss)
                        20)
                   (vec (rest fpss))
                   fpss)))))

(defn average-fps [application-state]
  (let [fpss (:fpss application-state)]
    (/ (apply + fpss)
       (max 1 (count fpss)))))

(defn update-fps [application]
  (swap! application (fn [application-state]
                       (let [time-now (System/nanoTime)]
                         (-> application-state
                             (add-fps time-now)
                             (dataflow/define-to :fps (average-fps application-state))
                             (assoc :last-update-time time-now))))))

(defn update-time [application]
  (swap! application dataflow/define-to :time (System/nanoTime)))

(defn update [application]
  (handle-events application)
  ;;(update-time application)
  (update-fps application)
  (update-view application)

  application)


;; LAYOUT

(declare create-view-part-layout)

(defn create-layout [parent-layoutable]
  (if (satisfies? Layout parent-layoutable)
    (assoc parent-layoutable
      :children (vec (map (fn [child-layoutable]
                            (if (instance? ViewPart child-layoutable)
                              (do (dataflow/define (:local-id child-layoutable)
                                    #(create-view-part-layout (dataflow/get-global-value (:root-element-path child-layoutable))
                                                              (:width child-layoutable)
                                                              (:height child-layoutable)))
                                  child-layoutable)
                              (create-layout child-layoutable)))

                          (layout parent-layoutable
                                  (:width parent-layoutable)
                                  (:height parent-layoutable)))))
    parent-layoutable))

(defn create-view-part-layout [root-layoutable width height]
  (create-layout (assoc root-layoutable
                   :x 0
                   :y 0
                   :width width
                   :height height)))




;; INITIALIZATION

(defn initialize-application-state [window-width window-height root-element-constructor]
  (-> (dataflow/create)
      (dataflow/define-to
        :width  window-width
        :height  window-height
        :elements root-element-constructor
        :layout #(create-view-part-layout (dataflow/get-global-value :elements)
                                          (dataflow/get-global-value :width)
                                          (dataflow/get-global-value :height)))))

(defn create-application [window event-handler root-element-constructor]
  (let [application-state (-> (initialize-application-state @(:width window)
                                                            @(:height window)
                                                            root-element-constructor)
                              (assoc
                                  :fpss []
                                  :last-update-time (System/nanoTime)
                                  :layoutables-under-mouse #{}
                                  :window window
                                  :view-part-command-runners {}
                                  :view-part-layout-paths #{}
                                  :event-handler event-handler)
                              (load-view-part [:layout]))
        application (atom application-state)]

    #_(swap! application (fn [application-state]
                           (assoc application-state :atom application)))
    application))

(defn close-application [application]
  (window/request-close (:window application))
  application)


;; DRAWABLES

(defrecord Text [contents font color]
  Drawable
  (drawing-commands [text width height] [(text/create 0 0
                                                      contents
                                                      font
                                                      color)])

  Layoutable
  (preferred-width [text] (font/width font contents))
  (preferred-height [text] (font/height font)))

(defrecord Rectangle [width height color]
  Drawable
  (drawing-commands [rectangle requested-width requested-height]
    [(vector-rectangle/rectangle 0
                                 0
                                 requested-width
                                 requested-height
                                 color)])
  Layoutable
  (preferred-width [rectangle] width)
  (preferred-height [rectangle] height)

  Object
  (toString [_] (str "(->Rectangle " width " " height " " color ")")))


;; LAYOUTS

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
  (toString [_] (str "(->Box " margin " " outer " " inner)))

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
  (toString [_] (str "(->VerticalStack " layoutables)))

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
  (toString [_] (str "(->Stack " (vec layoutables) ")")))

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
  (toString [_] (str "(->Transaltion " translate-x " " translate-y " " layoutable ")")))

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
  (toString [_] (str "(->DockBottom " layoutable)))


;; TODO-LIST

(defn add-item [application-state item-list index value]
  (let [new-id (rand-int 10000)]
    (-> application-state
        (dataflow/define-to (concat item-list [:items new-id]) value)
        (dataflow/define-to (concat item-list [:item-order]) (zipper-list/insert (get application-state (concat item-list [:item-order]))
                                                                                 new-id
                                                                                 index)))))

(defn remove-item [application-state item-list index]
  (let [id (get (apply vector (zipper-list/items (get application-state (concat item-list [:item-order]))))
                (get application-state (concat item-list [:selection])))]
    (-> application-state
        (dataflow/undefine (concat item-list [:items id]))
        (dataflow/apply-to-value  (concat item-list [:item-order])  #(zipper-list/remove % id)))))

(defn items [application-state item-view]
  (map (fn [item-id] (property-from application-state item-view [:item-list-view :items item-id]))
       (zipper-list/items (property-from application-state item-view [:item-list-view :item-order]))))

(defn handle-editing-event [application-state editor event]
  (cond
   (key-pressed event input/escape)
   (-> application-state
       (dataflow/define-to (concat editor [:cursor-position]) 0)
       (dataflow/define-to (concat editor [:editing]) false)
       (dataflow/define-to (concat editor [:edited-value]) (get application-state (concat editor [:value])))
       (assoc :event-handled true))

   (key-pressed event input/left)
   (dataflow/apply-to-value application-state (concat editor [:cursor-position]) dec)

   (key-pressed event input/right)
   (dataflow/apply-to-value application-state (concat editor [:cursor-position]) inc)

   (not (nil? (:character event)))
   (-> application-state
       (dataflow/apply-to-value (concat editor [:edited-value]) (fn [edited-value]
                                                                  (-> (StringBuffer. edited-value)
                                                                      (.insert (get application-state (concat editor [:cursor-position])) (:character event))
                                                                      (.toString))))

       (dataflow/apply-to-value  (concat editor [:cursor-position]) inc)
       (assoc :event-handled true))



   :default application-state))

(defn handle-editor-event [application-state editor event]
  (cond

   (key-pressed event input/enter)
   (if (property-from application-state editor :editing)
     ((property-from application-state editor :change-listener)
      (dataflow/define-to application-state
        (concat editor [:value]) (property-from application-state editor :edited-value)
        (concat editor [:cursor-position]) 0
        (concat editor [:editing]) false)
      (property-from application-state editor :edited-value))
     (dataflow/define-to application-state (concat editor [:editing]) true))

   :default (if (get application-state (concat editor [:editing]))
              (handle-editing-event application-state editor event)
              application-state)))


(defn cursor [editor font]
  (let [text (property editor :edited-value)
        cursor-position (property editor :cursor-position)
        width (font/width font (subs text
                                     cursor-position
                                     (+ cursor-position 1)))
        height (font/height font)]
    (->Translation (font/width font (subs text 0 cursor-position))
                   0
                   (->Rectangle width
                                height
                                [1 #_(float (/ (mod (dataflow/get-global-value :time)
                                                    1000000000)
                                               1000000000))
                                 0 0 1]))))


(defn editor [value selected change-listener]
  (let [font (font/create "LiberationSans-Regular.ttf" 15)
        editor-path (dataflow/absolute-path [])]
    (dataflow/initialize
     :selected selected
     :value value
     :edited-value value
     :editing false
     :cursor-position 0
     :change-listener (fn [] change-listener)
     :mouse-over false)

    (initialize-view-part :cursor #(cursor editor-path
                                           font))

    (let [text (if (dataflow/get-value :editing)
                 (dataflow/get-value :edited-value)
                 (dataflow/get-value :value))]

      (assoc (->Box 2
                    (->Rectangle 0
                                 0
                                 (if (dataflow/get-value :mouse-over)
                                   [0.8 0.8 0.8 1]
                                   (if (dataflow/get-value :selected)
                                     [0 0 1 1]
                                     [1 1 1 1])))
                    (->Stack (concat (if (dataflow/get-value :editing)
                                       [(call-view-part :cursor)]
                                       [])
                                     [(->Text text
                                              font
                                              [0 0 0 1])])))

        :mouse-entered-handler (fn [application-state]
                                 (println "mouse entered" (property-from application-state editor-path :value))
                                 (dataflow/define-to application-state (concat editor-path [:mouse-over]) true))

        :mouse-left-handler (fn [application-state]
                              (dataflow/define-to application-state (concat editor-path [:mouse-over]) false))))))

(defn editor-id [item-id]
  [:editor item-id])


(defn item-list-view []
  (let [item-list-view-path (dataflow/absolute-path [])]
    (dataflow/initialize
     :selection 0
     :item-order (zipper-list/create)
     :selected-item-id #(nth (zipper-list/items (property item-list-view-path :item-order))
                             (property item-list-view-path :selection)
                             nil))

    (->VerticalStack (vec (map-indexed (fn [index item-id]
                                         (-> (init-and-call (editor-id item-id) (fn [] (editor (property item-list-view-path [:items item-id])

                                                                                               #(= item-id
                                                                                                   (property item-list-view-path :selected-item-id))

                                                                                               (fn [application-state new-value]
                                                                                                 (dataflow/define-to
                                                                                                   application-state
                                                                                                   (concat item-list-view-path [:items item-id])
                                                                                                   new-value)))))
                                             (assoc :mouse-event-handler (fn [application-state event]
                                                                           (if (= (:type event)
                                                                                  :left-mouse-button-up)
                                                                             (dataflow/define-to application-state (concat item-list-view-path [:selection]) index)
                                                                             application-state)))))

                                       (zipper-list/items (property item-list-view-path :item-order)))))))


(defn handle-item-list-view-event [application-state item-list-view event]
  (cond

   (key-pressed event input/down)
   (dataflow/apply-to-value application-state (concat item-list-view [:selection]) inc)

   (key-pressed event input/up)
   (dataflow/apply-to-value application-state (concat item-list-view [:selection]) dec)

   (key-pressed event input/backspace)
   (remove-item application-state item-list-view  (get application-state (concat item-list-view [:selection])))

   :default (let [application-state (handle-editor-event application-state
                                                         (concat item-list-view (editor-id (property-from application-state item-list-view :selected-item-id)))
                                                         event)]
              (if (not (:event-handled application-state))
                (cond
                 (key-pressed event input/space)
                 (add-item application-state item-list-view  (get application-state  (concat item-list-view [:selection])) "New item" )

                 :default application-state)

                application-state))))


(defn background []
  (->Rectangle 0 0 [1 1 1 1]))

(defn status []
  (->VerticalStack
   (concat [(init-and-call :fps fps)]
           (->> (dataflow/get-global-value [:status])
                (filter (fn [message] (not (= message nil))))
                (map str)
                (map (fn [message]
                       (->Text (str message)
                               (font/create "LiberationSans-Regular.ttf" 12)
                               [0 0 0 1])))))))

(defn fps []
  (->Text (str (float (dataflow/get-global-value :fps)))
          (font/create "LiberationSans-Regular.ttf" 12)
          [0 0 0 1]))

(defn item-view []
  (->Stack [(init-and-call :background background)
            (init-and-call :item-list-view item-list-view)
            (->DockBottom (init-and-call :status status))]))

(defn handle-item-view-event [application-state item-view event]
  (let [application-state (if (= (:type event)
                                 :mouse-moved)
                            (dataflow/define-to application-state [:status] (concat [(str "x: " (:mouse-x event) "y: " (:mouse-y event))]
                                                                                    (map :root-element-path (get application-state :layoutables-under-mouse))))

                            application-state)]

    (let [application-state (handle-item-list-view-event application-state (concat item-view [:item-list-view]) event)]
      (if (not (:event-handled application-state))
        (cond
         (key-pressed event input/escape)
         (close-application application-state)

         :default application-state)

        application-state))))

(defn create-todo-list [window]
  (let [application (create-application window handle-item-view-event item-view)]
    (swap! application (fn [application-state]
                         (let [item-list-view [:elements :item-list-view]]
                           (-> application-state
                               (dataflow/define-to [:status] [])
                               (add-item item-list-view 0 "Foo")
                               (add-item item-list-view 0 "Bar")
                               (dataflow/print-dataflow)))))
    application))

(defn start []
  (window/start 700 500
                60
                create-todo-list
                update
                identity
                (fn [application width height]
                  (println "resize callback")
                  (swap! application
                         #(dataflow/define-to
                            %
                            :width width
                            :height height))
                  application)))

(comment
  (start)
  )