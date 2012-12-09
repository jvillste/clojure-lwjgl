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
            [clojure-lwjgl.test.dataflow :as dataflow]
            (clojure [string :as string]
                     [set]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))

(defn debug [& messages]
  #_(apply println messages))

(defrecord ViewPartCall [view-part-command-path])

(defprotocol Element
  (drawing-commands [element width height])
  (preferred-width [element])
  (preferred-height [element]))

(defprotocol MouseEventHandler
  (handle-mouse-event [mouse-event-handler application-state event]))

(defprotocol MouseEventHandlerContainer
  (child-mouse-event-handlers [mouse-event-handler-container requested-width requested-height]))

(extend ViewPartCall
  command/Command
  {:create-runner identity}
  command/CommandRunner
  {:delete identity
   :run identity}
  Element
  {:drawing-commands (fn [view-part-call width height] [view-part-call])
   :preferred-width (fn [view-part-call]  (dataflow/get-global-value (conj (:view-part-command-path view-part-call) :preferred-width)))
   :preferred-height (fn [view-part-call] (dataflow/get-global-value (conj (:view-part-command-path view-part-call) :preferred-height)))})

(defn draw-view-part [application-state view-part-command-path]
  #_(debug "drawing view part " view-part-command-path)
  (doseq [command-runner (get-in application-state [:view-part-command-runners view-part-command-path])]
    #_(debug "running command runner " (type command-runner))
    (if (instance? ViewPartCall command-runner)
      (draw-view-part application-state (:view-part-command-path command-runner))
      (command/run command-runner))))

(defn render [application-state]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (draw-view-part application-state [:commands])

  application-state)

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (when (not (empty? unread-events))
      (doseq [event unread-events]
        (swap! application (fn [application-state]
                             ((:event-handler application-state)
                              (assoc application-state
                                :event-handled false)
                              [:elements]
                                event)))))))

(defn view-part-is-defined? [application-state view-part-commands-path]
  (contains? (:view-part-command-paths application-state) view-part-commands-path))

(defn undefine-view-part [application-state view-part-commands-path]
  (debug "undefining view part " view-part-commands-path)
  (dorun (map command/delete (get-in application-state [:view-part-command-runners view-part-commands-path])))

  (update-in application-state [:view-part-command-paths] disj view-part-commands-path))

(defn define-view-part [application-state view-part-commands-path]

  (undefine-view-part application-state view-part-commands-path)

  (debug "defining view part " view-part-commands-path)
  (let [drawing-commands (get application-state view-part-commands-path [])
        application-state (reduce define-view-part
                                  application-state
                                  (->> (filter #(and (instance? ViewPartCall %)
                                                     (not (view-part-is-defined? application-state (:view-part-command-path %))))
                                               drawing-commands)
                                       (map :view-part-command-path)))]

    (-> application-state
        (assoc-in [:view-part-command-runners view-part-commands-path] (command/command-runners-for-commands drawing-commands))
        (update-in [:view-part-command-paths] conj view-part-commands-path))))

(defn update-view-part [application-state view-part-commands-path]
  #_(debug "updating view part " view-part-commands-path)
  (if (contains? application-state view-part-commands-path)
    (define-view-part application-state view-part-commands-path)
    (undefine-view-part application-state view-part-commands-path)))

(defn update-view [application]
  (when (not (empty? (dataflow/changes @application)))
    (let [application-state (swap! application #(-> %
                                                    (assoc :changes-to-be-processed (dataflow/changes %))
                                                    (dataflow/reset-changes)))

          changed-view-part-command-paths (filter #(view-part-is-defined? application-state %)
                                                  (:changes-to-be-processed application-state))]
      (render (swap! application
                     (fn [application-state]
                       (reduce update-view-part application-state changed-view-part-command-paths)))))))

(defn update [application]
  (handle-events application)
  #_(swap! application (fn [application-state]
                         (dataflow/define-to application-state :time (System/nanoTime))))
  (update-view application)
  application)

(defn initialize-application-state [window-width window-height root-element-constructor]
  (-> (dataflow/create)
      (dataflow/define-to
        :width  window-width
        :height  window-height
        :elements root-element-constructor
        :commands #(drawing-commands (dataflow/get-global-value :elements)
                                     (dataflow/get-global-value :width)
                                     (dataflow/get-global-value :height)))))

(defn create-application [window event-handler root-element-constructor]
  (let [application-state (-> (initialize-application-state @(:width window)
                                                            @(:height window)
                                                            root-element-constructor)
                              (assoc
                                  :window window
                                  :view-part-command-runners {}
                                  :view-part-command-paths #{}
                                  :event-handler event-handler)
                              (define-view-part [:commands]))
        application (atom application-state)]

    #_(swap! application (fn [application-state]
                           (assoc application-state :atom application)))
    application))

(defn close-application [application]
  (window/request-close (:window application))
  application)


;; VIEW PARTS

(defrecord ViewPart [mouse-event-handler local-id root-element-path]
  Element
  (drawing-commands [view-part width height]
    (dataflow/define local-id (fn [] (drawing-commands (dataflow/get-global-value root-element-path)
                                                       width
                                                       height)))
    [(->ViewPartCall (dataflow/absolute-path local-id))])
  (preferred-width [view-part] (preferred-width (dataflow/get-global-value root-element-path)))
  (preferred-height [view-part] (preferred-height (dataflow/get-global-value root-element-path)))

  MouseEventHandler
  (handle-mouse-event [view-part application-state event] (mouse-event-handler application-state event)))

(defn call-view-part
  ([local-id]
     (->ViewPart (fn [application-state event] application-state) local-id (dataflow/absolute-path local-id)))
  ([local-id mouse-event-handler]
     (->ViewPart mouse-event-handler local-id (dataflow/absolute-path local-id))))


;; ELEMENTS

(defrecord Text [contents font color]
  Element
  (drawing-commands [text width height] [(text/create 0 0
                                                      contents
                                                      font
                                                      color)])
  (preferred-width [text] (font/width font contents))
  (preferred-height [text] (font/height font)))

(defrecord Rectangle [width height color]
  Element
  (drawing-commands [rectangle requested-width requested-height]
    [(vector-rectangle/rectangle 0
                                 0
                                 requested-width
                                 requested-height
                                 color)])

  (preferred-width [rectangle] width)
  (preferred-height [rectangle] height)

  Object
  (toString [_] (str "(->Rectangle " width " " height " " color ")")))

(defrecord Box [margin outer inner]
  Element
  (drawing-commands [box requested-width requested-height]
    (concat (drawing-commands outer
                              requested-width
                              requested-height)
            (apply translate/translate margin margin (drawing-commands inner
                                                                       (preferred-width inner)
                                                                       (preferred-height inner)))))

  (preferred-width [box] (+ (* 2 margin)
                            (preferred-width inner)))

  (preferred-height [box] (+ (* 2 margin)
                             (preferred-height inner)))

  MouseEventHandlerContainer
  (child-mouse-event-handlers [box requested-width requested-height]
    [{:x margin
      :y margin
      :width (preferred-width inner)
      :height (preferred-height inner)
      :mouse-event-handler inner}

     {:x 0
      :y 0
      :width requested-width
      :height requested-height
      :mouse-event-handler outer}]))



(defn vertical-stack-width [elements]
  (apply max (conj (map preferred-width elements)
                   0)))

(defrecord VerticalStack [elements]
  Element
  (drawing-commands [vertical-stack
                     requested-width
                     requested-height] (let [width (vertical-stack-width elements)]
                                         (vec (concat [(push-modelview/->PushModelview)]
                                                      (reduce (fn [commands element]
                                                                (let [preferred-height (preferred-height element)]
                                                                  (concat commands
                                                                          (drawing-commands element
                                                                                            width
                                                                                            preferred-height)
                                                                          [(translate/->Translate 0 preferred-height)])))
                                                              []
                                                              elements)
                                                      [(pop-modelview/->PopModelview)]))))
  (preferred-height [vertical-stack] (reduce + (map preferred-height elements)))
  (preferred-width [vertical-stack] (apply max (conj (map preferred-width elements)
                                                     0)))

  MouseEventHandlerContainer
  (child-mouse-event-handlers [vertical-stack requested-width requested-height]
    (let [width (vertical-stack-width elements)]
      (loop [mouse-event-handlers []
             y 0
             elements elements]
        (if (seq elements)
          (let [preferred-height (preferred-height (first elements))]
            (recur (conj mouse-event-handlers {:x 0
                                               :y y
                                               :width width
                                               :height preferred-height
                                               :mouse-event-handler (first elements)})
                   (+ y preferred-height)
                   (rest elements)))
          mouse-event-handlers))))

  Object
  (toString [_] (str "(->VerticalStack " elements ")")))

(defrecord HorizontalStack [elements]
  Element
  (drawing-commands [horizontal-stack
                     requested-width
                     requested-height] (let [height (apply max (conj (map preferred-height elements)
                                                                     0))]
                                         (vec (concat [(push-modelview/->PushModelview)]
                                                      (reduce (fn [commands element]
                                                                (concat commands
                                                                        (drawing-commands element
                                                                                          (preferred-width element)
                                                                                          height)
                                                                        [(translate/->Translate (preferred-width element) 0 )]))
                                                              []
                                                              elements)
                                                      [(pop-modelview/->PopModelview)]))))
  (preferred-height [horizontal-stack] (apply max (conj (map preferred-height elements)
                                                        0)))
  (preferred-width [horizontal-stack] (reduce + (map preferred-width elements)))
  Object
  (toString [_] (str "(->VerticalStack " elements ")")))



#_(defn route-mouse-event [application-state mouse-event-handler-container event]
    (loop [application-state application-state
           mouse-event-handlers (mouse-event-handlers mouse-event-handler-container)]
      (if (and (seq mouse-event-handlers)
               (not (:event-handled application-state)))
        (if (satisfies? MouseEventHandler (first mouse-event-handlers))
          (recur (handle-mouse-event (first mouse-event-handlers) application-state event)
                 (rest mouse-event-handlers))
          (recur application-state
                 (rest mouse-event-handlers)))
        application-state)))

(defrecord Stack [elements]
  Element
  (drawing-commands [stack requested-width requested-height]
    (vec (mapcat (fn [element]
                   (drawing-commands element
                                     requested-width
                                     requested-height))
                 elements)))
  (preferred-width [stack]
    (apply max (conj (map preferred-width elements)
                     0)))
  (preferred-height [stack]
    (apply max (conj (map preferred-height elements)
                     0)))

  MouseEventHandlerContainer
  (child-mouse-event-handlers [stack requested-width requested-height]
    (map (fn [element] {:x 0
                        :y 0
                        :width requested-width
                        :height requested-height
                        :mouse-event-handler element})
         elements))

  Object
  (toString [_] (str "(->Stack " elements ")")))

(defrecord Translation [x y element]
  Element
  (drawing-commands [translation requested-width requested-height] (vec (apply translate/translate
                                                                               x
                                                                               y
                                                                               (drawing-commands element
                                                                                                 (preferred-width element)
                                                                                                 (preferred-height element)))))
  (preferred-width [translation] (+ x (preferred-width element)))

  (preferred-height [translation] (+ y (preferred-height element)))


  Object
  (toString [_] (str "(->Transaltion " x " " y " " element ")")))


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


(defn property [element-path key]
  (dataflow/get-global-value (concat element-path (dataflow/as-path key))))

(defn property-from [dataflow element-path key]
  (dataflow/get-global-value-from dataflow (concat element-path (dataflow/as-path key))))

(defn init-and-call [view-part-id view-part-element-function]
  (dataflow/initialize view-part-id view-part-element-function)
  (call-view-part view-part-id))

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
     :cursor #(cursor editor-path
                      font)
     :change-listener (fn [] change-listener))

    (let [text (if (dataflow/get-value :editing)
                 (dataflow/get-value :edited-value)
                 (dataflow/get-value :value))]

      (->Box 2
             (->Rectangle 0
                          0
                          (if (dataflow/get-value :selected)
                            [0 0 1 1]
                            [1 1 1 1]))
             (->Stack (concat (if (dataflow/get-value :editing)
                                [(call-view-part :cursor)]
                                [])
                              [(->Text text
                                       font
                                       [0 0 0 1])]))))))

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

    (->VerticalStack (vec (doall (map-indexed (fn [index item-id]
                                                (init-and-call (editor-id item-id) (fn [] (editor (property item-list-view-path [:items item-id])

                                                                                                  #(= item-id
                                                                                                      (property item-list-view-path :selected-item-id))

                                                                                                  (fn [application-state new-value]
                                                                                                    (dataflow/define-to
                                                                                                      application-state
                                                                                                      (concat item-list-view-path [:items item-id])
                                                                                                      new-value))))))

                                              (zipper-list/items (property item-list-view-path :item-order))))))))


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

(defn item-view []
  (->Stack [(init-and-call :background background)
            (init-and-call :item-list-view item-list-view)]))


(defn handle-item-view-event [application-state item-view event]
  (debug "handling event " event)
  (println (items application-state item-view))

  (let [application-state (handle-item-list-view-event application-state (concat item-view [:item-list-view]) event)]
    (if (not (:event-handled application-state))
      (cond
       (key-pressed event input/escape)
       (close-application application-state)

       :default application-state)

      application-state)))



(defn create-todo-list [window]
  (let [application (create-application window handle-item-view-event item-view)]
    (swap! application (fn [application-state]
                         (let [item-list-view [:elements :item-list-view]]
                           (-> application-state
                               (add-item item-list-view 0 "Foo")
                               (add-item item-list-view 0 "Bar")
                               #_(dataflow/print-dataflow)))))
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

(defn mouse-event-handlers [mouse-event-handler-container application-state width height]
  (doseq [mouse-event-handler-spec (child-mouse-event-handlers mouse-event-handler-container width height)]
    (let [mouse-event-handler (:mouse-event-handler mouse-event-handler-spec)]
      (when (satisfies? MouseEventHandler mouse-event-handler)
        (println "handler")
        (println mouse-event-handler-spec))
      (when (satisfies? MouseEventHandlerContainer mouse-event-handler)
        (mouse-event-handlers mouse-event-handler
                              application-state
                              (:width mouse-event-handler-spec)
                              (:height mouse-event-handler-spec)))
      (when (instance? ViewPart mouse-event-handler)
        (let [root-element (get application-state (:root-element-path mouse-event-handler))]
          (when (satisfies? MouseEventHandlerContainer root-element)
            (mouse-event-handlers root-element
                                  application-state
                                  (:width mouse-event-handler-spec)
                                  (:height mouse-event-handler-spec))))))))

(let [item-list-view [:elements :item-list-view]
      application-state (-> (initialize-application-state 300 300 item-view)
                            (add-item item-list-view 0 "Foo")
                            (add-item item-list-view 0 "Bar"))]
  (binding [dataflow/current-dataflow (atom application-state)]
    (mouse-event-handlers (get application-state [:elements])
                          application-state
                          200
                          200)))

(comment
  (start)

  (let [item-list-view [:elements :item-list-view]
        application-state (-> (initialize-application-state 300 300 item-view)
                              (add-item item-list-view 0 "Foo")
                              (add-item item-list-view 0 "Bar"))]
    (mouse-event-handlers (get application-state [:elements]))

    #_(-> application-state
          (handle-item-view-event [:elements]
                                  {:type :key-released, :key-code input/enter, :character nil})
          (dataflow/print-dataflow))))