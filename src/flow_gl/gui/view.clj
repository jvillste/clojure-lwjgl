(ns flow-gl.gui.view
  (:require  (flow-gl.graphics [command :as command])
             (flow-gl.graphics.command [push-modelview :as push-modelview]
                                       [pop-modelview :as pop-modelview]
                                       [translate :as translate])
             (flow-gl.gui [layout :as layout]
                          [layoutable :as layoutable]
                          [drawable :as drawable]
                          [input :as input])
             (flow-gl [opengl :as opengl]
                      [dataflow :as dataflow]
                      [debug :as debug])
             [clojure.data.priority-map :as priority-map])

  (:use clojure.test
        flow-gl.threading))

;; DEBUG

(defn describe-gpu-state [gpu-state]
  (for [key (keys (:view-part-command-runners gpu-state))]
    (str key " = " (vec (get-in gpu-state [:view-part-command-runners key])))))


;; VIEW PARTS

(defrecord ViewPartCall [view-part-layout-path layer]
  command/Command
  (create-runner [view-part-call] view-part-call)
  command/CommandRunner
  (delete [view-part-call])
  (run [view-part-call]))

(defn element-path-to-layout-path [element-path]
  (vec (concat [:layout] (rest element-path))))

(defrecord ViewPart [local-id root-element-path]
  drawable/Drawable
  (drawing-commands [view-part]
    [(->ViewPartCall (element-path-to-layout-path root-element-path) 0)])

  layoutable/Layoutable
  (preferred-width [view-part] (dataflow/property root-element-path [:preferred-width]))
  (preferred-height [view-part] (dataflow/property root-element-path [:preferred-height]))

  layout/Layout
  (layout [view-part requested-width requested-height]
    (dataflow/initialize (:local-id view-part)
                         #(layout/set-dimensions-and-layout (dataflow/get-global-value (:root-element-path view-part))
                                                            0
                                                            0
                                                            requested-width
                                                            requested-height))
    view-part)

  (children-in-coordinates [this view-state x y]
    (let [layout (get view-state (element-path-to-layout-path (:root-element-path this)))]
      (concat [layout] (layout/children-in-coordinates layout view-state x y))))

  Object
  (toString [_] (str "(->ViewPart " root-element-path)))


(defn loaded-view-parts [gpu-state]
  (keys (:view-part-command-runners gpu-state)))

(defn view-part-is-loaded? [gpu-state view-part-layout-path]
  (contains? (:view-part-command-runners gpu-state) view-part-layout-path))

(defn unload-view-part [gpu-state layout-path]
  (dorun (map command/delete (get-in gpu-state [:view-part-command-runners layout-path])))
  (update-in gpu-state [:view-part-command-runners] dissoc layout-path))

(defn load-view-part [gpu-state view-state layout-path]
  (unload-view-part gpu-state layout-path)

  (debug/debug :view-update "loading " layout-path)

  (if (dataflow/is-defined? view-state layout-path)
    (let [drawing-commands (if-let [layout (get view-state layout-path)]
                             (drawable/drawing-commands layout)
                             [])
          gpu-state (reduce (fn [gpu-state layout-path]
                              (load-view-part gpu-state view-state layout-path))
                            gpu-state
                            (->> (filter #(and (instance? ViewPartCall %)
                                               (not (view-part-is-loaded? gpu-state (:view-part-layout-path %))))
                                         drawing-commands)
                                 (map :view-part-layout-path)))]

      (assoc-in gpu-state [:view-part-command-runners layout-path] (command/command-runners-for-commands drawing-commands)))
    gpu-state))

(defn update-view-part [gpu-state view-state layout-path]
  (if (dataflow/is-defined? view-state layout-path)
    (load-view-part gpu-state view-state layout-path)
    (unload-view-part gpu-state layout-path)))


(defn call-view-part [local-id]
  (->ViewPart local-id (dataflow/absolute-path local-id)))

(defn initialize-view-part [view-part-id view-part-element-function]
  (debug/debug :view-definition "initializing " view-part-id)
  (dataflow/initialize view-part-id view-part-element-function)
  (let [view-part-path (dataflow/absolute-path view-part-id)]
    (dataflow/initialize (concat (dataflow/as-path view-part-id) [:preferred-width]) #(layoutable/preferred-width (dataflow/get-global-value view-part-path)))
    (dataflow/initialize (concat (dataflow/as-path view-part-id) [:preferred-height]) #(layoutable/preferred-height (dataflow/get-global-value view-part-path)))))

(defn init-and-call [view-part-id view-part-element-function]
  (initialize-view-part view-part-id view-part-element-function)
  (call-view-part view-part-id))


;; RENDERING

(defn draw-view-part [gpu-state layout-path]
  (debug/debug :render "draw-view-part " layout-path)

  (doseq [command-runner (get-in gpu-state [:view-part-command-runners layout-path])]
    (if (instance? ViewPartCall command-runner)
      (draw-view-part gpu-state (:view-part-layout-path command-runner))
      (debug/debug-drop-last :render "running" (type command-runner)
                             (command/run command-runner)))))

(defn render [gpu-state]
  (opengl/clear 0 0 0 0)
  (draw-view-part gpu-state [:layout]))


;; MOUSE

(defn invert-mouse-y [window-height mouse-event]
  (if (contains? mouse-event :mouse-y)
    (assoc mouse-event :mouse-y (- window-height
                                   (:mouse-y mouse-event)))
    mouse-event))



(defn call-mouse-event-handlers [view-state event mouse-event-handlers]
  (reduce (fn [view-state mouse-event-handler]
            ((:handler mouse-event-handler) view-state event))
          view-state
          mouse-event-handlers))


(defn layoutables-in-coordinates [view-state x y]
  (let [layout (get view-state [:layout])]
    (concat [layout]
            (layout/children-in-coordinates layout
                                            view-state
                                            x
                                            y))))

(defn mouse-event-handlers-in-coordinates [view-state x y]
  (->> (flow-gl.debug/debug :default "layoutables-in-coordinates: " (vec (layoutables-in-coordinates view-state x y)))
       (filter #(not (= nil
                        (:mouse-event-handler %))))
       (map :mouse-event-handler)))

(defn call-mouse-enter-and-leave-handlers [view-state current-mouse-event-handlers-under-mouse old-mouse-event-handlers-under-mouse]
  (let [current-set (set (map :id current-mouse-event-handlers-under-mouse))
        old-set (set (map :id old-mouse-event-handlers-under-mouse))

        mouse-left (filter #(not (contains? current-set (:id %)))
                           old-mouse-event-handlers-under-mouse)

        mouse-entered (filter #(not (contains? old-set (:id %)))
                              current-mouse-event-handlers-under-mouse)]
    (-> view-state
        (call-mouse-event-handlers {:type :mouse-entered} mouse-entered)
        (call-mouse-event-handlers {:type :mouse-left} mouse-left))))


(deftest call-mouse-enter-and-leave-handlers-test
  (let [create-handler (fn [id]
                         (fn [state event]
                           (conj state [id event])))]
    (is (= (call-mouse-enter-and-leave-handlers []
                                                [{:id 1 :handler (create-handler 1)}]

                                                [{:id 1 :handler (create-handler 2)}])
           []))))

(defn update-mouse-event-handlers-under-mouse [view-state]
  (let [current-mouse-event-handlers-under-mouse (mouse-event-handlers-in-coordinates view-state
                                                                                      (get view-state [:mouse-x])
                                                                                      (get view-state [:mouse-y]))]

    (flow-gl.debug/debug :events "current-mouse-event-handlers-under-mouse " (vec current-mouse-event-handlers-under-mouse))

    (-> view-state
        (call-mouse-enter-and-leave-handlers current-mouse-event-handlers-under-mouse
                                             (:mouse-event-handlers-under-mouse view-state))
        (assoc :mouse-event-handlers-under-mouse current-mouse-event-handlers-under-mouse))))

(defn update-mouse-position [view-state event]
  (-> view-state
      (dataflow/define-to
        :mouse-x (:mouse-x event)
        :mouse-y (:mouse-y event))
      (update-mouse-event-handlers-under-mouse)))

(defn capture-mouse [view-state capturer]
  (assoc view-state :mouse-capturer capturer))

(defn release-mouse [view-state]
  (dissoc view-state :mouse-capturer))

(defn send-mouse-event [view-state event]
  (let [handlers (if-let [mouse-capturer (:mouse-capturer view-state)]
                   [{:handler mouse-capturer}]
                   (:mouse-event-handlers-under-mouse view-state))]
    (-> view-state

        (call-mouse-event-handlers (assoc event :event-handling-direction :down)
                                   handlers)

        #_(call-mouse-event-handlers (assoc event :event-handling-direction :up)
                                     (reverse handlers)))))

(defn add-mouse-event-handler [layoutable id new-handler]
  (assoc layoutable
    :mouse-event-handler {:id (dataflow/absolute-path id)
                          :handler (fn [view-state event]
                                     (if-let [handler (:mouse-event-handler layoutable)]
                                       (-> ((:handler handler) view-state event)
                                           (new-handler event))
                                       (new-handler view-state event)))}))

(defn handle-mouse-event [view-state event]
  (-> view-state
      (when-> (= (:type event) :mouse-moved)
              (update-mouse-position event))
      (send-mouse-event event)))

(defn handle-keyboard-event [view-state event]
  ((:event-handler view-state)
   view-state
   [:elements]
     event))

(defn trim-mouse-movements [events]
  (letfn [(is-mouse-move [event] (= :mouse-moved (:type event)))]
    (loop [events events
           trimmed-events []]
      (if-let [event (first events)]
        (recur (rest events)
               (if (and (is-mouse-move event)
                        (is-mouse-move (first (rest events))))
                 trimmed-events
                 (conj trimmed-events event)))
        trimmed-events))))

;; EVENT HANDLING

(defn send-close-event [view-state]
  ((:event-handler view-state)
   view-state
   [:elements]
     {:type :close}))

(defn handle-event [view-state event]
  (debug/debug :events "handle event " event)
  (let [view-state (assoc view-state :event-handled false)]
    (case (:source event)
      :keyboard (handle-keyboard-event view-state event)
      :mouse (handle-mouse-event view-state event)
      (throw (Exception. (str "unknown source " (:source event)))))))

(defn handle-events [view]
  (let [unread-events (->> (concat (input/unread-keyboard-events)
                                   (->> (input/unread-mouse-events)
                                        (trim-mouse-movements)
                                        (map (partial invert-mouse-y (get view [:height])))))
                           (sort-by :time))]

    (reduce handle-event view unread-events)))


(defn update-view [view-atom]
  (when (not (empty? (dataflow/changes @view-atom)))
    (let [view-state (swap! view-atom (fn [view-state]
                                        (-> view-state
                                            (assoc :changes-to-be-processed (dataflow/changes view-state))

                                            (dataflow/reset-changes))))

          changed-view-part-layout-paths (filter #(view-part-is-loaded? @(:gpu-state view-state) %)
                                                 (:changes-to-be-processed view-state))]

      (debug/debug :events "New view state:")
      (dorun (map #(debug/debug :events %)
                  (dataflow/describe-dataflow view-state)))
      (when (not (empty? changed-view-part-layout-paths))
        (-> (swap! (:gpu-state view-state)
                   (fn [gpu-state]
                     (-> (reduce (fn [gpu-state layout-path]
                                   (update-view-part gpu-state view-state layout-path))
                                 gpu-state
                                 changed-view-part-layout-paths)
                         ((fn [gpu-state]
                            (debug/debug :view-update "New gpu state:")
                            (dorun (map #(debug/debug :view-update %) (describe-gpu-state gpu-state)))
                            gpu-state)))))
            (render))))))

;; FPS

(defn add-fps [view-state time-now]
  (update-in view-state [:fpss]
             (fn [fpss]
               (let [fpss (conj fpss (/ 1
                                        (/ (- time-now
                                              (:last-update-time view-state))
                                           1E9)))]
                 (if (> (count fpss)
                        20)
                   (vec (rest fpss))
                   fpss)))))

(defn average-fps [view]
  (let [fpss (:fpss view)]
    (/ (apply + fpss)
       (max 1 (count fpss)))))

(defn update-fps [view]
  (let [time-now (System/nanoTime)]
    (-> view
        (add-fps time-now)
        (dataflow/define-to :fps (average-fps view))
        (assoc :last-update-time time-now))))

;; TIME

(defn update-time [view]
  (dataflow/define-to view :time (System/nanoTime)))


;; UPDATE

(defn update [view-atom]
  (swap! view-atom
         (fn [view]
           (-> view
               (handle-events)
               (dataflow/propagate-changes)
               (update-time)
               ;;(update-fps)
               )))

  (update-view view-atom))


;; INITIALIZATION

(defn initialize-view-state [width height root-element-constructor]
  (-> (dataflow/create)
      (dataflow/define-to
        :width width
        :height height
        :mouse-x 0
        :mouse-y 0
        :fps 0
        :elements root-element-constructor
        :layout #(layout/set-dimensions-and-layout (dataflow/get-global-value :elements)
                                                   0
                                                   0
                                                   (dataflow/get-global-value :width)
                                                   (dataflow/get-global-value :height)))))

(defn create [width height event-handler root-element-constructor]
  (let [gpu-state (atom {:view-part-command-runners {}})
        view-state (-> (initialize-view-state width
                                              height
                                              root-element-constructor)
                       (assoc
                           :fpss []
                           :last-update-time (System/nanoTime)
                           :mouse-event-handlers-under-mouse []
                           :gpu-state gpu-state
                           :event-handler event-handler))]

    (swap! gpu-state load-view-part
           view-state [:layout])

    view-state))


(defn handle-resize [view-atom width height]
  (swap! view-atom dataflow/define-to
         :width width
         :height height))


(defn set-view [state view]
  (-> state
      (dataflow/define-to [:elements] view)
      (dataflow/propagate-changes)))

;; HELPERS

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn with-mouse-over [layoutable key]
  (let [this (dataflow/absolute-path [])]
    (dataflow/initialize key false)
    (add-mouse-event-handler layoutable [this key]
                             (fn [application-state event]
                               (case (:type event)
                                 :mouse-entered (dataflow/define-property-to application-state this key true)
                                 :mouse-left (dataflow/define-property-to application-state this key false)
                                 application-state)))))



(run-tests)