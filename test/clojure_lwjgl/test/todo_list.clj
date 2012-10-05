(ns clojure-lwjgl.test.todo-list
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input]
                           [logged-access :as logged-access]
                           [zipper-list :as zipper-list])
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

(defrecord ViewPartCall [id function])
(extend ViewPartCall
  command/Command
  {:create-runner identity}
  command/CommandRunner
  {:delete identity
   :run identity})

(defn draw-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (if (instance? ViewPartCall command-runner)
      (draw-view-part application (:id command-runner))
      (command/run command-runner))))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (draw-view-part application (:root-view-part application))

  application)

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))



(defn handle-events [application application-state]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application-state
      (reduce (partial (:event-handler application-state) application) application-state unread-events))))

(defn define-view-part-calls [application-state view-part-calls]
  (reduce (fn [application-state view-part-call]
            (dataflow/define application-state
              [:view-parts (:id view-part-call)] (:function view-part-call)))
          application-state
          view-part-calls))

(defn undefine-uncalled-view-parts [application-state]
  (let [view-part-calls (filter #(instance? ViewPartCall %)
                                (apply concat (vals (:view-parts application-state))))
        called-view-part-ids (-> (set (map :id view-part-calls))
                                 (conj (:root-view-part application-state)))
        all-view-part-ids (set (keys (:view-parts application-state)))
        uncalled-view-part-ids (clojure.set/difference all-view-part-ids called-view-part-ids)]
    (reduce (fn [application-state view-part-id]
              (-> application-state
                  (dataflow/undefine [:view-parts view-part-id])
                  (update-in [:view-parts] dissoc view-part-id)))
            application-state
            uncalled-view-part-ids)))

(defn update-view [application]
  (when (not (empty? (dataflow/changes @application)))
    (let [application-state (swap! application #(-> %
                                                    (assoc :changes-to-be-processed (dataflow/changes %))
                                                    (dataflow/reset-changes)))

          changes (:changes-to-be-processed application-state)

          changed-view-parts (->> (filter #(= (first %)
                                              :view-parts)
                                          changes)
                                  (map second))



          undefined-view-part-calls (->> changed-view-parts
                                         (mapcat #(get-in application-state [:view-parts %]))
                                         (filter #(and (instance? ViewPartCall %)
                                                       (not (contains? (:view-parts application-state)
                                                                       (:id %))))))

          new-command-runners (reduce (fn [command-runners view-part]
                                        (dorun (map command/delete (get command-runners view-part)))
                                        (assoc command-runners
                                          view-part (command/command-runners-for-commands (get-in application-state
                                                                                                  [:view-parts view-part]))))
                                      (:view-part-command-runners application-state)
                                      changed-view-parts)]

      (let [application-state (swap! application
                                     #(-> %
                                          (assoc :view-part-command-runners new-command-runners)
                                          (undefine-uncalled-view-parts)
                                          (define-view-part-calls undefined-view-part-calls)))]
        (if (empty? undefined-view-part-calls)
          (render application-state)
          (update-view application))))))

(defn update [application]
  (swap! application (partial handle-events application))
  (update-view application)
  application)

(defmacro view-part [name arguments & body]
  `(defn ~name [~@arguments]
     (->ViewPartCall [~name ~@arguments]
                     (fn [] ~@body))))


(defn set-view [application-state view-part]
  (-> (define-view-part-calls application-state [(view-part)])
      (assoc :root-view-part (:id (view-part)))))

(defn create-application [event-handler root-view-part]
  (-> (dataflow/create)
      (assoc
          :view-part-command-runners {}
          :event-handler event-handler)
      (set-view root-view-part)
      (atom)))


;; TODO-LIST

(defn add-item [application-state index value]
  (let [new-id (rand-int 10000)]
    (-> application-state
        (dataflow/define [:items new-id] value)
        (dataflow/apply-to-value [:item-order] #(zipper-list/insert % new-id index)))))

(defn remove-item [application-state index]
  (println "remove item")
  (let [id (get (zipper-list/items (:item-order application-state))
                (:selection application-state))]
    (println "removing " id)
    (-> application-state
        (dataflow/undefine [:items id])
        (update-in [:items] dissoc id)
        (dataflow/apply-to-value [:item-order] #(zipper-list/remove % id)))))


(defn handle-event [application application-state event]
  (println event)
  (cond

   (key-pressed event input/down)
   (dataflow/apply-to-value application-state :selection inc)

   (key-pressed event input/up)
   (dataflow/apply-to-value application-state :selection dec)

   (key-pressed event input/space)
   (add-item application-state (:selection application-state) "New item" )

   (key-pressed event input/backspace)
   (remove-item application-state (:selection application-state))
   
   :default application-state))

(view-part editor [item-index selected]
           #_(println "running editor " item-index " " selected)
           (vector (vector-rectangle/rectangle 0 0
                                               100 30
                                               (if selected
                                                 [0 0 1 1]
                                                 [0.9 0.9 1 1]))
                   (text/create 5 5
                                (str (dataflow/get-value-in [:items item-index]))
                                (font/create "LiberationSans-Regular.ttf" 15)
                                [0.0 0.0 0.0 1.0])))

(view-part background []
           #_(println "running background")
           (dataflow/with-values [width height]
             [(vector-rectangle/rectangle 0 0
                                          width height
                                          [1 1 1 1])]))

(view-part item-list []
           #_(println "running item-list")
           (dataflow/with-values [item-order selection]
             (flatten (map-indexed (fn [line-number item-index]
                                     [(push-modelview/->PushModelview)
                                      (translate/->Translate 0
                                                             (* line-number 30))
                                      (editor item-index
                                              (= selection
                                                 line-number))
                                      (pop-modelview/->PopModelview)])
                                   (zipper-list/items item-order)))))

(view-part item-view []
           [(background)
            (item-list)])

#_(comment
    {:item-view {:children {:item-list-1 {:items {0 "Foo"
                                                  1 "Bar"}
                                          :item-order [0 1 2]
                                          :selected 0
                                          :children {:editor-0 {:in-focus #(= (get-value :selected)
                                                                              0)
                                                                :value #(get-value-in [:items 0])
                                                                :edited-value "Foo"
                                                                :editing false
                                                                :cursor-position 0}
                                                     :editor-1 {:in-focus #(= (get-value :selected)
                                                                              1)
                                                                :value #(get-value-in [:items 1])
                                                                :edited-value "Bar"
                                                                :editing false
                                                                :cursor-position 0}}}}}}
    (view-part item-list [item-ids]
               (vertical-stack (map (fn [[item-id]]
                                      (editor value))
                                    item-ids))
               (map-indexed (fn [line-number item-index]
                              [(push-modelview/->PushModelview)
                               (translate/->Translate 0
                                                      (* line-number 30))
                               (editor item-index
                                       (= selection
                                          line-number))
                               (pop-modelview/->PopModelview)])
                            item-order)))

(defn create-todo-list [window]
  (println "Creating application")
  (let [application (create-application handle-event item-view)]
    (swap! application (fn [application-state]
                         (-> application-state
                             (dataflow/define
                               :width  @(:width window)
                               :height  @(:height window)
                               :selection 0
                               :item-order (zipper-list/create))
                             (add-item 0 "Foo")
                             (add-item 0 "Bar")
                             (add-item 0 "FooBar"))))
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
                         #(dataflow/define
                            %
                            :width width
                            :height height))
                  application)))

(comment
(start))