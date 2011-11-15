(ns clojure-lwjgl.frp.frpgui
  (:require (clojure-lwjgl [window :as window]
                           [input :as input]
                           [visual-list :as visual-list]
                           [text :as text]
                           [free-layout :as free-layout])
            (clojure-lwjgl.frp [event-stream :as event-stream]
                               [signal :as signal]))
  (:import [org.lwjgl.opengl GL11]))


(defn was-key-pressed? [keyboard-state]
  (= (:type (:last-event keyboard-state)
            :key-pressed)))

(defn last-key [keyboard-state]
  (:key-code (:last-event keyboard-state)))

(defn last-character [keyboard-state]
  (:character (:last-event keyboard-state)))

(defn is-key-down? [keyboard-state key]
  (contains? (:keys-down keyboard-state)
             key))

(defn create-keyboard-state [keyboard-events]
  (signal/fold keyboard-events
               {:last-event nil
                :keys-down #{}}
               (fn [keyboard-state keyboard-event]
                 {:last-event keyboard-event
                  :keys-down (input/update-keys-down (:keys-down keyboard-state)
                                                     keyboard-event)})))

(defn create-text-buffer [keyboard-state]
  (signal/fold (signal/changes keyboard-state)
               ""
               (fn [text-buffer new-keyboard-state]
                 (if (was-key-pressed? new-keyboard-state)
                   (str text-buffer
                        (last-character new-keyboard-state))
                   text-buffer))))

(defn create-keys-down [keyboard-events]
  (signal/fold keyboard-events
               #{}
               input/update-keys-down))


(defn add-text-buffer-action? [keyboard-state]
  (and (was-key-pressed? keyboard-state)
       (= (last-key keyboard-state)
          input/enter)
       (is-key-down? keyboard-state input/control)))

(defn move-focus-down-action? [keyboard-state]
  (= (last-key keyboard-state)
     input/down))

(defn move-focus-down [text-buffer-list]
  (if (:has-focus (first text-buffer-list))
    (if (second text-buffer-list)
      (concat [(assoc (first text-buffer-list)
                 :has-focus false)]
              [(assoc (second text-buffer-list)
                 :has-focus true)]
              (rest (rest text-buffer-list)))
      text-buffer-list)
    (concat [(first text-buffer-list)]
            (move-focus-down (rest text-buffer-list)))))

(defn create-text-buffer-list [keyboard-state]
  (signal/fold (signal/changes keyboard-state)
               [(assoc (create-text-buffer keyboard-state)
                  :has-focus true)]
               (fn [text-buffer-list new-keyboard-state]
                 (cond
                  (add-text-buffer-action? new-keyboard-state)
                  (conj text-buffer-list (create-text-buffer keyboard-state))

                  (move-focus-down-action? new-keyboard-state)
                  (move-focus-down text-buffer-list)

                  :default text-buffer-list))))

(defn create-view-visuals [text-signal1 text-signal2]
  ;; (signal/create-from-signal text-signal1
  ;;                                (fn [old-visuals new-text]
  ;;                                  [(free-layout/layout 10
  ;;                                                       10
  ;;                                                       (text/create new-text))
  ;;                                   (free-layout/layout 10
  ;;                                                       40
  ;;                                                       currently-active-text-buffer
  ;;                                                       (text/create (str "second: " new-text)))]))
  )

(defn create-visual-list [visuals-signal]
  ;; (signal/create-from-signal visual-signal
  ;;                                (fn [old-visual-list new-visual]
  ;;                                  ))
  )


(defn create []
  (let [keyboard-events (event-stream/create)]
    (-> {:window (window/create 200 200)
         :keyboard-events keyboard-events
         :text-buffer-list (create-text-buffer-list (create-keyboard-state keyboard-events))
         ;; :visual-list (-> (visual-list/create)
         ;;                  (visual-list/add-visual (create-view-visuals text-buffer)))
         })))

(defn handle-input [gui]
  (doseq [event (input/unread-keyboard-events)]

    (event-stream/send-event (:keyboard-events gui)
                             event)
    (println "text buffer list: " (:text-buffer-list gui)))
  gui)

(defn update [gui]
  (assoc gui
    :window (window/update (:window gui)
                           5)))

(defn run []
  (let [initial-gui (create)]
    (try
      (loop [gui initial-gui]
        (if (not @(:close-requested (:window gui)))
          (recur (-> gui
                     (handle-input)
                     (update)))
          (window/close (:window gui))))
      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close (:window initial-gui))))))

(comment
  (run))






