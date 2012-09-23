(ns clojure-lwjgl.test.dataflow-scanner
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input]
                           [logged-access :as logged-access])
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
            (clojure [string :as string]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))


(defn file-name-prefix [{:keys [archive-path document-number page-number]}]
  (str archive-path "/" document-number "_" page-number))

(defn files-in-directory [directory-path]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (files-in-directory (.getPath file)))
                             (conj files file)))
          []
          (.listFiles (File. directory-path))))

(defn files-in-document [{:keys [archive-path]}
                         document-number]
  (filter (fn [file-name]
            (.startsWith file-name (str document-number "_")))
          (map #(.getName %) (files-in-directory archive-path))))

(defn document-numbers [{:keys [archive-path]}]
  (->> (files-in-directory archive-path)
       (map #(.getName %))
       (map #(first (clojure.string/split % #"_")))
       (map read-string)))

(defn preview-file-name [application]
  (str (file-name-prefix application) "_preview.jpg"))

(defn archive-copy-file-name [application]
  (str (file-name-prefix application) ".jpg"))

(defn set-status [application status]
  (swap! application dataflow/define :status status))

(def scanned-file-name "scanned.tif")

(comment (defn scan []
           (with-open [output-stream (io/output-stream scanned-file-name)]
             (io/copy (:out (shell/sh "scanimage" "--resolution" "300" "--format=tiff" "-x" "210" "-y" "297"  :out-enc :bytes))
                      output-stream))))
(defn scan []
  (with-open [output-stream (io/output-stream scanned-file-name)]
    (io/copy (:out (shell/sh "gphoto2" "--capture-image-and-download" "--stdout" :out-enc :bytes))
             output-stream)))

(defn create-preview [application]
  (println (shell/sh "convert" scanned-file-name "-resize" "550x10000" (preview-file-name application))))

(defn create-archive-copy [application]
  (shell/sh "convert" scanned-file-name "-quality" "60" (archive-copy-file-name application)))

(defn start-scanning [application]
  (let [{:keys [status document-number page-number] :as application-state} @application
        file-name-prefix (file-name-prefix application-state)]

    (set-status application "Scanning...")
    (scan)

    (set-status application (str "Creating preview for document " document-number " page " page-number))
    (create-preview application-state)

    (set-status application "Creating archive copy...")
    (create-archive-copy application-state)

    (set-status application "Ready.")))

(defn draw-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (command/run command-runner)))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (dorun (map (partial draw-view-part application)
              (:view application)))

  application)


(defn schedule-thread [application function]
  (update-in application [:scheduled-threads] conj function))

(defn launch-scheduled-threads [application]
  (doseq [function (:scheduled-threads @application)]
    (.start (Thread. (fn [] (function application)))))
  (swap! application #(assoc % :scheduled-threads #{})))

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn handle-event [application event]
  (cond

   (key-pressed event input/down)
   (dataflow/apply-to-value application :document-number dec)

   (key-pressed event input/up)
   (dataflow/apply-to-value application :document-number inc)

   (key-pressed event input/right)
   (dataflow/apply-to-value application :page-number inc)

   (key-pressed event input/left)
   (dataflow/apply-to-value application :page-number dec)

   (key-pressed event input/page-up)
   (dataflow/define application :document-number (apply max (document-numbers application)))

   (key-pressed event input/page-down)
   (dataflow/define application :document-number 0)

   (key-pressed event input/space)
   (schedule-thread application start-scanning)

   :default application))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application
      (reduce handle-event application unread-events))))


(defn update-view [application]
  (when (dataflow/changes @application)
    (let [application-state (swap! application #(-> %
                                                    (assoc :changes-to-be-processed (dataflow/changes %))
                                                    (dataflow/reset-changes)))
          changes (:changes-to-be-processed application-state)
          changed-view-parts (->> (filter #(= (first %)
                                              :view-parts)
                                          changes)
                                  (map second))
          new-command-runners (reduce (fn [command-runners view-part]
                                        (println "loading view-part " view-part)
                                        (dorun (map command/delete (view-part command-runners)))
                                        (assoc command-runners
                                          view-part (command/command-runners-for-commands (get-in application-state [:view-parts view-part]))))
                                      (:view-part-command-runners application-state)
                                      changed-view-parts)]
      (-> (swap! application
                 assoc :view-part-command-runners new-command-runners)
          (render)))))

(defn update [application]
  (swap! application handle-events)
  (launch-scheduled-threads application)
  (update-view application)
  application)


(defn background []
  (dataflow/with-values [width height]
    [(vector-rectangle/rectangle 0 0
                                 width height
                                 [1 1 1 1])]))

(defn lines [x y font color strings]
  (map-indexed (fn [line-number string]
                 (text/create
                  x
                  (+ y (* line-number (font/height font)))
                  string
                  font
                  color))
               strings))

(defn foreground []
  (dataflow/with-values [document-number status]
    (lines 0 0 (font/create "LiberationSans-Regular.ttf" 15) [0.0 0.0 0.0 1.0]
           (concat [(str "Document number " document-number)
                    (str "Status: " status)
                    (str "file name prefix: " (file-name-prefix (dataflow/values-to-map :archive-path :document-number :page-number)))]
                   (files-in-document (dataflow/values-to-map :archive-path) document-number)))))

(defn preview []
  (dataflow/with-values [width]
    (if (.exists (File. (preview-file-name (dataflow/values-to-map :archive-path :document-number :page-number))))

      [(push-modelview/->PushModelview)
       (translate/->Translate (- width (+ (* 1.3 550)
                                          10))
                              10)
       (scale/->Scale 1.3)
       (image/create 0
                     0
                     (preview-file-name (dataflow/values-to-map :archive-path :document-number :page-number)))
       (pop-modelview/->PopModelview)]

      [])))


(defn create-application [window]
  (println "Creating application")
  (-> (dataflow/create)
      (dataflow/define
        :archive-path "/home/jukka/Pictures/dia"
        :width  @(:width window)
        :height  @(:height window)
        :document-number 0
        :page-number 0
        :status "Started"
        :view-part-command-runners {}
        :scheduled-threads #{}

        [:view-parts :background] background
        [:view-parts :foreground] foreground
        [:view-parts :preview] preview
        :view [:background
               :preview
               :foreground])
      (atom)))

(defn start []
  (window/start 700 500
                20
                create-application
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
  (start)

  (defrecord CommandRunnerBatch [command-runners]
    CommandRunner
    (delete [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map delete %))))
    (run [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map run %))))))