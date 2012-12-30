(ns clojure-lwjgl.test.scanner
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input])
            (clojure-lwjgl.command [text :as text]
                                   [image :as image]
                                   [command :as command]
                                   [translate :as translate]
                                   [scale :as scale]
                                   [push-modelview :as push-modelview]
                                   [pop-modelview :as pop-modelview])
            (clojure.java [shell :as shell]
                          [io :as io])
            (clojure [string :as string]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))


(defn invalidate-view-part [application view-part]
  (swap! (:invalid-view-parts application)
         conj view-part))

(defn invalidate-view-parts [application & view-parts]
  (doall (map #(invalidate-view-part application %) view-parts)))

(defn validate-view-part [application view-part]
  (swap! (:invalid-view-parts application)
         disj view-part))

(defn is-view-part-invalid? [application view-part]
  (contains? @(:invalid-view-parts application) view-part))

(defn is-view-invalid? [application]
  (> (count @(:invalid-view-parts application))
     0))

(defn file-name-prefix [{:keys [archive-path document-number page-number]}]
  (str archive-path "/" document-number "_" page-number))

(defn document-name-prefix [{:keys [archive-path document-number]}]
  (str archive-path "/" document-number))

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
       (filter #(.endsWith % ".pdf"))
       (map #(first (clojure.string/split % #"\.")))
       (map read-string)))

(defn page-numbers [{:keys [archive-path] :as application}]
  (->> (files-in-document application (:document-number application))
       (map #(second (re-seq #"[0-9]+" %)))
       (map read-string)
       (set)
       (sort)))

(defn preview-file-name [application]
  (str (file-name-prefix application) "_preview.jpg"))

(defn archive-copy-file-name [application]
  (str (file-name-prefix application) ".jpg"))

(defn black-and-white-copy-file-name [application]
  (str (file-name-prefix application) "_black_and_white.tiff"))

(defn set-status [application status]
  (reset! (:status application) status)
  (invalidate-view-part application :foreground))

(def scanned-file-name "scanned.tif")

(defn scan []
  (with-open [output-stream (io/output-stream scanned-file-name)]
    (io/copy (:out (shell/sh "scanimage" "--resolution" "300" "--format=tiff" "-x" "210" "-y" "297"  :out-enc :bytes))
             output-stream)))
(comment
  (defn scan []
    (with-open [output-stream (io/output-stream scanned-file-name)]
      (io/copy (:out (shell/sh "gphoto2" "--capture-image-and-download" "--stdout" :out-enc :bytes))
               output-stream))))

(defn create-preview [application]
  (println (shell/sh "convert" scanned-file-name "-resize" "550x10000" (preview-file-name application))))

(defn create-black-and-white-copy [application]
  (shell/sh "convert" scanned-file-name "+dither" "-monochrome" "+matte" "-format" "tiff" "-compress" "Group4" (black-and-white-copy-file-name application)))

(defn create-archive-copy [application]
  (shell/sh "convert" scanned-file-name "-quality" "60" (archive-copy-file-name application)))

(defn create-pdf [application]
  (println "creating pdf")
  (let [black-and-white-file-names (map #(black-and-white-copy-file-name (assoc application
                                                                           :page-number %))
                                        (page-numbers application))]
    (apply shell/sh "tiffcp" (concat black-and-white-file-names
                                     ["pdf.tiff"])))

  (shell/sh "tiff2pdf" "-o" (str (document-name-prefix application) ".pdf") "pdf.tiff"))

(defn start-scanning [application]
  (.start (Thread. (fn []
                     (let [{:keys [status document-number page-number]} application
                           file-name-prefix (file-name-prefix application)]

                       (set-status application "Scanning...")
                       (scan)

                       (set-status application (str "Creating preview for document " document-number " page " page-number))
                       (create-preview application)
                       (invalidate-view-part application :preview)

                       ;;(set-status application "Creating archive copy...")
                       ;;(create-archive-copy application)

                       (set-status application "Creating black and white copy...")
                       (create-black-and-white-copy application)

                       (set-status application "Creating pdf...")
                       (create-pdf application)

                       (set-status application "Ready."))))))

(defn draw-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (command/run command-runner)))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (doseq [view-part (:view application)]
    (draw-view-part application view-part))

  application)

(defn update-view-part [application view-part]
  (if (is-view-part-invalid? application view-part)
    (do (validate-view-part application view-part)

        (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
          (command/delete command-runner))

        (assoc-in application [:view-part-command-runners view-part]
                  (command/command-runners-for-commands ((get-in application [:view-parts view-part]) application))))
    application))

(defn update-view [application]
  (if (is-view-invalid? application)

    (-> (reduce update-view-part application (:view application))
        render)

    application))

(defn background [{:keys [width height]}]
  [(vector-rectangle/rectangle 0 0
                               width height
                               [1 1 1 1])])

(defn lines [x y font color strings]
  (map-indexed (fn [line-number string]
                 (text/create x
                              (+ y (* line-number (font/height font)))
                              string
                              font
                              color))
               strings))

(defn foreground [{:keys [width height document-number status] :as application}]
  (lines 0 0 (font/create "LiberationSans-Regular.ttf" 15) [0.0 0.0 0.0 1.0]
         (concat [(str "Document number " document-number)
                  (str "Status: " @status)
                  (str "file name prefix: " (file-name-prefix application))]
                 (files-in-document application document-number))))

(defn preview [{:keys [width] :as application}]
  (if (.exists (File. (preview-file-name application)))

    [(push-modelview/->PushModelview)
     (translate/->Translate (- width (+ (* 0.7 550)
                                        10))
                            10)
     (scale/->Scale 0.7)
     (image/create 0
                   0
                   (preview-file-name application))
     (pop-modelview/->PopModelview)]

    []))




(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn handle-event [application event]
  (println event)
  (cond

   (key-pressed event input/down)
   (do (invalidate-view-part application :foreground)
       (invalidate-view-part application :preview)
       (-> application
           (update-in [:document-number] dec)
           (assoc :page-number 0)))

   (key-pressed event input/up)
   (do (invalidate-view-part application :foreground)
       (invalidate-view-part application :preview)
       (-> application
           (update-in [:document-number] inc)
           (assoc :page-number 0)))

   (key-pressed event input/right)
   (do (invalidate-view-part application :foreground)
       (invalidate-view-part application :preview)
       (update-in application [:page-number] inc))

   (key-pressed event input/left)
   (do (invalidate-view-part application :foreground)
       (invalidate-view-part application :preview)
       (update-in application [:page-number] dec))

   (key-pressed event input/page-up)
   (do (invalidate-view-parts application :foreground :preview)
       (assoc-in application [:document-number] (apply max (document-numbers application))))


   (key-pressed event input/page-down)
   (do (invalidate-view-part application :foreground)
       (invalidate-view-part application :preview)
       (assoc-in application [:document-number] 0))


   (key-pressed event input/space)
   (do (start-scanning application)
       application)

   (key-pressed event input/p)
   (do (create-pdf application)
       application)

   :default application))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application
      (reduce handle-event application unread-events))))

(defn update [application]
  (-> application
      (handle-events)
      (update-view)))

;; :archive-path "/home/jukka/Pictures/visa"
(defn create-application [window]
  (-> {:archive-path "/home/jukka/Spideroak/kuitit"
       :view-parts {:background background
                    :foreground foreground
                    :preview preview}
       :view [:background
              :preview
              :foreground]
       :invalid-view-parts (atom #{:background :foreground :preview})
       :preview-changed (atom false)
       :status (atom "Started")
       :view-part-command-runners {}
       :width @(:width window)
       :height @(:height window)
       :document-number 0
       :page-number 0}
      (update-view)))

(defn start []
  (window/start 700 500
                20
                create-application
                update
                identity
                (fn [application width height]
                  (invalidate-view-part application :foreground)
                  (invalidate-view-part application :background)
                  (invalidate-view-part application :preview)
                  (-> application
                      (assoc
                          :width width
                          :height height)
                      (update-view)))))

(comment
(start)


  (defrecord CommandRunnerBatch [command-runners]
    CommandRunner
    (delete [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map delete %))))
    (run [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map run %))))))