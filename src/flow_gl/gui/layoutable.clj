(ns flow-gl.gui.layoutable)

(defprotocol Layoutable
  (preferred-width [element])
  (preferred-height [element]))


;; DESCRIPTION

(defn describe-property [layoutable property]
  (str (name property) ": " (property layoutable)))

(defn describe-properties [layoutable properties]
  (apply str (interpose " " (map (partial describe-property layoutable)
                                 properties))))

(defn describe-layoutable [layoutable name & properties]
  (str "(" name " " (describe-properties layoutable (keys layoutable) #_(concat [:x :y :width :height] properties)) ")"))
