(ns flow-gl.applications.midi
  (:import javax.sound.midi.MidiSystem))


(defn get-transmitter-device [name]
  (->> (MidiSystem/getMidiDeviceInfo)
       (filter (fn [device-info]
                 (and (-> device-info
                          (MidiSystem/getMidiDevice)
                          (.getMaxTransmitters)
                          (not= 0))
                      (= name (.getName device-info)))))
       (first)
       (MidiSystem/getMidiDevice)))

(comment

(def transmitter-device (get-transmitter-device "II [hw:1,0,0]"))
(def receiver (.getReceiver receiver-device))

)