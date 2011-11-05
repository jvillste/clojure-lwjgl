(ns clojure-lwjgl.sound.synthesizer
  (:import [javax.sound.midi MidiSystem]))

(def synthesizer (MidiSystem/getSynthesizer))

