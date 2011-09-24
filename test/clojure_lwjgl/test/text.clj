(ns clojure-lwjgl.test.text
  (:use clojure.test)
  (:require [clojure-lwjgl.text :as text] :verbose))

(def text {:content "foo"})

(deftest width-should-be-positive
  (is (> (text/get-width text)
         0)))

(deftest height-should-be-positive
  (is (> (text/get-height text)
         0)))

(def graphics-mock (proxy [java.awt.Graphics2D] []
                     (setColor [_])
                     (setFont [_])
                     (setRenderingHint [_ _])
                     (drawString [string _ _] (is (= string "foo")))))

(deftest render
  (text/render text graphics-mock ))

(run-tests 'clojure-lwjgl.test.text)