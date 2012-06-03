(defproject clojure-lwjgl "1.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :dependencies [[org.clojure/clojure "1.3.0"]
                 [org.lwjgl/lwjgl "2.7.1"]
                 [org.lwjgl/lwjgl-util "2.7.1"]
                 [org.lwjgl/lwjgl-native-platform "2.7.1"]
                 [org.clojure/data.priority-map "0.0.1"]
                 [jpen/jpen "2.111002"]
                 [jpen/jpen-native "2.111002"]
                 [midje "1.3.1"]
                 [analyze "0.1.5"]
                 [clojure-cello/clojure-cello "1.0.0-SNAPSHOT"]])
