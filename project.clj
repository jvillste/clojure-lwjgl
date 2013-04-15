(defproject clojure-lwjgl "2.0.0-SNAPSHOT"
  :description "FIXME: write description"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"} ;; for priority-map
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [org.lwjgl/lwjgl "2.7.1"]
                 [org.lwjgl/lwjgl-util "2.7.1"]
                 [org.lwjgl/lwjgl-native-platform "2.7.1"]
                 [jpen/jpen "2.111002"]
                 [jpen/jpen-native "2.111002"]
                 ;; [midje "1.3.1"]
                 [analyze "0.1.5"]
                 [clojure-cello/clojure-cello "1.0.0"]
                 [slingshot "0.10.3"]
                 [org.clojure/data.priority-map "0.0.2"]
                 [com.datomic/datomic-free "0.8.3731"]]
  :plugins [[lein-swank "1.4.4"]]

  ;; :java-cmd "/usr/lib/jvm/java-6-sun/bin/java"
  :java-opts ["-Xmx60m"
              "-Xms40m"
              "-client"
              ;; "-XX:MinHeapFreeRatio=20"
              "-XX:MaxHeapFreeRatio=50"])
