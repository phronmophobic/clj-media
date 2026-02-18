(ns com.phronemophobic.clj-media.impl.video
  (:require [clojure.java.io :as io]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.datafy
             :as datafy-media]
            [tech.v3.tensor :as dtt ]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [tech.v3.datatype.casting :as dt-casting]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import java.awt.image.BufferedImage)
  (:gen-class))


(defn pf [& args]
  (apply prn args)
  (flush))

;; used by model
(defn frame->buf [frame]
  (let [buf-size (first (:linesize frame))
        buf (.getByteBuffer (:extended_data frame)
                            0
                            buf-size)]
    buf))

