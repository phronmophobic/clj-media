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
            [tech.v3.datatype.nio-buffer :as dt-nio-buffer]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import java.awt.image.BufferedImage)
  (:gen-class))


(defn pf [& args]
  (apply prn args)
  (flush))

(defmacro with-tile [[tile-bind img] & body]
  `(let [img# ~img
         ~tile-bind (.getWritableTile img# 0 0)]
     (try
       ~@body
       (finally
         (.releaseWritableTile img# 0 0)))))

(def ^:private pixel-format->buffered-image-format
  {(datafy-media/kw->pixel-format :pixel-format/rgb24) BufferedImage/TYPE_3BYTE_BGR
   (datafy-media/kw->pixel-format :pixel-format/rgba) BufferedImage/TYPE_4BYTE_ABGR
   (datafy-media/kw->pixel-format :pixel-format/rgb555le) BufferedImage/TYPE_USHORT_555_RGB
   (datafy-media/kw->pixel-format :pixel-format/rgb565le) BufferedImage/TYPE_USHORT_565_RGB
   })


(defn render-frame
  "Writes an AVFrame into a BufferedImage. Assumes :byte-bgr image format."
  [img frame]
  (let [
        width (:width frame)
        height (:height frame)
        linesize (nth (:linesize frame) 0)
        buf-addr (nth (:data frame) 0)

        dtype (condp contains? (.getType img)
                #{BufferedImage/TYPE_3BYTE_BGR
                  BufferedImage/TYPE_4BYTE_ABGR} :int8

                #{BufferedImage/TYPE_USHORT_555_RGB
                  BufferedImage/TYPE_USHORT_565_RGB} :int16)


        data (-> (native-buffer/wrap-address buf-addr (* linesize height))
                 (dtt/reshape [height linesize]))

        ;; row-bytes (* width (dt-casting/numeric-byte-width dtype))
        temp-buf (byte-array linesize)

        ]

    (with-tile [wraster img]
      (doseq [y (range height)]
        (dt/copy! (dtt/select (nth data y) (range linesize))
                  temp-buf)
        (.setDataElements wraster 0 y width 1
                          temp-buf)))))

(defn frame->img [frame]

  (let [format (pixel-format->buffered-image-format (:format frame))
        _ (when (not format)
            (throw (ex-info "Unsupported format"
                            {:format (datafy-media/pixel-format->kw (:format frame))})))
        width (:width frame)
        height (:height frame)
        img (BufferedImage. width height format)]
    (render-frame img frame)
    img))

;; used by model
(defn frame->buf [frame]
  (let [buf-size (first (:linesize frame))
        nbuf (native-buffer/wrap-address (:extended_data frame)
                                         buf-size)]
    (dt-nio-buffer/->nio-buffer nbuf)))


#_(require '[tech.v3.libs.buffered-image :as dt-image])