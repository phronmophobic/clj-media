(ns com.phronemophobic.clj-media.java2d
  (:require [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [clojure.java.io :as io]

            [com.phronemophobic.clj-media.av :as av]
            [com.phronemophobic.clj-media.video :as video]
            [com.phronemophobic.clj-media.av.raw :as raw
             :refer :all]
            [tech.v3.tensor :as dtt]

            [clojure.pprint :refer [pprint]]
            ;; [avclj :as avclj]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [com.phronemophobic.clong.gen.jna :as gen]
            [tech.v3.datatype :as dtype]
            [tech.v3.libs.buffered-image :as bufimg]
            )
  (:import com.sun.jna.Pointer))

(raw/import-structs!)

(defmacro with-tile [[tile-bind img] & body]
  `(let [img# ~img
         ~tile-bind (.getWritableTile img# 0 0)]
     (try
       ~@body
       (finally
         (.releaseWritableTile img# 0 0)))))

(defn write-frame
  "Writes an AVFrame into a BufferedImage. Assumes :byte-bgr image format."
  [img frame]
  (let [
        width (.readField frame "width")
        height (.readField frame "height")
        linesize (-> frame
                     (.readField "linesize")
                     (nth 0))
        buf-ptr (-> frame
                    (.readField "data")
                    (nth 0)
                    (.getPointer))]

    (with-tile [wraster img]
      (doseq [y (range height)]
        (.setDataElements wraster 0 y width 1
                          (.getByteArray buf-ptr (* linesize y) linesize))))))

(defn play-video [fname]
  "Open a window and play the video found at `fname`."
  ;; (avclj/initialize!)
  (let [input-context (av/open-context (.getAbsolutePath (io/file fname)))
        decoder-context (av/find-decoder-context :video input-context)
        width (.readField decoder-context "width")
        height (.readField decoder-context "height")


        buffered-image (bufimg/new-image height width :byte-bgr)

        window-info (java2d/run
                      (fn []
                        (ui/image buffered-image))
                      {:window-start-width width
                       :window-start-height width})
        repaint (::java2d/repaint window-info)
        start-time (System/currentTimeMillis)

        time-base (.readField decoder-context "time_base")
        num (.readField time-base "num")
        den (.readField time-base "den")
        fps (/ num den)

        xform (comp (av/decode-frame decoder-context)
                    (video/transcode-frame decoder-context AV_PIX_FMT_RGB24))
        rf (xform (completing
                   (fn [_ frame]
                     frame)))]
    (try
      (loop [t 0]
        (let [start-frame-time (System/currentTimeMillis)
              frame (loop []
                      (let [packet (av/next-packet input-context)]
                        (when packet
                          (let [frame (rf nil packet)]
                            (if frame
                              frame
                              (recur))))))]

          (when frame
            (let [best-effort-timestamp (.readField frame "best_effort_timestamp")
                  sleep-ms (- (* 1000 fps best-effort-timestamp)
                              (- (System/currentTimeMillis) start-time ))]
              (when (pos? sleep-ms)
                (Thread/sleep sleep-ms))
              (write-frame buffered-image frame)

              (repaint)

              (recur best-effort-timestamp))

            )))
      (catch Exception e
        (println e)))))

(defn -main [fname]
  (play-video fname))
