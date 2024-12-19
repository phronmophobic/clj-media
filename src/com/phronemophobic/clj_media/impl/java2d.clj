(ns com.phronemophobic.clj-media.impl.java2d
  (:require [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            [clojure.java.io :as io]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.video :as video]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [tech.v3.tensor :as dtt]

            [clojure.pprint :refer [pprint]]
            ;; [avclj :as avclj]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [com.phronemophobic.clong.gen.jna :as gen]
            [tech.v3.datatype :as dtype]
            [tech.v3.libs.buffered-image :as bufimg]
            )
  (:import com.sun.jna.Pointer
           com.sun.jna.Structure
           java.awt.Graphics2D
           java.awt.Color
           java.awt.RenderingHints))

(set! *warn-on-reflection* true)

(raw/import-structs!)

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

        ^AVRational
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
              ^AVFrame
              frame (loop []
                      (let [packet (av/next-packet input-context)]
                        (when packet
                          (let [frame (rf nil packet)]
                            (if frame
                              frame
                              (recur))))))]

          (when frame
            (let [best-effort-timestamp (long (.readField frame "best_effort_timestamp"))
                  sleep-ms (- (* 1000 fps best-effort-timestamp)
                              (- (System/currentTimeMillis) start-time ))]
              (when (pos? sleep-ms)
                (Thread/sleep (long sleep-ms)))
              (video/render-frame buffered-image frame)

              (repaint)

              (recur best-effort-timestamp))

            )))
      (catch Exception e
        (println e)))))



(defn -main [fname]
  (play-video fname))

(defn draw-to-image
  ([elem size]

   (let [[w h :as size] (if size
                          size
                          (let [[w h] (ui/bounds elem)
                                [ox oy] (ui/origin elem)]
                            [(+ w ox)
                             (+ h oy)]))
         _ (assert (and (pos? (first size))
                        (pos? (second size)))
                   "Size must be two positive numbers [w h]")
         img (bufimg/new-image h w :byte-bgr)]
     (binding [java2d/*g* (.createGraphics img)
               java2d/*image-cache* (atom {})]
       (.setRenderingHint ^Graphics2D java2d/*g*
                          RenderingHints/KEY_ANTIALIASING
                          RenderingHints/VALUE_ANTIALIAS_ON)
       (.setRenderingHint ^Graphics2D java2d/*g*
                          RenderingHints/KEY_TEXT_ANTIALIASING,
                          RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
       (.setFont ^Graphics2D java2d/*g* (java2d/get-java-font (ui/font nil nil)))

       (java2d/draw (ui/filled-rectangle [1 1 1]
                                  w h))
       (.setColor ^Graphics2D java2d/*g* (Color/BLACK))
       (java2d/draw elem))
     img)))

(defn raster-data [^java.awt.image.BufferedImage img]
  (let [raster (.getData img)
        buffer (.getDataBuffer raster)
        buf (cond
              (instance? java.awt.image.DataBufferByte buffer) (.getData ^java.awt.image.DataBufferByte buffer)
              (instance? java.awt.image.DataBufferDouble buffer) (.getData ^java.awt.image.DataBufferDouble buffer)
              (instance? java.awt.image.DataBufferFloat buffer) (.getData ^java.awt.image.DataBufferFloat buffer)
              (instance? java.awt.image.DataBufferInt buffer) (.getData ^java.awt.image.DataBufferInt buffer)
              (instance? java.awt.image.DataBufferShort buffer) (.getData ^java.awt.image.DataBufferShort buffer)
              (instance? java.awt.image.DataBufferUShort buffer) (.getData ^java.awt.image.DataBufferUShort buffer)
              :else (throw (ex-info (str "Unknown class: " (class buffer)) {})))]
    buf))

(defn img->frame [^java.awt.image.BufferedImage img]
  (let [w (.getWidth img)
        h (.getHeight img)
        pix-fmt AV_PIX_FMT_RGB24
        ^AVFrame frame (av_frame_alloc)
        _ (doto frame
            (.writeField "width" w)
            (.writeField "height" h)
            (.writeField "format" pix-fmt))
        err av_frame_get_buffer]
    (assert (zero? (av_frame_get_buffer frame 0)))
    (assert (zero? (av_frame_make_writable frame)))
    (let [data-ptr (-> (.readField frame "data")
                       first
                       (Structure/.getPointer))
          buf (raster-data img)
          bytes-per-pixel 3
          source-linesize (* bytes-per-pixel w)
          dest-linesize (first (.readField frame "linesize"))]
      (doseq [y (range h)
              :let [offset (long (* y dest-linesize))
                    index (long (* y source-linesize))]]
        (cond
          (instance? byte/1 buf) (.write data-ptr offset ^byte/1 buf index source-linesize)
          (instance? char/1 buf) (.write data-ptr offset ^char/1 buf index source-linesize)
          (instance? double/1 buf) (.write data-ptr offset ^double/1 buf index source-linesize)
          (instance? float/1 buf) (.write data-ptr offset ^float/1 buf index source-linesize)
          (instance? int/1 buf) (.write data-ptr offset ^int/1 buf index source-linesize)
          (instance? long/1 buf) (.write data-ptr offset ^long/1 buf index source-linesize)
          (instance? short/1 buf) (.write data-ptr offset ^short/1 buf index source-linesize))))

    frame))

(comment
  (play-video "/Users/adrian/Downloads/231_sd_integrating_swiftui.mp4")
  ;; "/Users/adrian/workspace/membrane/todo-app.mp4"

  (java2d/draw-to-image)

  ,)


