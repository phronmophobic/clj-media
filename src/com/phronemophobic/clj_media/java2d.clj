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
  (:import com.sun.jna.Pointer
           java.awt.image.BufferedImage
           java.awt.image.DataBuffer
           java.awt.image.DataBufferByte
           java.awt.image.WritableRaster
           java.awt.image.Raster
           javax.imageio.ImageIO
           java.awt.image.ColorModel
           java.awt.Point))

(raw/import-structs!)

(def band-masks (int-array [0xFF0000, 0xFF00, 0xFF]))
(def default-cm (ColorModel/getRGBdefault))
(def default-cm (ColorModel/OPAQUE))

(defn rects->img [buffer width height]
  (let [
        db (DataBufferByte. buffer (alength buffer))
        raster (Raster/createPackedRaster db width height width band-masks nil)
        bi (BufferedImage. default-cm
                           raster
                           false ;; (.isAlphaPremultiplied default-cm)
                           nil)]
    bi))


(def input-context (av/open-context (.getAbsolutePath (io/file "test.mov"))))
(def decoder-context (av/find-decoder-context :video input-context))
(def width (.readField decoder-context "width"))
(def height (.readField decoder-context "height"))

(def buffered-image (bufimg/new-image height width :byte-bgr))
(.readField decoder-context "pix_fmt")
(doto (.readField decoder-context "time_base")
  .read
  pprint)



(def xform (comp av/read-frame
                 (av/decode-frame decoder-context)
                 (video/transcode-frame decoder-context AV_PIX_FMT_BGR24)
                 (take 1)
                 ))
(def rf (xform
         (completing (fn [_ frame]
                       (prn frame)
                       frame))))
(def frame (unreduced (rf true input-context)))

#_(def rf (xform
           (completing
            write-frame)))

(defn count-frames []
  (let [input-context (av/open-context (.getAbsolutePath (io/file "test.mov")))
        decoder-context (av/find-decoder-context :video input-context)]
    (transduce (comp av/read-frame
                     (av/decode-frame decoder-context))
               (completing
                (fn [n _]
                  (inc n)))
               0
               [input-context])))

(rf buffered-image input-context)
(java2d/run (constantly
             (ui/image buffered-image)))


(defn- pixfmt->n-bytes
  [^long pixfmt]
  (condp = pixfmt
    AV_PIX_FMT_RGB24 3
    AV_PIX_FMT_BGR24 3
    AV_PIX_FMT_ARGB 4
    AV_PIX_FMT_RGBA 4
    AV_PIX_FMT_ABGR 4
    AV_PIX_FMT_BGRA 4
    AV_PIX_FMT_GRAY8 1
    nil))

(defn- fix-frame-padding
  [data ^long width pixfmt]
  (if-let [n-channels (pixfmt->n-bytes pixfmt)]
    (let [cropsize (* width n-channels)
          [_ dlinesize] (dtype/shape data)]
      (->
       (if (not= cropsize dlinesize)
         (dtt/select data :all (range cropsize))
         data)
       ;;reshape to be more image like
       (dtt/reshape [(first (dtype/shape data)) width n-channels])))
    data))

(defn write-frame [img frame]
  ;; assuming planar format
  (def my-frame
    (doto frame
      .read
      pprint))
  (let [height (.readField frame "width")
        height (.readField frame "height")
        linesize (-> frame
                      (.readField "linesize")
                      (nth 0))

        pixfmt (.readField frame "format")
        buf-ptr (-> frame
                    (.readField "data")
                    (nth 0)
                    (.getPointer))


        buf (-> (native-buffer/wrap-address (Pointer/nativeValue buf-ptr)
                                        (* height linesize)
                                        frame)
            (native-buffer/set-native-datatype :uint8)
            (dtt/reshape [height linesize])
            (fix-frame-padding width pixfmt))]
    (dtype/copy! buf img)
    #_(rects->img buf width height)))

(defn write-frame-rf [img]
  (fn
    ([])
    ([img])
    ([img frame]
     
     ))
  )




(defn play-video [fname]
  "Open a window and play the video found at `fname`."
  ;; (avclj/initialize!)
  (let [decoder
        (avclj/make-video-decoder fname)

        {:keys [width height]} (meta decoder)
        {:keys [num den] :as time-base} (-> decoder meta :time-base )
        fps (/ num den)

        buffered-image (bufimg/new-image height width :byte-bgr)

        window-info (java2d/run
                      (fn []
                        (ui/image buffered-image))
                      {:window-start-width width
                       :window-start-height width})
        repaint (::java2d/repaint window-info)
        start-time (System/currentTimeMillis)]
    (try
      (loop [t 0]
        (let [start-frame-time (System/currentTimeMillis)
              frame-data (avclj/decode-frame! decoder)]
          (when frame-data

            (let [best-effort-timestamp (-> frame-data
                                            meta
                                            :best-effort-timestamp)
                  sleep-ms (- (* 1000 fps best-effort-timestamp)
                              (- (System/currentTimeMillis) start-time ))]
              (when (pos? sleep-ms)
                (Thread/sleep sleep-ms))
              (dtype/copy! (first (#'avclj/raw-frame->buffers frame-data))

                           buffered-image)

              (repaint)

              (recur best-effort-timestamp))

            )))
      (catch Exception e
        (println e))
      (finally
        (.close decoder)))))

(defn -main [fname]
  (play-video fname))
