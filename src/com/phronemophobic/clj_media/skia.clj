(ns com.phronemophobic.clj-media.skia
  (:require [membrane.ui :as ui]
            
            [membrane.skia :as skia]
            [avclj :as avclj]
            [avclj.av-codec-ids :as codec-ids]
            ;; [tech.v3.tensor :as dtt]
            ;; [tech.v3.datatype.ffi :as dt-ffi]
            ;; [tech.v3.datatype :as dtype]
            ;; [tech.v3.libs.buffered-image :as bufimg]
            )
  (:import com.sun.jna.Pointer
           com.sun.jna.Memory))


(defn ^:private long->pointer [n]
  (tech.v3.datatype.ffi.Pointer. n))

(skia/defc skia_bgra8888_draw skia/membraneskialib Void/TYPE [skia-resource buffer width height row-bytes])
(defn skia-bgra8888-draw [resource buffer width height row-bytes]
  (skia_bgra8888_draw resource buffer (int width) (int height) (int row-bytes)))

(skia/defc skia_direct_bgra8888_buffer skia/membraneskialib Pointer [buf width height row-bytes])
(defn skia-direct-bgra8888-buffer [buf width height row-bytes]
  (skia_direct_bgra8888_buffer buf (int width) (int height) (int row-bytes)))

(skia/defc skia_cleanup skia/membraneskialib Void/TYPE [skia-resource])

(skia/defc skia_draw_surface skia/membraneskialib Void/TYPE [destination source])

(defrecord VideoView [n resource buffer width height draw-lock]
  ui/IOrigin
  (-origin [_]
    [0 0])

  ui/IBounds
  (-bounds [_]
    [width height])

  skia/IDraw
  (draw [this]
    (locking draw-lock
      (when resource
        (skia_draw_surface skia/*skia-resource* resource)))))

(defn play-video
  "Open a window and play the video found at `fname`."
  [fname]
  (avclj/initialize!)
  (let [decoder
        (avclj/make-video-decoder fname
                                  {:output-pixfmt "AV_PIX_FMT_BGRA"})
        natom (atom 0)

        {:keys [width height]} (meta decoder)
        {:keys [num den] :as time-base} (-> decoder meta :time-base )
        fps (/ num den)

        buffer (Memory. (* height width 4))
        resource (skia-direct-bgra8888-buffer buffer width height (* width 4))
        draw-lock (Object.)
        video-view (map->VideoView
                    {:n @natom
                     :resource resource
                     :buffer buffer
                     :width width
                     :height height
                     :draw-lock draw-lock})
        window-info (skia/run
                      (fn []
                        (assoc video-view
                               :n @natom))
                      {:window-start-width width
                       :window-start-height height})
        repaint (:membrane.skia/repaint window-info)
        start-time (System/currentTimeMillis)]
    (future
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

                (locking draw-lock
                  (skia-bgra8888-draw resource
                                      (Pointer. (:data frame-data))
                                      width
                                      height
                                      (:linesize frame-data)))
                (swap! natom inc)
                (repaint)

                (recur best-effort-timestamp)))))
        (catch Exception e
          (println e))
        (finally
          (.close decoder))))))


(comment
  (def encoder (avclj/make-video-encoder
                100 100
                "foo.mp4"
                (merge
                 {:input-pixfmt "AV_PIX_FMT_BGRA"
                  :fps-numerator 60
                  :fps-denominator 1})))

  (def decoder (avclj/make-video-decoder "my-movie.mp4"))
  ,)

(defn write-video
  "Save a video to `fname`.

  `fname`: A string path to where the video should be written.
  `frames`: A sequence of membrane views, one for each frame.
  `width`: Width of the gif in pixels.
  `height`: Height of the gif in pixels.
  `opts`: Optional parameters to pass to `avclj/make-video-encoder`."
  [fname frames
   width height
   &
   [{:keys [encoder-name
            encoder-pixfmt
            fps-numerator
            fps-denominator]
     :or {encoder-name codec-ids/AV_CODEC_ID_H264
          fps-numerator 60
          fps-denominator 1}
     :as opts}]]

  (avclj/initialize!)
  

  (with-open [encoder (avclj/make-video-encoder
                       height width
                       fname
                       (merge
                        {:input-pixfmt "AV_PIX_FMT_BGRA"
                         :encoder-name encoder-name
                         :fps-numerator fps-numerator
                         :fps-denominator fps-denominator}
                        (when encoder-pixfmt
                          {:encoder-pixfmt encoder-pixfmt})))]
    
    (doseq [[i frame-elem] (map-indexed vector frames)]
      (let [input-frame (avclj/get-input-frame encoder)
            skia-resource (skia-direct-bgra8888-buffer (Pointer. (:data input-frame))
                                                       (:width input-frame)
                                                       (:height input-frame)
                                                       (:linesize input-frame))]
        (binding [skia/*skia-resource* skia-resource
                  skia/*image-cache* (atom {})
                  skia/*already-drawing* true]
          (#'skia/skia_clear skia-resource)
          (skia/draw frame-elem)
          (#'skia/skia_flush skia-resource))
        (avclj/encode-frame! encoder input-frame)))))


(defn write-gif
  "Save a gif to `fname`.

  `fname`: A string path to where the gif should be written.
  `frames`: A sequence of membrane views, one for each frame.
  `width`: Width of the gif in pixels.
  `height`: Height of the gif in pixels.
  `opts`: Optional parameters to pass to `avclj/make-video-encoder`."
  ([fname frames width height opts]
   (write-video fname frames
                width height
                (merge {:fps-numerator 48
                        :encoder-name codec-ids/AV_CODEC_ID_GIF
                        :encoder-pixfmt "AV_PIX_FMT_PAL8"}
                       opts)))
  ([fname frames width height]
   (write-gif fname frames width height nil)))

(comment
  (write-gif "my-filter.gif"
             (map (fn [i]
                    (ui/padding 10 (ui/label (str "frame: " i))))
                    (range 120))
             200 200
             {:fps-numerator 24})

  ,)


(defn -main [fname]
  (play-video fname))

(defn gen-test-video [& args]
  (write-video "my-movie.mp4"
               (map (fn [i]
                      (ui/padding 10 (ui/label (str "frame: " i))))
                    (range 120))
               100 100))
