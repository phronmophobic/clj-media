(ns com.phronemophobic.clj-media.skija
  (:require [membrane.ui :as ui]
            [membrane.skija :as skija]
            [avclj :as avclj]
            [avclj.av-codec-ids :as codec-ids]
            )
  (:import
   (org.jetbrains.skija Canvas
                        Surface
                        ImageInfo
                        ColorAlphaType
                        ColorType
                        Pixmap)
   (org.lwjgl.glfw  GLFW)))


(defn force-repaint! []
  (GLFW/glfwPostEmptyEvent))

(defn skia-draw-surface [surface image-info addr row-bytes]
  (let [pixmap (Pixmap/make ^ImageInfo image-info ^long addr ^long row-bytes)]
    (.writePixels ^Surface surface pixmap 0 0)))

(defrecord VideoView [n surface width height draw-lock]
  ui/IOrigin
  (-origin [_]
    [0 0])

  ui/IBounds
  (-bounds [_]
    [width height])

  skija/IDraw
  (draw [this]
    (locking draw-lock
      (when surface
        (.draw ^Surface surface skija/*canvas* 0 0 (skija/map->paint skija/*paint*))))))

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

        image-info (ImageInfo. width height ColorType/BGRA_8888 ColorAlphaType/UNPREMUL)
        surface (Surface/makeRaster ^ImageInfo image-info)

        draw-lock (Object.)
        video-view (map->VideoView
                    {:n @natom
                     :surface surface
                     :width width
                     :height height
                     :draw-lock draw-lock})
        window-info (skija/run
                      (fn []
                        (assoc video-view
                               :n @natom))
                      {:window-start-width width
                       :window-start-height height})
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
                  (skia-draw-surface surface image-info (:data frame-data) (:linesize frame-data)))
                (swap! natom inc)
                ;; (force-repaint!)

                (recur best-effort-timestamp)))))
        (catch Exception e
          (println e))
        (finally
          (.close decoder))))))


(defn -main [fname]
  (play-video fname))


