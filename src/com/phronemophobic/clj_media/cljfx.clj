(ns com.phronemophobic.clj-media.cljfx
  (:require [membrane.ui :as ui]
            membrane.component
            [membrane.cljfx :as cljfx]
            
            [avclj :as avclj]
            [tech.v3.datatype :as dtype]
            [tech.v3.libs.buffered-image :as bufimg]
            )
  (:import javafx.embed.swing.SwingFXUtils
           javafx.scene.canvas.GraphicsContext)
  (:gen-class))


(defrecord VideoViewCljfx [best-effort-timestamp buffered-image draw-lock width height]
  ui/IOrigin
  (-origin [_]
    [0 0])

  ui/IBounds
  (-bounds [_]
    [width height])

  cljfx/IDraw
  (draw [this]
    (locking draw-lock
      (when buffered-image
        (let [img (SwingFXUtils/toFXImage buffered-image nil)]
          (.drawImage ^GraphicsContext cljfx/*ctx* img 0 0 width height))))))

(membrane.component/defui cljfx-video [{:keys [video]}]
  video)

(defn play-video [fname]
  (avclj/initialize!)
  (let [decoder
        (avclj/make-video-decoder fname)

        {:keys [num den] :as time-base} (-> decoder meta :time-base )
        fps (/ num den)

        {:keys [width height]} (meta decoder)

        buffered-image (bufimg/new-image height width :byte-bgr)
        draw-lock (Object.)

        app-state (atom {:img nil})
        start-time (System/currentTimeMillis)]

    (cljfx/run-app #'cljfx-video app-state)
    (future
      (try
       (loop[t 0]
         (let [start-frame-time (System/currentTimeMillis)
               frame-data (avclj/decode-frame! decoder)]
           (when frame-data
             (locking
                 (dtype/copy! (first (#'avclj/raw-frame->buffers frame-data))
                              buffered-image))
             (let [best-effort-timestamp (-> frame-data
                                             meta
                                             :best-effort-timestamp)
                   sleep-ms (- (* 1000 fps best-effort-timestamp)
                               (- (System/currentTimeMillis) start-time ))]
               (when (pos? sleep-ms)
                 (Thread/sleep sleep-ms))
              
               (swap! app-state assoc
                      :video (map->VideoViewCljfx
                              {:best-effort-timestamp best-effort-timestamp
                               :buffered-image buffered-image
                               :draw-lock draw-lock
                               :width width
                               :height height}))
               (recur best-effort-timestamp)))))
       (catch Exception e
         (println e))
       (finally
         (.close decoder))))))




(defn -main [fname]
  (play-video fname))
