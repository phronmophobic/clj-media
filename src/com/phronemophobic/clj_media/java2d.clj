(ns com.phronemophobic.clj-media.java2d
  (:require [membrane.ui :as ui]
            [membrane.java2d :as java2d]
            
            [avclj :as avclj]
            [tech.v3.datatype :as dtype]
            [tech.v3.libs.buffered-image :as bufimg]
            ))

(defn play-video [fname]
  (avclj/initialize!)
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
