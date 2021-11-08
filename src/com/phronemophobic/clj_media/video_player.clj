(ns com.phronemophobic.clj-media.video-player
  (:require [membrane.ui :as ui]
            [membrane.basic-components :as basic]
            [avclj.avformat :as avformat]
            [avclj.avutil :as avutil]
            [clojure.core.async :as async
             :refer [<!! >!! <! >! go]]
            [avclj :as avclj]
            [membrane.skia :as skia]
            [com.phronemophobic.clj-media.skia
             :as media]
            [membrane.component :as component
             :refer [defui defeffect]])
  (:import com.sun.jna.Pointer
           com.sun.jna.Memory))

(defn repaint! []
  (#'skia/glfw-post-empty-event))

(defn video-thread [fname key repaint!]
  (avclj/initialize!)
  (let [control-ch (async/chan)]
    (async/thread


      (let [decoder
            (avclj/make-video-decoder fname
                                      {:output-pixfmt "AV_PIX_FMT_BGRA"})

            ;; assume static width, height, time base
            ;; which is not always true.
            ;; file can change while we're reading it
            ;; or after we reload when we reach EOF.
            {:keys [width height]} (meta decoder)
            {:keys [num den] :as time-base} (-> decoder meta :time-base )
            fps (/ num den)

            decoder-map (avclj/->map decoder)
            duration (-> decoder-map
                         :fmt-ctx
                         :duration)
            ;; _ (prn (format " duration %.2f" (float (/ duration avutil/AV_TIME_BASE))))

            buffer (Memory. (* height width 4))
            resource (media/skia-direct-bgra8888-buffer buffer width height (* width 4))
            draw-lock (Object.)]
        (try

          (loop [;; always want a unique number
                 ;; to break caching
                 counter 0
                 start-time nil
                 decoder decoder
                 play-timeout-ch nil
                 seek-ts nil
                 playing? false]
            (let [[val port] (async/alts!! (if play-timeout-ch
                                            [control-ch play-timeout-ch]
                                            [control-ch])
                                          :priority true)]
              (if (= port control-ch)
                (do
                  (prn "control: " val)
                  (case (:op val)
                    :play
                    (if playing?
                      ;; we're already playing
                      (recur counter
                             start-time
                             decoder
                             play-timeout-ch
                             seek-ts
                             playing?)
                      ;; start playing
                      (recur counter
                             nil
                             decoder
                             (doto (async/promise-chan)
                               (>!! false))
                             seek-ts
                             true))

                    :pause
                    (recur counter
                           start-time
                           decoder
                           nil
                           seek-ts
                           false)

                    :seek
                    (let [seek-percent (:seek-percent val)
                          
                          seek-ts (int (* duration seek-percent))]
                      (recur counter
                             nil
                             decoder
                             (doto (async/promise-chan)
                               (>!! false))
                             seek-ts
                             playing?))
                    

                    :stop
                    ;; we're done!
                    nil))
                ;; if val is true, draw frame
                ;; next, start decoding next frame
                (do

                  (when val
                    (locking draw-lock
                      (let [frame-data val]
                        (media/skia-bgra8888-draw resource
                                                  (Pointer. (:data frame-data))
                                                  width
                                                  height
                                                  (:linesize frame-data))))
                    (repaint!
                     {:n counter
                      :resource resource
                      :width width
                      :height height
                      :draw-lock draw-lock}))

                  (let [decoder (if decoder
                                  decoder
                                  ;; initialize
                                  (let [decoder (avclj/make-video-decoder fname
                                                                          {:output-pixfmt "AV_PIX_FMT_BGRA"})
                                        {_width :width _height :height} (meta decoder)
                                        {:keys [num den] :as time-base} (-> decoder meta :time-base )
                                        _fps (/ num den)

                                        decoder-map (avclj/->map decoder)
                                        _duration (-> decoder-map
                                                     :fmt-ctx
                                                     :duration)
]
                                    (assert (= _width width) "Video width changed unexpectedly!!")
                                    (assert (= _height height) "Video height changed unexpectedly!")
                                    (assert (= _fps fps) "Video frame rate changed unexpectedly!")
                                    (assert (= _duration duration) "Video duration changed unexpectedly!")
                                    decoder))]

                    ;; seek, if requested
                    (when seek-ts
                      (let [flags 0 ;; avformat/AVSEEK_FLAG_ANY
                            stream-idx -1
                            min-ts (Long/MIN_VALUE)
                            max-ts (Long/MAX_VALUE)
                            decoder-map (avclj/->map decoder)]

                        (avformat/avformat_seek_file (:fmt-ctx decoder-map)
                                                     stream-idx
                                                     min-ts
                                                     seek-ts
                                                     max-ts
                                                     flags)
                        (loop [frame-data (avclj/decode-frame! decoder)]
                          (when frame-data
                            (let [best-effort-timestamp (-> frame-data
                                                            meta
                                                            :best-effort-timestamp)]
                             (if (< (* fps best-effort-timestamp)
                                    (/ seek-ts avutil/AV_TIME_BASE))
                               (recur (avclj/decode-frame! decoder))))))))


                    ;; show-next-frame
                    (if-not (or seek-ts
                                playing?)
                      (recur counter
                             start-time
                             decoder
                             nil
                             nil
                             playing?)
                      (let [frame-data (avclj/decode-frame! decoder)]
                        (if-not frame-data
                          (recur counter
                                 start-time
                                 nil
                                 nil
                                 nil
                                 false)
                          (let [best-effort-timestamp (-> frame-data
                                                          meta
                                                          :best-effort-timestamp)best-effort-timestamp (-> frame-data
                                                          meta
                                                          :best-effort-timestamp)best-effort-timestamp (-> frame-data
                                                          meta
                                                          :best-effort-timestamp)
                                sleep-ms (if start-time
                                           (- (* 1000 fps best-effort-timestamp)
                                              (- (System/currentTimeMillis) start-time))
                                           0)]
                            (recur (inc counter)
                                   (cond
                                     seek-ts nil
                                     start-time start-time
                                     :else (- (System/currentTimeMillis)
                                              (* 1000 fps best-effort-timestamp)))
                                   
                                   decoder
                                   (if (pos? sleep-ms)
                                     (go
                                       (<! (async/timeout sleep-ms))
                                       frame-data)
                                     (doto (async/promise-chan)
                                       (>!! frame-data)))
                                   nil
                                   playing?))))))))))
          (catch Exception e
            (prn e))
          (finally
            (repaint!
             {:n -1
              :resource nil
              :width width
              :height height
              :draw-lock draw-lock})

            (when decoder
              (.close decoder))

            ;; cleanup
            (#'media/skia_cleanup resource)))))

    ;; return control ch
    control-ch))

(defonce vthreads (atom {}))

(defn get-vthread [fname key repaint!]
  (let [k [fname key]
        m (swap! vthreads
                 (fn [m]
                   (if-let [x (get m k)]
                     m
                     (assoc m k
                            (delay
                              (video-thread fname key repaint!))))))]
    @(get m k)))

(defeffect ::pause [fname key $video-resources]
  (let [ch (get-vthread fname key
                        (fn [video-resources]
                          (dispatch! :set $video-resources video-resources)
                          (repaint!)))]
    (async/put! ch {:op :pause})))

(defeffect ::play [fname key $video-resources]
  (let [ch (get-vthread fname key
                        (fn [video-resources]
                          (dispatch! :set $video-resources video-resources)
                          (repaint!)))]
    (async/put! ch {:op :play})))



(defonce seek-diffs (atom [] ))

(defeffect ::seek-to [fname key $video-resources pct]
  (let [ch (get-vthread fname key
                        (fn [video-resources]
                          (dispatch! :set $video-resources video-resources)
                          (repaint!)))]
    (async/put! ch {:op :seek
                    :seek-percent pct})))

(defn aspect-fit [elem width height]
  (let [[w h] (ui/bounds elem)
        elem-ratio (/ w h)
        ratio (/ width height)]
    (if (> elem-ratio ratio)
      (let [scale (/ width w)]
        (ui/scale scale scale
                  elem))
      (let [scale (/ height h)]
        (ui/scale scale scale
                  elem)))))

(defui video-player [{:keys [fname
                             video-resources
                             key
                             width
                             height]}]
  (ui/vertical-layout
   (if video-resources
     (let [vid (media/map->VideoView
                video-resources)
           scaled-vid (aspect-fit vid
                                  width height)
           [w h] (ui/bounds scaled-vid)]
       [scaled-vid
        (ui/on
         :mouse-down
         (fn [[x _y]]
           [[::seek-to fname key $video-resources (/ x w)]])
         (ui/filled-rectangle [0.5 0.5 0.5 0.5]
                              w 40))])
     (ui/label "press play to see something"))
   (ui/horizontal-layout
    (basic/button {:text "Play"
                   :on-click
                   (fn []
                     [[::play fname key $video-resources]])})
    (basic/button {:text "Pause"
                   :on-click
                   (fn []
                     [[::pause fname key $video-resources]])})))
  )





