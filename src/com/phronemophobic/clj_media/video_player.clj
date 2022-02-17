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

(defn video-thread [fname repaint!]
  (avclj/initialize!)
  (let [control-ch (async/chan (async/sliding-buffer 1))]
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
                (let [op (:op val)
                      op (if (= :toggle-play op)
                           (if playing?
                             :pause
                             :play)
                           op)]
                  (case op
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
             nil
             #_{:n -1
                :resource nil
                :width width
                :height height
                :chan nil
                :draw-lock nil})

            (when decoder
              (.close decoder))

            ;; cleanup
            (#'media/skia_cleanup resource)))))

    ;; return control ch
    control-ch))

(defeffect ::init-video-resources [fname $video-resources]
  (dispatch! :update
             $video-resources
             (fn [video-resources]
               (if (:chan video-resources)
                 video-resources
                 (assoc video-resources
                        :chan
                        (delay
                          (video-thread fname
                                        (fn [video-resources]
                                          (if video-resources
                                            (dispatch! :update $video-resources merge video-resources)
                                            (dispatch! :delete $video-resources))
                                          (repaint!))))))))
  nil)

(defeffect ::pause [fname $video-resources]
  (dispatch! ::init-video-resources fname $video-resources)
  (let [ch (-> (dispatch! :get $video-resources)
               :chan
               deref)]
    (async/put! ch {:op :pause})))

(defeffect ::stop [$video-resources]
  (when-let [ch (some-> (dispatch! :get $video-resources)
                        :chan
                        deref)]
    (async/put! ch {:op :stop})))

(defeffect ::play [fname $video-resources]
  (dispatch! ::init-video-resources fname $video-resources)
  (let [ch (-> (dispatch! :get $video-resources)
               :chan
               deref)]
    (async/put! ch {:op :play})))

(defeffect ::toggle-play [fname $video-resources]
  (dispatch! ::init-video-resources fname $video-resources)
  (let [ch (-> (dispatch! :get $video-resources)
               :chan
               deref)]
    (async/put! ch {:op :toggle-play})))

(defeffect ::seek-to [fname $video-resources pct]
  (dispatch! ::init-video-resources fname $video-resources)
  (let [ch (-> (dispatch! :get $video-resources)
               :chan
               deref)]
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
                             mouse-down?
                             width
                             height]}]
  (ui/vertical-layout
   (if (and video-resources
            (:width video-resources)
            (:height video-resources))
     (let [vid (media/map->VideoView
                video-resources)
           scaled-vid (aspect-fit vid
                                  width height)
           [w h] (ui/bounds scaled-vid)]
       [scaled-vid
        (let [mdown? (get extra :mdown?)]
          (ui/on
           :mouse-move
           (fn [[x _y]]
             (when mdown?
               [[::seek-to fname $video-resources (/ x w)]]))
           :mouse-up
           (fn [[x _y]]
             [[:set $mdown? false]
              [::seek-to fname $video-resources (/ x w)]
              [::play fname $video-resources][::play fname $video-resources]])
           :mouse-down
           (fn [[x _y]]
             [[::pause fname $video-resources]
              [::seek-to fname $video-resources (/ x w)]
              [:set $mdown? true]
              ])
           (ui/filled-rectangle [0.5 0.5 0.5 0.5]
                                w 40)))])
     (ui/label "press play to see something"))
   (ui/horizontal-layout
    (basic/button {:text "Play"
                   :on-click
                   (fn []
                     [[::play fname $video-resources]])})
    (basic/button {:text "Pause"
                   :on-click
                   (fn []
                     [[::pause fname $video-resources]])})))
  )





