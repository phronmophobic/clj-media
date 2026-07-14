(ns com.phronemophobic.clj-media.impl.skia
  (:require [membrane.ui :as ui]
            [membrane.component :as component
             :refer [defui defeffect]]
            [membrane.skia :as skia]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [com.phronemophobic.clj-media.impl.datafy :as media.datafy]
            [com.phronemophobic.clj-media :as clj-media]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.video :as video]
            [com.phronemophobic.clj-media.impl.audio :as audio]
            [com.phronemophobic.clj-media.impl.raw :as raw]
            [com.phronemophobic.clj-media.model :as mm]
            [clojure.java.io :as io]
            ;; [tech.v3.tensor :as dtt]
            ;; [tech.v3.datatype.ffi :as dt-ffi]
            ;; [tech.v3.datatype :as dtype]
            ;; [tech.v3.libs.buffered-image :as bufimg]
            )
  (:import com.sun.jna.Pointer
           com.sun.jna.Memory
           (javax.sound.sampled AudioFormat
                                AudioFormat$Encoding
                                DataLine$Info
                                SourceDataLine
                                AudioSystem)))


(defn ^:private long->pointer [n]
  (tech.v3.datatype.ffi.Pointer. n))

(def skialib @#'skia/membraneskialib)

(skia/defc skia_bgra8888_draw skialib Void/TYPE [skia-resource buffer width height row-bytes])
(defn skia-bgra8888-draw [resource buffer width height row-bytes]
  (skia_bgra8888_draw resource buffer (int width) (int height) (int row-bytes)))

(skia/defc skia_direct_bgra8888_buffer skialib Pointer [buf width height row-bytes])
(defn skia-direct-bgra8888-buffer [buf width height row-bytes]
  (skia_direct_bgra8888_buffer buf (int width) (int height) (int row-bytes)))

(skia/defc skia_cleanup skialib Void/TYPE [skia-resource])

(skia/defc skia_draw_surface skialib Void/TYPE [destination source])

(defrecord VideoView [n resource width height draw-lock]
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




(defn frame->pixformat [frame]
  (let [{:keys [width height linesize data format]} frame
        _ (when (neg? linesize)
            (throw (ex-info "Only frames with positive line size supported."
                            {:frame frame
                             :linesize linesize})))
        buf-size (* linesize height)
        buf (dt/->byte-array
             (native-buffer/wrap-address (first data)
                                         buf-size))
        skia-pixel-format (case (:format frame)
                            ;; (media.datafy/kw->pixel-format :pixel-format/bgra)
                            28 membrane.skia/kBGRA_8888_SkColorType
                            
                            ;; else
                            (throw (ex-info "Unsupported pixel format"
                                            {:pixel-format (media.datafy/pixel-format->kw (:format frame))
                                             :frame frame})))]
    (skia/pixmap (:pts frame)
                 (dt/->byte-array
                  (native-buffer/wrap-address (first data)
                                              buf-size))
                 width height skia-pixel-format
                 membrane.skia/kOpaque_SkAlphaType 
                 linesize)))

(defn draw-frame [resource frame]
  (let [width (.readField frame "width")
        height (.readField frame "height")
        linesize (-> frame
                     (.readField "linesize")
                     (nth 0))
        buf-ptr (-> frame
                    (.readField "data")
                    (nth 0)
                    (.getPointer))]
    (skia-bgra8888-draw resource
                        buf-ptr
                        width
                        height
                        linesize)))

(defn play-video
  "Open a window and play the video found at `fname`."
  [fname dispatch! $video-state]
  (let [

        input-context (av/open-context (.getAbsolutePath (io/file fname)))
        decoder-context (av/find-decoder-context :video input-context)

        ;; {:keys [width height]} (meta decoder)
        width (.readField decoder-context "width")
        height (.readField decoder-context "height")

        time-base (.readField decoder-context "time_base")
        num (.readField time-base "num")
        den (.readField time-base "den")
        fps (/ num den)

        xform (comp (av/decode-frame decoder-context)
                    (video/transcode-frame decoder-context raw/AV_PIX_FMT_BGRA))
        rf (xform (completing
                   (fn [_ frame]
                     frame)))


        buffer (Memory. (* height width 4))
        resource (skia-direct-bgra8888-buffer buffer width height (* width 4))
        draw-lock (Object.)
        video-view (map->VideoView
                    {:n 0
                     :resource resource
                     :buffer buffer
                     :width width
                     :height height
                     :draw-lock draw-lock})

        audio-format (audio/default-stereo-format)
        info (DataLine$Info. SourceDataLine
                             audio-format)
         source-data-line (^SourceDataLine AudioSystem/getLine info)
         source-data-line (doto ^SourceDataLine source-data-line
                            (.open audio-format)
                            (.start))
        running? (atom true)]
    (dispatch! :set $video-state {:view video-view
                                  :pause (fn []
                                           (reset! running? false))
                                  :cleanup (fn []
                                             (reset! running? false))})

    ;; audio
    (future
      (try
        (let [media (clj-media/file fname)
              bs (volatile! nil)]
          (transduce
           (comp (take-while (fn [_]
                               @running?))
                 (map com.phronemophobic.clj-media.impl.model/raw-frame)
                 (map (fn [frame]
                        (let [buf @bs
                              linesize (-> frame :linesize first)
                              buf (if (or (nil? buf)
                                          (< (alength buf) linesize))
                                    (vreset! bs (byte-array linesize))
                                    buf)]
                          (audio/frame->byte-array frame buf)))))
           (fn
             ([])
             ([result]
              (.drain source-data-line)
              (.close source-data-line)
              result)
             ([result buf]
              (.write source-data-line buf 0 (alength buf))
              result))
           nil
           (clj-media/frames media :audio {:format (clj-media/audio-format
                                                    {:channel-layout "stereo"
                                                     :sample-rate 44100
                                                     :sample-format :sample-format/s16})})))
        (catch Throwable e
          (prn e))
        (finally
          (println "quit audio"))))


    (future
      (try

        (loop []
          (when @running?
            (let [frame (loop []
                          (let [packet (av/next-packet input-context)]
                            (when (and @running? packet)
                              (let [frame (rf nil packet)]
                                (if frame
                                  frame
                                  (recur))))))]
              (when frame
                (let [best-effort-timestamp (.readField frame "best_effort_timestamp")
                      sleep-ms (- (* 1000 fps best-effort-timestamp)
                                  (quot (.getMicrosecondPosition source-data-line)
                                        1000))]

                  (when (pos? sleep-ms)
                    (Thread/sleep (long sleep-ms)))

                  (locking draw-lock
                    (draw-frame resource frame))

                  (when @running?
                    (dispatch! :update $video-state
                               (fn [vs]
                                 (when vs
                                   (update-in vs [:view :n] inc))))
                    (dispatch! :repaint!))

                  (recur))))))
        (catch Exception e
          (println e))
        (finally
          (skia_cleanup resource)
          (println "quit video"))))))

(defui video-player [{:keys [path video-state]}]
  (let [video-view (:view video-state)]
    video-view))

(defeffect ::play [{:keys [$video-state path]}]
  (future
    (play-video path dispatch! $video-state)))
(defeffect ::pause [{:keys [video-state]}]
  (when-let [pause (:pause video-state)]
    (pause)))
(defeffect ::cleanup [{:keys [video-state]}]
  (when-let [cleanup (:cleanup video-state)]
    (cleanup)))

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

#_(defn write-video
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


#_(defn write-gif
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
  #_(write-video "my-movie.mp4"
               (map (fn [i]
                      (ui/padding 10 (ui/label (str "frame: " i))))
                    (range 120))
               100 100)
  )

(comment

  (def input-context (av/open-context (.getAbsolutePath (io/file "foo.mp4"))))

  (def decoder-context (av/find-decoder-context :audio input-context))

  (av/codec-context-format decoder-context)
  (audio/resample2 (av/codec-context-format decoder-context)
                   ;;{:codec }
                   )
  

  ,)


(defn test-main [& args]
  (let [uuid (random-uuid)
        next-int (let [atm (atom 0)]
                   (fn []
                     (swap! atm inc)))]
    (time
     (run! (fn [frame]
            (prn (:pts frame))
            
            (let [linesize (-> frame :linesize first)
                  width (:width frame)
                  height (:height frame)
                  buf-size (* linesize height)
                  i (next-int)]
              (membrane.skia/save-image
               (str "frames/frame" i ".png")
               (skia/pixmap [uuid i]
                            (dt/->byte-array
                             (native-buffer/wrap-address (first (:data frame))
                                                         buf-size))
                            width height membrane.skia/kBGRA_8888_SkColorType membrane.skia/kOpaque_SkAlphaType 
                            linesize))))
           (frames-reducible 
            {:type :avfilter
             :output-format {:media-type :media-type/video
                             :pixel-format :pixel-format/bgra}
             :filter-name "hstack"
             :inputs [{:type :concat
                       :inputs [{:type :avfilter
                                 :filter-name "vflip"
                                 :inputs [{:file media-fname
                                           :stream :video
                                           :type :file}]}
                                {:file media-fname
                                 :stream :video
                                 :type :file}]}
                      {:type :concat
                       :inputs [{:type :avfilter
                                 :filter-name "gblur"
                                 :opts {:sigma 20}
                                 :inputs [{:file media-fname
                                           :stream :video
                                           :type :file}]}
                                {:file media-fname
                                 :stream :video
                                 :type :file}]}]}
            
            
            #_{:type :avfilter
               :output-format {:media-type :media-type/video
                               :pixel-format :pixel-format/bgra}
               :filter-name "null"
               :inputs [{:type :avfilter
                         :filter-name "gblur"
                         :opts {:sigma 100}
                         :inputs [{:file media-fname
                                   :start-timestamp (+ (* 2 3600) (* 60 29) 0)
                                   :stream :video
                                   :type :file}]}]}))))
  (prn "done")
  (Thread/sleep (long 5e3)))
