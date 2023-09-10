(ns com.phronemophobic.clj-media.impl.filter.media
  (:refer-clojure :exclude [format])
  (:require [com.phronemophobic.clj-media.impl.filter.frame
             :as ff]
            [net.cgrand.xforms :as x]
            [clojure.java.io :as io]
            [clojure.core.protocols :as p]
            [com.phronemophobic.clj-media.impl.datafy
             :as datafy-media]
            [com.phronemophobic.clj-media.impl.model
             :as impl.model]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.audio :as audio]
            [com.phronemophobic.clj-media.impl.video :as video]
            [com.phronemophobic.clj-media.impl.util
             :refer [distinct-by
                     insert-last]
             :as media-util]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.IntByReference
   com.sun.jna.ptr.ByteByReference
   java.lang.ref.Cleaner
   com.sun.jna.Structure))

(raw/import-structs!)

;; All frames from a frame source should the same format
(defprotocol IFrameSource
  (-frames [this])
  (-format [this]))


(defprotocol IMediaSource
  (-media [this]
    "Returns a collection of IFrameSource"))

(defn media-source? [x]
  (satisfies? IMediaSource x))

(defrecord FrameSource [frames format]
  IFrameSource
  (-frames [this]
    frames)
  (-format [this]
    format))

(defn audio? [src]
  (= :media-type/audio
     (:media-type (-format src))))

(defn video? [src]
  (= :media-type/video
     (:media-type (-format src))))

(defn map-audio [f]
  (map (fn [src]
         (if (audio? (-format src))
           (->FrameSource (eduction
                           (map f)
                           (-frames src))
                          (-format src))
           src))))

(defn map-video [f]
  (map (fn [src]
         (if (video? (-format src))
           (->FrameSource (eduction
                           (map f)
                           (-frames src))
                          (-format src))
           src))))

(defrecord AdjustVolume [volumef media]
  IMediaSource
  (-media [this]
    (into []
          (map-audio ff/adjust-volume)
          media)))

(defn adjust-volume
  ([volumef]
   (->AdjustVolume volumef nil))
  ([volumef media]
   (->AdjustVolume volumef media)))


(defrecord Scale [sx sy media]
  IMediaSource
  (-media [this]
    (mapv (fn [src]
            (let [input-format (-format src)

                  ]
              (case (:media-type input-format)
                :media-type/audio src

                :media-type/video
                (let [output-format (-> input-format
                                        (update :width (fn [w]
                                                         (int (* w sx))))
                                        (update :height (fn [h]
                                                          (int (* h sy)))))]
                  (->FrameSource
                   (sequence
                    (video/swscale input-format output-format)
                    (-frames src))
                   output-format)))))
     (-media media))))

(defrecord MediaFile [fname]
  IMediaSource
  (-media [this]
    (let [format-context (av/open-context fname)
          err (avformat_find_stream_info format-context nil)
          _ (when (not (zero? err))
              (throw (ex-info "Could not find stream info."
                              {:error-code err})))
          num-streams (:nb_streams format-context)
          streams (.getPointerArray
                   (.readField format-context "streams")
                   0 num-streams)
          packets (sequence
                   av/read-frame
                   [format-context])]
      (into []
            (comp
             (map (fn [stream]
                    (let [stream+ (Structure/newInstance AVStreamByReference
                                                         stream)
                          stream-index (:index stream+)

                          codec-parameters (:codecpar stream+)
                          codec-id (:codec_id codec-parameters)
                          decoder (avcodec_find_decoder codec-id)
                          _ (when (nil? decoder)
                              (throw (ex-info "Could not find decoder"
                                              {:codec-id codec-id})))
                          decoder-context (avcodec_alloc_context3 (.getPointer decoder))

                          _ (when (nil? decoder-context)
                              (throw (ex-info "Could not allocate decoder"
                                              {})))
                          _ (doto decoder-context
                              (.writeField "time_base"
                                           (.readField stream+ "time_base")))

                          _ (avcodec_parameters_to_context decoder-context codec-parameters)
                          err (avcodec_open2 decoder-context decoder nil)
                          _ (when (neg? err)
                              (throw (Exception. "Could not open codec"
                                                 {:error-code err})))

                          format (merge {:time-base (:time_base stream+)}
                                        (av/codec-context-format decoder-context))

                          time-base (condp = (:codec_type decoder-context)
                                      AVMEDIA_TYPE_AUDIO [1 (:sample-rate format)]
                                      AVMEDIA_TYPE_VIDEO (let [tb (:time_base stream+)]
                                                           [(:num tb) (:den tb)]))
                          format (assoc format
                                        :time-base
                                        (av/->avrational (first time-base)
                                                     (second time-base)))

                          ;; do not share time base
                          ;; with format.
                          time-base (av/->avrational (first time-base)
                                                     (second time-base))
                          frames (sequence
                                  (comp (filter (fn [packet]
                                                  (= (:stream_index packet)
                                                     stream-index)))
                                        (insert-last nil)
                                        (av/decode-frame decoder-context)
                                        (map (fn [frame]
                                               (doto frame
                                                 (.writeField "time_base"
                                                              time-base)))))
                                  packets)]
                      (->FrameSource frames format)))))
            streams))))

(defn audio-pts []
  (comp
   (x/reductions
    (fn
      ([] [0 nil])
      ([[sum _] frame]
       [(+ sum (:nb_samples frame))
        (when frame
          (doto frame
            (.writeField "pts" sum)))])))
   (keep second)))

(defn auto-format-video [input-format codec]
  (let [pix-fmt (:pixel-format input-format)

        pix_fmts (:pix_fmts codec)
        _ (assert pix_fmts)
        pix-fmts (datafy-media/pointer-seq pix_fmts
                                           4 -1)

        input-format-acceptable?
        (some #{pix-fmt} pix-fmts)]
    (if input-format-acceptable?
      identity
      (let [output-pix-fmt (first pix-fmts)]
        (video/transcode-frame3 input-format output-pix-fmt)))))

(defn auto-format-audio [input-format codec]
  (let [{:keys [channel-layout sample-format sample-rate]} input-format

        sample_rates (:supported_samplerates codec)
        sample-rates (if sample_rates
                       (datafy-media/pointer-seq sample_rates 4 0)
                       #{44100})

        sample_fmts (:sample_fmts codec)
        _ (assert sample_fmts)
        sample-formats (datafy-media/pointer-seq sample_fmts
                                       4 -1)

        channel_layouts (:channel_layouts codec)
        
        ;; assume that empty channel_layouts
        ;; means any channel layout is supported?
        channel-layouts (when channel_layouts
                          (datafy-media/pointer-seq channel_layouts 8 0))

        input-format-acceptable?
        (and (some #{sample-rate} sample-rates)
             (some #{sample-format} sample-formats)
             (or
              (nil? channel-layouts)
              (some #{channel-layout} channel-layouts)))
        ]
    (if input-format-acceptable?
      (audio/resample2 input-format input-format)
      (let [sample-rate (if (some #{44100} sample-rates)
                          44100
                          (first sample-rates))

            sample-format (first sample-formats)
            channel-layout (if (some #{audio/AV_CH_LAYOUT_STEREO} channel-layouts)
                             audio/AV_CH_LAYOUT_STEREO
                             (or (first channel-layouts)
                                 channel-layout))
            output-format {:sample-rate sample-rate
                           :sample-format sample-format
                           :channel-layout channel-layout}]
        (audio/resample2 input-format output-format)))))

(defn auto-format [input-format codec]
  (condp = (:media-type input-format)
    :media-type/audio (auto-format-audio input-format codec)
    :media-type/video (auto-format-video input-format codec)))

(defn auto-format2 [input-format output-format]
  (case (:media-type input-format)
    :media-type/audio
    ;; always resample to obey frame size.
    (audio/resample2 input-format output-format)
    
    :media-type/video
    (if (= (:pixel-format input-format)
           (:pixel-format output-format))
      identity
      (video/transcode-frame3 input-format (:pixel-format output-format)))))

(defn default-channel-layout []
  (let [channel-layout (AVChannelLayoutByReference.)
        err (raw/av_channel_layout_from_string channel-layout "stereo")]
    (assert (zero? err))
    channel-layout))

(defn pick-output-format
  "Chooses an output format. Returns the input format if supported by the output format.
  Otherwise, tries to choose a good default"
  [output-format input-format]
  (let [;; assume that codecs can encode what they decode
        supported? (= 1
                      (avformat_query_codec
                       output-format
                       (-> input-format :codec :id)
                       raw/FF_COMPLIANCE_NORMAL))]
    (if supported?
      input-format
      ;; else choose a default based on what the
      ;;   output format supports
      (case (:media-type input-format)
        :media-type/video
        (let [codec-id (:video_codec output-format)
              codec (avcodec_find_encoder codec-id)

              pix-fmt (:pixel-format input-format)

              pix_fmts (:pix_fmts codec)
              _ (assert pix_fmts)
              pix-fmts (datafy-media/pointer-seq pix_fmts
                                       4 -1)

              pix-fmt (first pix-fmts)
              output-format (merge
                             input-format
                             {:pixel-format pix-fmt
                              :codec {:id codec-id}})]
          output-format)

        :media-type/audio
        (let [codec-id (:audio_codec output-format)
              codec (avcodec_find_encoder codec-id)

              sample_rates (:supported_samplerates codec)
              default-sample-rate (int 44100)
              sample-rates (if sample_rates
                             (datafy-media/pointer-seq sample_rates 4 0)
                             #{default-sample-rate})
              sample-rate (or (some #{default-sample-rate} sample-rates)
                              (first sample-rates))

              sample_fmts (:sample_fmts codec)
              _ (assert sample_fmts)
              sample-formats (datafy-media/pointer-seq sample_fmts
                                             4 -1)
              sample-format (first sample-formats)

              ch_layouts (:ch_layouts codec)
              
              ;; assume that empty channel_layouts
              ;; means any channel layout is supported?
              ch-layouts (when ch_layouts
                           (datafy-media/avchannellayout-seq ch_layouts))
              
              channel-layout (if ch-layouts
                               (letfn [(find-layout [needle]
                                         (some (fn [layout]
                                                 (when (zero?
                                                        (av_channel_layout_compare
                                                         (.getPointer layout)
                                                         (.getPointer needle)))
                                                   layout))
                                               ch-layouts))]
                                 (or (find-layout (:ch-layout input-format))
                                     (find-layout (default-channel-layout))
                                     (first ch-layouts)))
                               (default-channel-layout))
              

              output-format {:sample-rate sample-rate
                             :sample-format sample-format
                             :ch-layout channel-layout
                             :media-type (:media-type input-format)
                             :codec {:id codec-id}}]
          output-format)))))


(defn write!
  ([src fname]
   (write! src fname nil))
  ([src fname opts]
   ;; create a stream for each input
   (let [output-format-context (av/open-output-context fname)

         input-streams (-media src)

         output-streams
         (into []
               (map-indexed
                (fn [i src]
                  (let [input-format (-format src)

                        output-format
                        (cond
                          (and (audio? src)
                               (:audio-format opts))
                          (let [{:keys [audio-format]} opts]
                            (assert (:channel-layout audio-format) "Audio format must have `:channel-layout.`")
                            (assert (:sample-format audio-format) "Audio format must have `:sample-audio-format.`")
                            (assert (:sample-rate audio-format) "Audio format must have `:sample-rate`.")
                            (assert (:id (:codec audio-format)) "Audio format must have `{:codec {:id codec-id}}`.")
                            (merge
                             (select-keys input-format
                                          [:time-base])
                             (datafy-media/map->format audio-format
                                                       :media-type/audio)))

                          (and (video? src)
                               (:video-format opts))
                          (let [video-format (:video-format opts)]
                            (assert (:pixel-format video-format) "Video format must have `:pixel-format`.")
                            (assert (:id (:codec video-format)) "Video format must have `{:codec {:id codec-id}}`.")
                            (merge
                             (select-keys input-format
                                          [:width :height :time-base])
                             (datafy-media/map->format video-format
                                                       :media-type/video)))

                          :else
                          (pick-output-format
                           (Structure/newInstance AVOutputFormatByReference
                                                  (:oformat output-format-context))
                           input-format))

                        encoder-context (av/encoder-context output-format-context output-format)

                        stream (av/add-stream output-format-context encoder-context)
                        stream-index (:index stream)

                        output-format
                        (if (audio? src)
                          (assoc output-format
                                 :frame-size (:frame_size encoder-context))
                          output-format)]
                    (lazy-seq
                     (when (audio? src)
                       (let [{:keys [num den]} (:time_base stream)]
                         (when (or (not= (:sample-rate output-format)
                                         den)
                                   (not= 1 num))
                           (throw (Exception. "Unexpected time base for audio. must be 1/sample-rate.")))))
                     (sequence
                      (comp (insert-last nil)
                            (auto-format2 input-format
                                          output-format)
                            (if (audio? src)
                              (audio-pts)
                              identity)
                            (insert-last nil)
                            (av/encode-frame encoder-context)
                            (if (video? src)
                              (map (fn [packet]
                                     (let [{:keys [duration pts]} packet]
                                       (.writeField packet "time_base" (:time_base stream))
                                       (av_packet_rescale_ts packet
                                                               (:time-base input-format)
                                                               (:time_base stream)))
                                     packet))
                              ;; we set the pts via audio-pts already
                              identity)
                            (map (fn [packet]
                                   (doto packet
                                     (.writeField "stream_index" stream-index)))))
                      (-frames src))))))
               input-streams)]


     ;; need to write header before creating streams
     ;; sets time_base in streams
     (let [err (avformat_write_header output-format-context nil)]
       (when (neg? err)
         (throw (Exception.)))
       err)

     (transduce
      identity
      (av/write-packet2 output-format-context)
      (apply media-util/interleave-all-by
             (fn [packet]
               (let [{{:keys [num den]} :time_base
                      :keys [pts]} packet]
                 (/ (* pts
                       num)
                    den)))
             output-streams))

     (let [err (av_write_trailer output-format-context)]
       (when (neg? err)
         (throw (Exception.)))
       err)

     (avio_closep (doto (PointerByReference.)
                    (.setValue (:pb output-format-context))))
     (avformat_free_context output-format-context))))

(defn filter-audio [media]
  (reify
    IMediaSource
    (-media [this]
      (filterv (fn [src]
                 (= :media-type/audio
                    (:media-type (-format src))))
               (-media media)))))

(defn filter-video [media]
  (reify
    IMediaSource
    (-media [this]
      (filterv (fn [src]
                 (= :media-type/video
                    (:media-type (-format src))))
               (-media media)))))

(defrecord Union [medias]
  IMediaSource
  (-media [this]
    (into []
          (comp (mapcat -media)
                (distinct-by #(System/identityHashCode %)))
          medias)))

(defn union [& medias]
  (->Union medias))

(defn ^:private ->url-str [fname]
  (str "file://" (.getCanonicalPath (io/file fname))))

(defn -main [& args]
  (let [outname (or (first args)
                    "test.mov")
        default-input (->url-str "/Users/adrian/workspace/eddie/vids/guitar_notes.mp4")
        [inname outname]
        (case (count args)
          0 [default-input
             "test.mp4"]
          1 [default-input
             (first args)]

          ;; else
          [(str "file://" (.getCanonicalPath (io/file (first args))))
           (second args)]
          )]
    (write! (-> (->MediaFile
                 ;;"file:///Users/adrian/workspace/apple-data/iCloud Photos Part 1 of 3/Photos/IMG_0454.mp4"
                 inname
                 #_"file:///Users/adrian/workspace/blog/blog/resources/public/mairio/videos/mairio-level-5-3.mp4"
                 )
                ;; (filter-video)
                ;; (filter-audio)
                (->> (->Scale 0.25 0.5))
                )
            outname)

    #_(write! (-> (->Union
                   (->MediaFile
                    (->url-str "./examples/codetogif/this-is-fine-but-with-earthquakes.gif"))
                   (->MediaFile
                    (->url-str "./examples/avencode/bar.mp3")))
                  ;; (filter-video)
                  ;; (filter-audio)
                  )
              "union.mp4")))

(deftype FramesReducible [media stream audio-format video-format]
  clojure.lang.IReduceInit
  (reduce [_ f init]
    (let [stream
          (case stream
            :audio (some #(when (audio? %) %) (-media media))
            :video (some #(when (video? %) %) (-media media))
            ;; else
            (nth (-media media) stream))
          _ (when (not stream)
              (throw (ex-info "Stream not found."
                              {:media media
                               :stream stream})))
          input-format (-format stream)
          formatter
          (case (:media-type input-format)
            :media-type/audio (if audio-format
                                (comp
                                 (insert-last nil)
                                 (audio/resample2 input-format
                                                  (datafy-media/map->format input-format
                                                                            :media-type/audio)))
                                identity)
            :media-type/video (if video-format
                                (comp
                                 (insert-last nil)
                                 (video/transcode-frame3
                                  input-format
                                  (datafy-media/kw->pixel-format
                                   (:pixel-format video-format))))
                                identity))
          ->frame
          (case (:media-type input-format)
            :media-type/audio impl.model/->AudioFrame
            :media-type/video impl.model/->VideoFrame)]
      (transduce
       (comp formatter
             (map ->frame))
       (completing f)
       init
       (-frames stream)))))

(defn frames-reducible
  ([media stream]
   (frames-reducible media stream nil))
  ([media stream {:keys [audio-format
                         video-format]
                  :as opts}]
   (->FramesReducible media
                      stream
                      audio-format
                      video-format)))
