(ns com.phronemophobic.clj-media.impl.filter.media
  (:refer-clojure :exclude [format])
  (:require [com.phronemophobic.clj-media.impl.filter.frame
             :as ff]
            [net.cgrand.xforms :as x]
            [clojure.java.io :as io]
            [clojure.core.protocols :as p]
            [clojure.string :as str]
            [com.phronemophobic.clj-media.impl.datafy
             :as datafy-media]
            [com.phronemophobic.clj-media.impl.model
             :as impl.model]
            [clojure.set :as set]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.audio :as audio]
            [com.phronemophobic.clj-media.impl.video :as video]
            loom.alg
            loom.graph
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

(defprotocol IComputeNode
  (configure! [this input-ports]))

;; All frames from a frame source should the same format
(defprotocol IFrameSource
  (-frame-inputs [this])
  (-frame-outputs [this])
  (-frames [this])
  (-format [this]))

(defprotocol ITransformName
  (-transform-name [this]))


(defprotocol IMediaSource
  (-media [this]
    "Returns a collection of IFrameSource")
  (-media2 [this]
    "Returns a collection of IFrameSource")
  (-media-inputs [this]))

(defn media-source? [x]
  (satisfies? IMediaSource x))

(defrecord FrameSource [frames format inputs transform-name]
  ITransformName
  (-transform-name [this]
    transform-name)
  IFrameSource
  (-frame-inputs [this]
    inputs)
  (-frames [this]
    frames)
  (-format [this]
    format))

(defn frame-source [frames format inputs transform-name]
  (->FrameSource
   frames
   format
   inputs
   transform-name))

(defn audio? [src]
  (= :media-type/audio
     (:media-type (-format src))))

(defn video? [src]
  (= :media-type/video
     (:media-type (-format src))))

(defn map-audio [f]
  (map (fn [src]
         (if (audio? (-format src))
           (frame-source (eduction
                           (map f)
                           (-frames src))
                          (-format src)
                          [src]
                          "map-audio")
           src))))

(defn map-video [f]
  (map (fn [src]
         (if (video? (-format src))
           (frame-source (eduction
                           (map f)
                           (-frames src))
                          (-format src)
                          [src]
                          "map-video")
           src))))


(defrecord MediaFile [fname]
  IComputeNode
  (configure! [this _input-ports]
    (let [format-context (av/open-context fname)
          err (avformat_find_stream_info format-context nil)
          _ (when (not (zero? err))
              (throw (ex-info "Could not find stream info."
                              {:error-code err})))
          num-streams (:nb_streams format-context)
          streams (.getPointerArray
                   (.readField format-context "streams")
                   0 num-streams)
          packet-ports
          (into []
                (comp
                 (map (fn [stream]
                        (let [stream+ (Structure/newInstance AVStreamByReference
                                                             stream)
                              stream-index (:index stream+)]
                          {:id [this :packets stream-index]
                           :format ::packet}))))
                streams)
          decoders
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
                              ]
                          {:decoder-context decoder-context
                           :frame (av_frame_alloc)
                           :stream-index stream-index
                           :id [this stream-index]
                           :format format}))))
                streams)
          frame-ports
          (into []
                (map #(select-keys % [:id :format]))
                decoders)
          subscriptions
          (into {}
                (map (fn [{:keys [decoder-context id frame stream-index]}]
                       [[this :packets stream-index]
                        (fn sub
                          ([send]
                           ;; flush
                           (sub send nil)
                           (send id))
                          ([send packet]
                           (let [err (avcodec_send_packet decoder-context packet)
                                 done?
                                 (if (and (not (zero? err))
                                          (not= err -22)
                                          (av/eagain? err))
                                   {:error-code err
                                    :type :send-packet-failure}
                                   (let [err (avcodec_receive_frame decoder-context frame)
                                         done?
                                         (cond
                                           (or (zero? err)
                                               (av/einvalid? err))
                                           (do
                                             (send id frame)
                                             nil)

                                           (av/eagain? err)
                                           nil

                                           (av/eof? err)
                                           true

                                           ;; some other error
                                           :else
                                           {:error-code err
                                            :error-msg (av/error->str err)
                                            :type :decode-error})]
                                     done?))]
                             (av_frame_unref frame)
                             done?)))]))
                decoders)

          packet (av_packet_alloc)
          pumps [(fn [send]
                   ;; need to signal done, continue, or error
                   (let [err (av_read_frame format-context packet)
                         done? (cond
                                 (zero? err)
                                 (do
                                   (send [this :packets (:stream_index packet)]
                                         packet)
                                   false)

                                 (av/eof? err)
                                 (do (send [this :packets (:stream_index packet)]
                                           packet)
                                     ;; flush all
                                     (doseq [i (range num-streams)]
                                       (send [this :packets i]))
                                     true)

                                 :else ;; some other error
                                 {:error-code err
                                  :type :read-error})]
                     (av_packet_unref packet)
                     done?))]
          cleaners [(fn []
                      (av_packet_free
                       (PointerByReference. (.getPointer packet))))]]
      {:ports (into []
                    cat
                    [frame-ports
                     ;; packet-ports
                     ])
       :subscriptions subscriptions
       :pumps pumps
       :cleaners cleaners}))
  IMediaSource
  (-media-inputs [this]
    nil)
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
                      (frame-source frames format
                                    nil
                                    (str fname ": " stream-index " "
                                         (condp = (:codec_type decoder-context)
                                           AVMEDIA_TYPE_AUDIO "audio"
                                           AVMEDIA_TYPE_VIDEO "video")))))))
            streams))))
(defn media-file [f]
  (->MediaFile
   (.getCanonicalPath (io/as-file f))))

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
  [fname output-format input-format]
  (let [input-codec-id (-> input-format :codec :id)
        ;; assume that codecs can encode what they decode
        supported? (and
                    input-codec-id
                    (= 1
                       (avformat_query_codec
                        output-format
                        input-codec-id
                        raw/FF_COMPLIANCE_NORMAL)))]
    (if supported?
      input-format
      ;; else choose a default based on what the
      ;;   output format supports
      (case (:media-type input-format)
        :media-type/video
        (let [codec-id
              (if (str/ends-with? fname ".mp4")
                ;; default to h264 for .mp4
                27
                (:video_codec output-format))

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


(defrecord FileWriter [opts fname media]
  IMediaSource
  (-media-inputs [this]
    [media])
  IComputeNode
    (configure! [this input-ports]
      (let [output-format-context (av/open-output-context fname)

            encoders
            (into []
                  (comp
                   (filter #(not= ::packet
                                  (:format %)))
                   (map (fn [port]
                          (let [input-format (:format port)

                                output-format
                                (cond
                                  (and (= :media-type/audio
                                          (:media-type input-format))
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

                                  (and (= :media-type/video
                                          (:media-type input-format))
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
                                   fname
                                   (Structure/newInstance AVOutputFormatByReference
                                                          (:oformat output-format-context))
                                   input-format))

                                encoder-context (av/encoder-context output-format)

                                stream (av/add-stream output-format-context encoder-context)
                                stream-index (:index stream)

                                output-format
                                (if (= :media-type/audio
                                       (:media-type input-format))
                                  (assoc output-format
                                         :frame-size (:frame_size encoder-context))
                                  output-format)

                                port-id [(:id port) :packets]]
                            {:encoder-context encoder-context
                             :stream stream
                             :input-port port
                             :port-id port-id
                             :stream-index stream-index
                             :input-format input-format
                             :output-format output-format}))))
                  input-ports)
            subscriptions
            (into {}
                  (map (fn [{:keys [encoder-context
                                    stream
                                    input-port
                                    port-id
                                    stream-index
                                    input-format
                                    output-format]}]
                         (let [xf (comp (insert-last nil)
                                        (auto-format2 input-format
                                                      output-format)
                                        (if (= :media-type/audio
                                               (:media-type output-format))
                                          (audio-pts)
                                          identity)
                                        (insert-last nil)
                                        (av/encode-frame encoder-context)
                                        (if (= :media-type/video
                                               (:media-type output-format))
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
                                                 (.writeField "stream_index" stream-index))))

                                        )
                               rf (xf
                                   (av/write-packet2 output-format-context))]
                           [(:id input-port)
                            rf])))
                  encoders)]
     ;; need to write header before creating streams
     ;; sets time_base in streams
     (let [err (avformat_write_header output-format-context nil)]
       (when (neg? err)
         (throw (Exception.)))
       err)

     {:subscriptions subscriptions
      :cleaners
      [(fn []
         (let [err (av_write_trailer output-format-context)]
           (when (neg? err)
             (throw (Exception.)))
           err)

         (avio_closep (doto (PointerByReference.)
                        (.setValue (:pb output-format-context))))
         (avformat_free_context output-format-context)
         nil)]})))

(defn filter-audio [media]
  (reify
    IComputeNode
    (configure! [this input-ports]
      {:ports (into []
                    (filter (fn [port]
                              (= :media-type/audio
                                 (get-in port [:format :media-type]))))
                    input-ports)})
    IMediaSource
    (-media-inputs [this]
      [media])
    (-media [this]
      (filterv (fn [src]
                 (= :media-type/audio
                    (:media-type (-format src))))
               (-media media)))))

(defn filter-video [media]
  (reify
    IComputeNode
    (configure! [this input-ports]
      {:ports (into []
                    (filter (fn [port]
                              (= :media-type/video
                                 (get-in port [:format :media-type]))))
                    input-ports)})
    IMediaSource
    (-media-inputs [this]
      [media])
    (-media [this]
      (filterv (fn [src]
                 (= :media-type/video
                    (:media-type (-format src))))
               (-media media)))))

(defrecord Union [medias]
  IComputeNode
  (configure! [this input-ports]
    {:ports (into []
                  (distinct-by :id)
                  input-ports)})
  IMediaSource
  (-media-inputs [this]
    (set medias))
  (-media [this]
    (into []
          (comp (mapcat -media)
                (distinct-by #(System/identityHashCode %)))
          medias)))

(defn union [& medias]
  (->Union medias))


(defrecord MediaFrames [format frames]
  IComputeNode
  (configure! [this input-ports]
    (let [frame-seq (volatile! (seq frames))
          pump
          (fn [send]
            (if-let [s @frame-seq]
              (do (send this (first s))
                  (let [nexts (vswap! frame-seq next)]
                    (nil? nexts)))
              (do
                ;; flush
                (send this)
                true)))]
      {:ports
       [{:id this
         :format
         (datafy-media/map->format format)}]
       :pumps [pump]}))
  IMediaSource
  (-media-inputs [this]
    nil)
  (-media [this]
    [(frame-source
      frames
      (datafy-media/map->format format)
      nil
      "raw-frames")]))

(defn media-frames [format frames]
  (->MediaFrames format
                 frames))

(defn ^:private ->url-str [fname]
  (str "file://" (.getCanonicalPath (io/file fname))))

(deftype FramesReducible [media stream output-format]
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
            :media-type/audio (if output-format
                                (comp
                                 (insert-last nil)
                                 (audio/resample2 input-format
                                                  (datafy-media/map->format output-format
                                                                            :media-type/audio)))
                                identity)
            :media-type/video (if output-format
                                (comp
                                 (insert-last nil)
                                 (video/transcode-frame3
                                  input-format
                                  (datafy-media/kw->pixel-format
                                   (:pixel-format output-format))))
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
  ([media stream {:keys [format]
                  :as opts}]
   (->FramesReducible media
                      stream
                      format)))

(defn compute-graph [media]
  (let [[nodes edges]
        (loop [to-visit #{media}
               edges #{}
               nodes #{}
               visited #{}]
          (if-let [src (first to-visit)]
            (let [visited (conj visited src)]
              (recur (set/difference
                      (into to-visit (-media-inputs src))
                      visited)
                     (into edges
                           (map (fn [to]
                                  [to src]))
                           (-media-inputs src))
                     (conj nodes src)
                     visited))
            [nodes edges]))

        nodes-by-id
        (into {}
              (map-indexed (fn [i node]
                             [i node]))
              nodes)
        node->id (set/map-invert nodes-by-id)
        output-node-id (get node->id media)

        g
        (apply
         loom.graph/digraph
         (concat
          (mapv node->id nodes)
          (eduction
           (map (fn [edge]
                  (mapv node->id edge)))
           edges)))
        top (loom.alg/topsort g)

        compute-graph
        (loop [compute-graph {}
               node-ids (seq top)]
          (if node-ids
            (let [node-id (first node-ids)
                  node (get nodes-by-id node-id)

                  input-nodes (into #{}
                                    (comp
                                     (map first)
                                     (mapcat
                                      (fn [input-node-id]
                                        (get-in compute-graph [input-node-id :ports]))))
                                    (loom.graph/in-edges g node-id))
                  compute-node (configure! node input-nodes)
                  compute-node (update compute-node
                                       :ports
                                       (fn [ports]
                                         (mapv (fn [port]
                                                 (assoc port :media node))
                                               ports)))]
              (recur (assoc compute-graph node-id compute-node)
                     (next node-ids)))
            ;; else
            compute-graph
            ))
        compute-graph
        {:ports (into {}
                      (comp (mapcat :ports)
                            (map (juxt :id identity)))
                      (vals compute-graph))
         ;; ids of the ports of the
         :outputs
         (into []
               (comp (map :id)
                     (distinct))
               (:ports
                (get compute-graph output-node-id)))
         :cleaners (into []
                         (mapcat :cleaners)
                         (vals compute-graph))
         :subscriptions
         (into {}
               (comp
                (mapcat :subscriptions)
                (x/by-key (x/into [])))
               (vals compute-graph))
         :pumps (into []
                      (mapcat :pumps)
                      (vals compute-graph))}]
    compute-graph

    ))


(defn add-subscription [cg port-id f]
  (update-in cg [:subscriptions port-id]
             (fn [subs]
               (if subs
                 (conj subs f)
                 [f]))))
(defn run-graph [cg]
  (let [subscriptions (:subscriptions cg)
        send
        (fn send
          ([port-id]
           (let [subs (get subscriptions port-id)]
             (doseq [sub subs]
               (sub send))))
          ([port-id val]
           (let [subs (get subscriptions port-id)]
             (doseq [sub subs]
               (sub send val)))))]
    (try
      (loop [pumps (media-util/queue (:pumps cg))]
        (when-let [pump (peek pumps)]
          (let [done? (pump send)]
            (if done?
              (recur (pop pumps))
              (recur (conj (pop pumps) pump))))))
      (finally
        ;; cleanup
        (doseq [cleaner (:cleaners cg)]
          (cleaner)))))
  nil)


(defn write!
  ([media fname])
  ([media fname opts]
   (let [media (->FileWriter opts fname media)
         cg (compute-graph media)]
     (run-graph cg))))
