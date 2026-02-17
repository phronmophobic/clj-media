(ns com.phronemophobic.clj-media.impl.av
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.datafy :as d]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [com.phronemophobic.clj-media.impl.datafy
             :as datafy-media]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.gen.jna :as gen]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [com.rpl.specter :as specter])
  (:import
   java.io.PushbackReader
   ;; com.sun.jna.Memory
   ;; com.sun.jna.Pointer
   ;; com.sun.jna.ptr.PointerByReference
   ;; com.sun.jna.ptr.IntByReference
   ;; com.sun.jna.ptr.ByteByReference
   java.nio.ByteOrder
   java.lang.ref.Cleaner
   java.util.Map
   ;; com.sun.jna.Structure
)
  (:gen-class))

;; (raw/import-structs!)

(def cleaner (Cleaner/create))

(defonce handles (atom #{}))
(defn ref! [o]
  (swap! handles conj o)
  o)

(defn ->avrational [num den]
   (datafy-media/->avrational num den))


(defn error->str [err]
  (let [buf (native-buffer/malloc 256)]
    (av_strerror err buf (native-buffer/native-buffer-byte-len buf))
    (dt-ffi/c->string buf)))


(defn eof? [err]
  (= err AVERROR_EOF))
(defn eagain? [err]
  (averror-eagains err))
(defn einvalid? [err]
  (= err AVERROR_INVALIDDATA))




;; (defn next-packet [ctx]
;;   (let [packet (av_packet_alloc)
;;         ptr (Pointer/nativeValue (.getPointer packet))]
;;     (.register cleaner packet
;;                (fn []
;;                  (av_packet_free
;;                   (doto (PointerByReference.)
;;                     (.setValue (Pointer. ptr))))))
;;     (let [err (av_read_frame (.getPointer ctx) packet)
;;             result (cond
;;                      (zero? err)
;;                      packet

;;                      (eof? err)
;;                      nil

;;                      :else ;; some other error
;;                      (throw (ex-info "Error reading packet"
;;                                      {:error-code err
;;                                       :type :read-error})))]
;;         result)))

;; (defn packet-seq [ctx]
;;   (when-let [packet (next-packet ctx)]
;;     (cons packet (lazy-seq (packet-seq ctx)))))

;; doesn't work
;; because it doesn't implement valAt
;; for underlying frame struct
;; (defrecord Frame+ [frame]
;;   com.sun.jna.NativeMapped
;;   (nativeType [_]
;;     Pointer)
;;   (toNative [_]
;;     (.toNative frame)))

;; (defn new-frame []
;;   (let [frame (av_frame_alloc)
;;         ptr (Pointer/nativeValue (.getPointer frame))]
;;     (.register cleaner frame
;;                (fn []
;;                  (let [pointer (Pointer. ptr)]
;;                    ;; av_frame_free also calls unref
;;                    ;; but this is what the examples do
;;                    (av_frame_unref pointer)
;;                    (av_frame_free
;;                     (doto (PointerByReference.)
;;                       (.setValue pointer))))))
;;     frame))

;; (defn new-packet[]
;;   (let [packet (av_packet_alloc)
;;         ptr (Pointer/nativeValue (.getPointer packet))]
;;     (.register cleaner packet
;;                (fn []
;;                  (let [pointer (Pointer. ptr)]
;;                    ;; av_frame_free also calls unref
;;                    ;; but this is what the examples do
;;                    (av_packet_unref pointer)
;;                    (av_packet_free
;;                     (doto (PointerByReference.)
;;                       (.setValue pointer))))))
;;     packet))

;; shouldn't be a transducer
;; instead. should return an IReduceInit
;; (defn read-frame [rf]
;;   (fn
;;     ([] (rf))
;;     ([result] (rf result))
;;     ([result format-context]
;;      (loop [result result]
;;        (let [packet (new-packet)
;;              err (av_read_frame (.getPointer format-context) packet)
;;              result (cond
;;                       (zero? err)
;;                       (rf result packet)

;;                       (eof? err)
;;                       (ensure-reduced
;;                        (rf result packet))

;;                       :else ;; some other error
;;                       (reduced {:error-code err
;;                                 :type :read-error}))]
;;          (if (reduced? result)
;;            result
;;            (recur result)))))))



;; (defn decode-frame [decoder-context]
;;   (fn [rf]
;;     (fn
;;       ([] (rf))
;;       ([result] (rf result))
;;       ([result packet]
;;        (let [err (avcodec_send_packet decoder-context packet)
;;              result
;;              (if (and (not (zero? err))
;;                       (not= err -22)
;;                       (eagain? err))
;;                (reduced {:error-code err
;;                          :type :send-packet-failure})
;;                (loop [result result]
;;                  (let [frame (new-frame)
;;                        err (avcodec_receive_frame decoder-context frame)]
;;                    (cond
;;                      (or (zero? err)
;;                          (einvalid? err))
;;                      (let [result (rf result frame)]
;;                        (if (reduced? result)
;;                          result
;;                          (recur result)))

;;                      (eagain? err)
;;                      result

;;                      (eof? err)
;;                      (reduced result)

;;                      ;; some other error
;;                      :else
;;                      (reduced {:error-code err
;;                                :error-msg (error->str err)
;;                                :type :decode-error})))))]
;;          result)))))


;; (defn encode-frame [encoder-context]
;;   (fn [rf]
;;     (fn
;;       ([] (rf))
;;       ;; needs to send empty frame here and receive packets again.
;;       ([result] (rf result))
;;       ([result frame]
;;        (let [err (avcodec_send_frame encoder-context frame)
;;              result
;;              (if (and (not (zero? err))
;;                       (not= err -22)
;;                       (eagain? err))
;;                (reduced {:error-code err
;;                          :type :send-packet-failure})
;;                (loop [result result]
;;                  (let [packet (new-packet)
;;                        err (avcodec_receive_packet encoder-context packet)]
;;                    (cond
;;                      (or (zero? err)
;;                          (einvalid? err))
;;                      (let [result (rf result packet)]
;;                        (if (reduced? result)
;;                          result
;;                          (recur result)))

;;                      (eagain? err)
;;                      result

;;                      (eof? err)
;;                      result
;;                      #_(reduced result)

;;                      ;; some other error
;;                      :else
;;                      (reduced {:error-code err
;;                                :error-msg (error->str err)
;;                                :type :encode-error})))))]
;;          result)))))

;; (defn write-packet [output-format-context]
;;   (let [err (avformat_write_header output-format-context nil)]
;;     (when (neg? err)
;;       (throw (Exception.))))

;;   (fn
;;     ([])
;;     ([result]
;;      (let [result
;;            (loop []
;;              (let [err (av_write_frame output-format-context nil)]
;;                (cond

;;                  (neg? err)
;;                  (reduced {:error-code err
;;                            :type :write-flush-error})

;;                  ;; we're done
;;                  (= 1 err)
;;                  nil

;;                  ;; continue flushing
;;                  :else
;;                  (recur))))

;;            result (if (reduced? result)
;;                     result
;;                     (let [err (av_write_trailer output-format-context)]
;;                       (if (zero? err)
;;                         nil
;;                         (reduced {:error-code err
;;                                   :type :write-tailer-error}))))]
;;        result))
;;     ([result packet]
;;      (let [err (av_write_frame output-format-context packet)
;;            result (if (neg? err)
;;                     (reduced {:error-code err
;;                               :type :write-error})
;;                     ;; else
;;                     nil)]
;;        result))))

;; (defn write-packet2
;;   "Reducing function that writes a header, packets, then trailer."
;;   [output-format-context]
;;   (fn
;;     ([]
;;        output-format-context)
;;     ([result]
;;        (let [result
;;            (loop []
;;              (let [err (av_write_frame output-format-context nil)]
;;                (cond

;;                  (neg? err)
;;                  (reduced {:error-code err
;;                            :type :write-flush-error})

;;                  ;; we're done
;;                  (= 1 err)
;;                  nil

;;                  ;; continue flushing
;;                  (zero? err)
;;                  (recur)


;;                  :else
;;                  (reduced {:type :write-flush-error}))))]
;;          result))
;;     ([result packet]
;;      (let [err (av_write_frame output-format-context packet)
;;            result (if (neg? err)
;;                     (reduced {:error-code err
;;                               :type :write-error})
;;                     ;; else
;;                     result)]
;;        result))))

(defn open-context [fname]
  (let [format-ctx (avformat_alloc_context)
        _ (when (nil? format-ctx)
            (throw (ex-info "Error allocating format context."
                            {:filename fname})))

        format-ctx* (dt-ffi/make-ptr :pointer (-> format-ctx
                                                  dt-ffi/->pointer
                                                  .address))
        #_(doto (PointerByReference.)
                      (.setValue (.getPointer format-ctx)))
        _ (.register cleaner format-ctx
                     (fn []
                       (avformat_free_context (.getValue format-ctx*))))
        err (avformat_open_input format-ctx* (dt-ffi/string->c fname) nil nil)]

    (if (zero? err)
      (do
        (.register cleaner format-ctx
                     (fn []
                       (avformat_close_input
                        
                        ;; hold explicit reference to format-ctx
                        (-> format-ctx
                            dt-ffi/->pointer
                            .address)
                        #_(PointerByReference. format-ctx))))
        format-ctx)
      (throw (ex-info "Error opening format context"
                      {:error-code err
                       :error-msg (error->str err)})))))

(defn video-codec-context-format [codec-context]
  {:codec {:id (:codec_id codec-context)}
   :media-type :media-type/video
   :width (:width codec-context)
   :height (:height codec-context)
   :pixel-format (:pix_fmt codec-context)
   :gop-size (:gop_size codec-context)})

(defn audio-codec-context-format [codec-context]
  {:codec {:id (:codec_id codec-context)}
   :media-type :media-type/audio
   :sample-format (:sample_fmt codec-context)
   :sample-rate (:sample_rate codec-context)
   ;; channel-layout is deprecated
   ;; :channel-layout (:channel_layout codec-context)
   :frame-size (:frame_size codec-context)
   :ch-layout (let [;; channel layout may mutate,
                    ;; make a copy!
                    ch-layout (dt-struct/new-struct :AVChannelLayout {:container-type :native-heap})]
                (raw/av_channel_layout_copy ch-layout (:ch_layout codec-context))
                ch-layout)})

(defn codec-context-format [codec-context]
  (condp = (:codec_type codec-context)
    AVMEDIA_TYPE_AUDIO (audio-codec-context-format codec-context)
    AVMEDIA_TYPE_VIDEO (video-codec-context-format codec-context)))

(defn add-stream [output-format-context encoder-context]
  (let [output-format (:oformat output-format-context)
        output-format+ (dt-ffi/ptr->struct :AVOutputFormat output-format)
        #_(Structure/newInstance AVOutputFormatByReference
                                              output-format)

        _ (when (not (zero?
                      (bit-and (:flags output-format+)
                               AVFMT_GLOBALHEADER)))
            (doto encoder-context
              (Map/.put :flags
                    (int (bit-or (:flags encoder-context )
                                 AV_CODEC_FLAG_GLOBAL_HEADER)))))

        output-codec (:codec encoder-context)
        _ (assert output-codec)
        err (avcodec_open2 encoder-context output-codec nil)
        _ (when (neg? err)
            (throw (ex-info "Could not open codec"
                            {:err err})))

        ;; stream
        stream (avformat_new_stream output-format-context 
                                    ;; originally passed output-codec, but
                                    ;; per the docs. this parameter is not used and does nothing.
                                    ;;output-codec
                                    nil
                                    )
        _ (when (nil? stream)
            (throw (Exception. "Could not create stream.")))

        ;; _ (doto stream
        ;;     (.writeField "time_base" (:time_base encoder-context )))

        ;; error = avcodec_parameters_from_context(stream->codecpar, avctx);
        err (avcodec_parameters_from_context (:codecpar stream )
                                             encoder-context)]
        (when (neg? err)
          (throw (Exception. "Could not initialize stream params")))
        stream))

(defn video-encoder-context [format]
  (let [codec-id (-> format :codec :id)

        output-codec (avcodec_find_encoder codec-id)
        _ (when (nil? output-codec)
            (throw (Exception. "could not find encoder")))

        encoder-context (avcodec_alloc_context3 output-codec)
        _ (when (nil? encoder-context)
            (throw (Exception. "Could not create encoder")))

        ;; /**
        ;; * the average bitrate
        ;; * - encoding: Set by user; unused for constant quantizer encoding.
        ;; * - decoding: Set by user, may be overwritten by libavcodec
        ;; *             if this info is available in the stream
        ;; */
        ;; unclear if setting a default bit-rate is a good default.
        ;; popular video formats have constant quantizers like h264.
        ;; bit-rate (or (get format :bit-rate) 128000)

        {:keys [time-base
                width
                height
                pixel-format]} format

        gop-size (or (:gop-size format)
                     ;; default for ffmpeg
                     12
                     )


        _ (when (= codec-id raw/AV_CODEC_ID_H264)
            (raw/av_opt_set (:priv_data encoder-context) 
                            (dt-ffi/string->c "preset")
                            (dt-ffi/string->c "slow") 
                            0))

        _ (doto encoder-context
            (Map/.put :width (int width))
            (Map/.put :height (int height))
            (Map/.put :gop_size (int gop-size))
            ;; (.writeField "max_b_frames" (int max_b_frames))
            (Map/.put :pix_fmt pixel-format)
            (Map/.put :time_base time-base))

        _ (when-let [bit-rate (:bit-rate format)]
            (doto encoder-context
              (Map/.put :bit_rate bit-rate)))]
    encoder-context))

(defn audio-encoder-context [format]
  (let [codec-id (-> format :codec :id)
        output-codec (avcodec_find_encoder codec-id)
        _ (when (nil? output-codec)
            (throw (Exception. "could not find encoder")))

        encoder-context (avcodec_alloc_context3 output-codec)
        _ (when (nil? encoder-context)
            (throw (Exception. "Could not create encoder")))

        bit-rate (or (get format :bit-rate) 128000)

        sample-fmt (:sample-format format)
        sample-rate (:sample-rate format)
        ch-layout (:ch-layout format)

        _ (assert
           (zero? (av_channel_layout_copy
                   (:ch_layout encoder-context)
                   ch-layout)))

        _ (doto encoder-context
            (Map/.put :sample_rate sample-rate)
            (Map/.put :sample_fmt sample-fmt)
            (Map/.put :bit_rate bit-rate)
            (Map/.put :time_base (->avrational 1 sample-rate)))]

    encoder-context))

(defn encoder-context [format]
  (case (:media-type format)
    :media-type/video (video-encoder-context format)
    :media-type/audio (audio-encoder-context format)))


(defn find-decoder-context [media-type format-context]
  (let [err (avformat_find_stream_info format-context nil)
        _ (when (not (zero? err))
            (throw (ex-info "Could not find stream info."
                            {:error-code err})))
        best-stream (av_find_best_stream format-context
                                         (case media-type
                                           :audio AVMEDIA_TYPE_AUDIO
                                           :video AVMEDIA_TYPE_VIDEO)
                                         -1
                                         -1
                                         nil
                                         0)
        _ (when (neg? best-stream)
            (throw (ex-info "Could not find best stream"
                            {:error-code best-stream
                             :media-type media-type})))
        ;; (.readField format-context "nb_streams")
        num-streams (:nb_streams format-context)
        
        ;; streams (.getPointerArray
        ;;          (.readField format-context "streams")
        ;;          0 num-streams)
        
        streams (-> (native-buffer/wrap-address (:streams format-context)
                                                (* 8 num-streams))
                    (native-buffer/set-native-datatype :uint64))
        stream (nth streams best-stream)
        ;; stream+ (Structure/newInstance AVStreamByReference
        ;;                                stream)
        stream+ (dt-ffi/ptr->struct :AVStream (dt-ffi/->pointer stream))

        ;; codec-parameters (.readField stream+ "codecpar")
        codec-parameters (dt-ffi/ptr->struct 
                          :AVCodecParameters
                          (:codecpar stream+))
        ;; codec-id (.readField codec-parameters "codec_id" )
        codec-id (:codec-id codec-parameters)
        decoder (avcodec_find_decoder codec-id)
        _ (when (nil? decoder)
            (throw (ex-info "Could not find decoder"
                            {:codec-id codec-id})))
        decoder-context (avcodec_alloc_context3 decoder)

        _ (when (nil? decoder-context)
            (throw (ex-info "Could not allocate decoder"
                            {})))

        _ (doto decoder-context
            (Map/.put decoder-context :time_base (:time_base stream+ ))
            #_(.writeField "time_base"
                         (.readField stream+ "time_base")))]

    (avcodec_parameters_to_context decoder-context codec-parameters)
    (let [err (avcodec_open2 decoder-context decoder nil)]
      (when (neg? err)
        (throw (Exception. "Could not open codec"
                           {:error-code err})))
      decoder-context)))

(defn open-output-context [fname]
  (let [;; output-io-context* (PointerByReference.)
        output-io-context* (dt-ffi/make-ptr :pointer 0)
        fname* (dt-ffi/string->c fname)
        err (avio_open output-io-context*
                       fname*
                       AVIO_FLAG_WRITE)
        _ (when (neg? err)
            (throw (Exception.)))

        ;; output-io-context (Structure/newInstance AVIOContextByReference
        ;;                                          (.getValue output-io-context*))
        output-io-context (dt-ffi/ptr->struct :AVIOContext
                                              (first output-io-context*))
        
        ;; output-format-context* (PointerByReference.)
        output-format-context* (dt-ffi/make-ptr :pointer 0)
        err (avformat_alloc_output_context2 output-format-context*
                                            nil
                                            nil
                                            fname*)
        _ (when (neg? err)
            (throw (Exception. "Could not create output context.")))
        ;; output-format-context (Structure/newInstance AVFormatContextByReference
        ;;                        (.getValue output-format-context*))
        output-format-context (dt-ffi/ptr->struct :AVFormatContext
                                                  (first output-format-context*))
        output-format-context (doto output-format-context
                                #_(.writeField "pb" (.getValue output-io-context*))
                                (Map/.put :pb output-io-context))]
    output-format-context))


(defn free [p]
  #_(condp instance? p
    com.phronemophobic.clj_ffmpeg.structs.AVFormatContext.ByReference
    (avformat_close_input (doto (PointerByReference.)
                            (.setValue (.getPointer p))))

    com.phronemophobic.clj_ffmpeg.structs.AVCodecContext.ByReference
    (avcodec_free_context (doto (PointerByReference.)
                            (.setValue (.getPointer p))))))

(defmacro with-free
  [bindings & body]
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-free ~(subvec bindings 2) ~@body)
                                (finally
                                  (free ~(bindings 0)))))
    :else (throw (IllegalArgumentException.
                   "with-free only allows Symbols in bindings"))))



(defn raw-codec-list []
  (let [;; iter-data (PointerByReference. Pointer/NULL)
        iter-data (dt-ffi/make-ptr :pointer 0)]
    (loop [codecs []]
      (let [codec (av_codec_iterate iter-data)]
        (if codec
          (recur (conj codecs codec))
          codecs)))))


(defn list-codecs []
  (into []
        (map d/datafy)
        (raw-codec-list)))


(defn probe [f]
  (let [f (io/as-file f)
        path (.getCanonicalPath f)
        format-context (open-context path)
        err (avformat_find_stream_info format-context nil)
        _ (when (not (zero? err))
            (throw (ex-info "Could not find stream info."
                            {:error-code err})))
        ;; num-streams (:nb_streams format-context)
        num-streams (:nb_streams format-context)
        ;; streams (.getPointerArray
        ;;          (.readField format-context "streams")
        ;;          0 num-streams)
        streams (-> (native-buffer/wrap-address (:streams format-context)
                                                (* 8 num-streams))
                    (native-buffer/set-native-datatype :uint64))

        streams-info
        (into []
              (comp
               (map (fn [stream]
                      (let [
                            ;; stream+ (Structure/newInstance AVStreamByReference
                            ;;                                 stream)
                            stream+ (dt-ffi/ptr->struct :AVStream stream)
                            stream-index (:index stream+)

                            codec-parameters (dt-ffi/ptr->struct 
                                              :AVCodecParameters
                                              (:codecpar stream+))
                            codec-id (:codec_id codec-parameters)

                            media-type (:codec_type codec-parameters)

                            format (merge
                                    {:time-base (d/datafy (:time_base stream+))
                                     :estimated-duration (:duration stream+)
                                     :stream-index (:index stream+)}
                                    (let [num-frames (:nb_frames stream+)]
                                      (when (not (zero? num-frames))
                                        {:num-frames num-frames}))
                                    (when (= :media-type/video
                                             media-type)
                                      {:average-frame-rate (:avg_frame_rate stream+)})
                                    (d/datafy codec-parameters))]
                        format))))
              streams)]
    (avformat_close_input 
     ;;(PointerByReference. (.getPointer format-context))
     (dt-ffi/make-ptr :pointer (-> format-context (dt-ffi/->pointer) .address)))
    {:streams streams-info}))

(comment
  (probe "../clj-media/my-fade-in-out.mp4")
  ,)



(defn make-frame [{:keys [bytes
                          format
                          time-base
                          key-frame?
                          pts]
                   :as m}]
  (let [frame (av_frame_alloc)]
    (if time-base
      (doto frame
       (Map/.put :time_base (datafy-media/clj->avrational time-base)))
      ;; else
      (throw (ex-info "Time base required when creating frames."
                      {:frame m})))

    (if pts
      (doto frame
        (Map/.put :pts (long pts)))
      ;; else
      (throw (ex-info "pts required when creating frames."
                      {:frame m})))

    (when key-frame?
      (doto frame
        (Map/.put :key_frame (case key-frame?
                           (1 true) (int 1)
                           ;; else
                           (int 0)))))

    (if bytes
      (case (:media-type format)
        :media-type/audio
        (let [{:keys [ch-layout
                      sample-format
                      sample-rate]} (datafy-media/map->format format)

              bytes-per-sample (raw/av_get_bytes_per_sample sample-format)
              num-output-channels (-> ch-layout
                                      :nb_channels)
              ;; calculation assumes non-planar format
              num-samples
              (Long/divideUnsigned
               (alength bytes)
               (* bytes-per-sample num-output-channels))]

          (when (= 1 (raw/av_sample_fmt_is_planar sample-format))
            (throw (ex-info "Cannot create planar audio frames."
                            {:frame m})))

          (doto frame
            (Map/.put :nb_samples (int num-samples))
            (Map/.put :format sample-format)
            (Map/.put :sample_rate sample-rate))
          (assert
           (zero? (raw/av_channel_layout_copy
                   (.getPointer (:ch_layout frame))
                   (.getPointer ch-layout))))
          (when (neg? (raw/av_frame_get_buffer frame 0)) 
            (throw (ex-info "Error allocating frame buffer.")))
          ;; linesize might not match the byte array size
          ;; since linesize is sometimes set for a particular alignment
          ;; I think line size is set by raw/av_frame_get_buffer
          #_(when (> (alength bytes)
                   (first (:linesize frame)))
                (throw (ex-info "Bytes are the wrong length for sample format."
                                {:frame m
                                 :bytes bytes
                                 :actual-size (native-buffer/native-buffer-byte-len bytes)
                                 :expected-length (first (:linesize frame))})))
          (dt/copy! bytes
                    (native-buffer/wrap-address (first (:data frame))
                                                (first (:linesize frame)))))

        :media-type/video
        (let [{:keys [pixel-format
                      width
                      height]} (datafy-media/map->format format)
              line-size (:line-size format)]
          (doto frame
            (Map/.put :width (int width))
            (Map/.put :height (int height))
            (Map/.put :format pixel-format))
          (if line-size
            (doto frame
              (Map/.put :linesize
                    (doto (int-array 8)
                      (aset 0 line-size))))
            (throw (ex-info ":line-size must be set when creating video frames."
                            {:frame m})))
          (assert
           (>= (raw/av_frame_get_buffer frame 0)
               0))

          (dt/copy! bytes 
                    (native-buffer/wrap-address (first (:data frame))
                                                (* line-size height))))

        nil (throw (ex-info "frame requires `:media-type` to be set."
                            {:frame m})))

      ;; else, no bytes
      (throw (ex-info "bytes required when creating frames."
                      {:frame m})))

    frame))
