(ns com.phronemophobic.clj-media.impl.av
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.datafy :as d]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            tech.v3.resource
            [com.phronemophobic.clj-media.impl.datafy
             :as datafy-media]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.gen.jna :as gen]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [com.rpl.specter :as specter])
  (:import
   java.io.PushbackReader
   java.nio.ByteOrder
   java.lang.ref.Cleaner
   java.util.Map
)
  (:gen-class))

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

(defn ^:private format-context-streams* [format-context]
  (let [num-streams (:nb_streams format-context)

        num-bytes (* 8 num-streams)

        streams (-> (native-buffer/wrap-address (:streams format-context)
                                                num-bytes)
                    (native-buffer/set-native-datatype :uint64))]
    (into []
          (map #(dt-ffi/ptr->struct :AVStream (dt-ffi/->pointer %)))
          streams)))

(defn open-context [fname]
  (let [format-context (raw/avformat_alloc_context)
        _ (when (nil? format-context)
            (throw (ex-info "Error allocating format context."
                            {:filename fname})))
        
        format-context* (dt-ffi/make-ptr :pointer (-> format-context dt-ffi/->pointer .address))

        _ (prn "opening" fname)

        err (raw/avformat_open_input format-context* (dt-ffi/string->c fname) nil nil)]
    (if (zero? err)
      (reify 
        dt-ffi/PToPointer
        (convertible-to-pointer? [_] true)
        (->pointer [_] (dt-ffi/->pointer format-context))
        
        clojure.lang.ILookup
        (valAt [_ k]
          nil
          (case k
            :streams (format-context-streams* format-context)
            
            ;; else
            (get format-context k)))
        java.lang.AutoCloseable
        (close [_]
          (prn "closing context for " fname)
          (raw/avformat_close_input format-context*)
          ;; use value of format-context*, which may be nulled by close_input
          ;; it's possible that free_context is redundant with close_input
          (raw/avformat_free_context (first format-context*))))
      
      (do
        (raw/avformat_close_input format-context*)
        ;; use value of format-context*, which may be nulled by close_input
        ;; it's possible that free_context is redundant with close_input
        (raw/avformat_free_context (first format-context*))
        (throw (ex-info "Error opening format context"
                      {:error-code err}))))))

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
          path (.getCanonicalPath f)]
    (with-open [format-context (open-context path)]
      (let [err (avformat_find_stream_info format-context nil)
            _ (when (not (zero? err))
                (throw (ex-info "Could not find stream info."
                                {:error-code err})))
            streams-info
            (into []
                  (comp
                   (map (fn [stream+]
                          (let [
                                ;; stream+ (Structure/newInstance AVStreamByReference
                                ;;                                 stream)
                                ;; stream+ (dt-ffi/ptr->struct :AVStream stream)
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
                  (:streams format-context))]
        {:streams streams-info}))))

(comment
  (probe "../clj-media/my-fade-in-out.mp4")
  ,)


