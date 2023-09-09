(ns com.phronemophobic.clj-media.impl.audio
  (:require [clojure.java.io :as io]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.IntByReference
   com.sun.jna.ptr.LongByReference
   com.sun.jna.ptr.ByteByReference
   com.sun.jna.Structure

   (javax.sound.sampled AudioFormat
                        AudioFormat$Encoding
                        AudioInputStream
                        AudioSystem
                        DataLine
                        DataLine$Info
                        Line
                        LineUnavailableException
                        SourceDataLine
                        UnsupportedAudioFileException
                        )

   )
  (:gen-class))

(raw/import-structs!)

(defn pf [& args]
  (apply prn args)
  (flush))

(def AV_CH_FRONT_LEFT             0x00000001)
(def AV_CH_FRONT_RIGHT            0x00000002)
(def AV_CH_FRONT_CENTER           0x00000004)
(def AV_CH_LOW_FREQUENCY          0x00000008)
(def AV_CH_BACK_LEFT              0x00000010)
(def AV_CH_BACK_RIGHT             0x00000020)
(def AV_CH_FRONT_LEFT_OF_CENTER   0x00000040)
(def AV_CH_FRONT_RIGHT_OF_CENTER  0x00000080)
(def AV_CH_BACK_CENTER            0x00000100)
(def AV_CH_SIDE_LEFT              0x00000200)
(def AV_CH_SIDE_RIGHT             0x00000400)
(def AV_CH_TOP_CENTER             0x00000800)
(def AV_CH_TOP_FRONT_LEFT         0x00001000)
(def AV_CH_TOP_FRONT_CENTER       0x00002000)
(def AV_CH_TOP_FRONT_RIGHT        0x00004000)
(def AV_CH_TOP_BACK_LEFT          0x00008000)
(def AV_CH_TOP_BACK_CENTER        0x00010000)
(def AV_CH_TOP_BACK_RIGHT         0x00020000)
(def AV_CH_STEREO_LEFT            0x20000000) ;;  ///< Stereo downmix.
(def AV_CH_STEREO_RIGHT           0x40000000) ;;  ///< See AV_CH_STEREO_LEFT.
;; (def AV_CH_WIDE_LEFT              0x0000000080000000ULL)
;; (def AV_CH_WIDE_RIGHT             0x0000000100000000ULL)
;; (def AV_CH_SURROUND_DIRECT_LEFT   0x0000000200000000ULL)
;; (def AV_CH_SURROUND_DIRECT_RIGHT  0x0000000400000000ULL)
;; (def AV_CH_LOW_FREQUENCY_2        0x0000000800000000ULL)
;; (def AV_CH_TOP_SIDE_LEFT          0x0000001000000000ULL)
;; (def AV_CH_TOP_SIDE_RIGHT         0x0000002000000000ULL)
;; (def AV_CH_BOTTOM_FRONT_CENTER    0x0000004000000000ULL)
;; (def AV_CH_BOTTOM_FRONT_LEFT      0x0000008000000000ULL)
;; (def AV_CH_BOTTOM_FRONT_RIGHT     0x0000010000000000ULL)

(def AV_CH_LAYOUT_MONO AV_CH_FRONT_CENTER)
(def AV_CH_LAYOUT_STEREO (bit-or AV_CH_FRONT_LEFT AV_CH_FRONT_RIGHT))

(comment

  (def fname "file:///Users/adrian/workspace/eddie/megaman.mp3")
  (def fname "file://Users/adrian/workspace/apple-data/iCloud Photos Part 1 of 3/Photos/IMG_0454.mp4" )
  (def fname "/var/tmp/song.m4a")

  (def format-ctx (avformat_alloc_context))
    (def format-ctx* (doto (PointerByReference.)
                       (.setValue (.getPointer format-ctx))))

    (avformat_open_input format-ctx*
                         fname
                         nil
                         nil)
    (.read format-ctx)

    (avformat_find_stream_info format-ctx nil)

    (def best-stream (av_find_best_stream format-ctx AVMEDIA_TYPE_AUDIO -1 -1 nil 0))

    (def num-streams (:nb_streams format-ctx))
    (def streams (.getPointerArray
                  (:streams format-ctx )
                  0 num-streams))

    (def stream (aget streams best-stream))
    (def stream+ (Structure/newInstance AVStreamByReference
                                        stream))

    (:codecpar stream+ )
    (def codec-parameters (:codecpar stream+ ))

    (def codec-id (:codec_id  codec-parameters ))
    

    (def decoder (avcodec_find_decoder codec-id))
    (def decoder-context (avcodec_alloc_context3 (.getPointer decoder)))
    (avcodec_parameters_to_context decoder-context codec-parameters)

    (avcodec_open2 decoder-context decoder nil)

    (def frame (av_frame_alloc))
    (def packet (av_packet_alloc))

    ;; while start
    (av_read_frame format-ctx packet)

    ;; need to check stream index
    ;; else if (pkt->stream_index == audio_stream_idx)

    (avcodec_send_packet decoder-context packet)

    (def ret (avcodec_receive_frame decoder-context frame))

    ;; check ret for error or eagain

    
    (:nb_samples frame )
    ;;     /* Write the raw audio data samples of the first plane. This works
    ;;  * fine for packed formats (e.g. AV_SAMPLE_FMT_S16). However,
    ;;  * most audio decoders output planar audio, which uses a separate
    ;;  * plane of audio samples for each channel (e.g. AV_SAMPLE_FMT_S16P).
    ;;  * In other words, this code will write only the first audio channel
    ;;  * in these cases.
    ;;  * You should use libswresample or libavfilter to convert the frame
    ;;  * to packed data. */
    ;; fwrite(frame->extended_data[0], 1, unpadded_linesize, audio_dst_file);
    
    
    ;; end while


      

  ,
  )

(defn find-encoder-codec [output-format-context {:keys [sample-fmt
                                                        sample-rate
                                                        bit-rate
                                                        channels
                                                        channel-layout]}]
  (let [
        output-format (:oformat output-format-context)
        output-format+ (Structure/newInstance AVOutputFormatByReference
                                              output-format)
        codec-id (:audio_codec output-format+ )

        output-codec (avcodec_find_encoder codec-id)
        _ (when (nil? output-codec)
            (throw (Exception. "could not find encoder")))

        encoder-context (avcodec_alloc_context3 output-codec)
        _ (when (nil? encoder-context)
            (throw (Exception. "Could not create encoder")))

        ;; num-channels 2
        ;; sample-rate (int 44100)
        #_#_sample-fmt (-> output-codec
                       (:sample_fmts )
                       (.getValue))

        _ (doto encoder-context
            (.writeField "channels" (int channels))
            (.writeField "channel_layout" channel-layout
                         ;;(av_get_default_channel_layout num-channels)
                         )
            (.writeField "sample_rate" sample-rate)
            (.writeField "sample_fmt" sample-fmt)
            (.writeField "bit_rate" bit-rate))

        _ (when (not (zero?
                      (bit-and (:flags output-format+)
                               AVFMT_GLOBALHEADER)))
            (doto encoder-context
              (.writeField "flags"
                           (int (bit-or (:flags encoder-context )
                                        AV_CODEC_FLAG_GLOBAL_HEADER)))))
        err (avcodec_open2 encoder-context output-codec nil)
        _ (when (neg? err)
            (throw (ex-info "Could not open codec"
                            {:err err})))

        ;; stream
        stream (avformat_new_stream output-format-context output-codec)
        _ (when (nil? stream)
            (throw (Exception. "Could not create stream.")))

        _ (doto stream
            (.writeField "time_base" (:time_base encoder-context )))

        _ (def my-stream stream)
        ;; error = avcodec_parameters_from_context(stream->codecpar, avctx);
        err (avcodec_parameters_from_context (:codecpar stream )
                                             encoder-context)]
    (when (neg? err)
      (throw (Exception. "Could not initialize stream params")))
    encoder-context))


(defn find-encoder-codec2
  ([output-format-context]
   (find-encoder-codec2 output-format-context nil))
  ([output-format-context {:keys [sample-fmt
                                  sample-rate
                                  bit-rate
                                  channels
                                  channel-layout] :as opts}]
   (let [


         output-format (:oformat output-format-context)
         output-format+ (Structure/newInstance AVOutputFormatByReference
                                               output-format)
         codec-id (:audio_codec output-format+ )

         output-codec (avcodec_find_encoder codec-id)
         _ (when (nil? output-codec)
             (throw (Exception. "could not find encoder")))

         encoder-context (avcodec_alloc_context3 output-codec)
         _ (when (nil? encoder-context)
             (throw (Exception. "Could not create encoder")))

         ;; need to figure out what good defaults are?
         bit-rate (or bit-rate 128000)

         sample-fmt
         (if-let [sample-fmts (:sample_fmts output-codec)]
           (.getInt sample-fmts 0)
           AV_SAMPLE_FMT_FLTP)
         
         sample-rate
         (if-let [supported-sample-rates (:supported_samplerates output-codec)]
           ;; loop through supported sample rates
           ;; if 44100 is an option, choose it
           ;; otherwise, use the sample rate at index 0
           (loop [idx 0]
             (let [sr (.getInt supported-sample-rates (* 4 idx))]
               (cond
                 (= 44100 sr) sr
                 ;; array terminated by -1
                 (= -1 sr) (.getInt supported-sample-rates 0)
                 :else (recur (inc idx)))))
           ;; default
           44100)


         channel-layout
         (if-let [channel-layouts (:channel_layouts output-codec)]
           (loop [idx 0]
             (let [cl (.getLong channel-layouts (* idx 8))]
               (cond
                 (= cl AV_CH_LAYOUT_STEREO) cl
                 ;; terminated by zero
                 (= cl 0) (.getInt channel-layouts 0)
                 :else (recur (inc idx)))))
           AV_CH_LAYOUT_STEREO)
         channels (av_get_channel_layout_nb_channels channel-layout)

         ;; num-channels 2
         ;; sample-rate (int 44100)
         #_#_sample-fmt (-> output-codec
                            (:sample_fmts )
                            (.getValue))

         _ (doto encoder-context
             (.writeField "channels" channels)
             (.writeField "channel_layout" channel-layout
                          ;;(av_get_default_channel_layout num-channels)
                          )
             (.writeField "sample_rate" sample-rate)
             (.writeField "sample_fmt" sample-fmt)
             (.writeField "bit_rate" bit-rate)
             (.writeField "time_base" (av/->avrational 1 sample-rate)))

         _ (when (not (zero?
                       (bit-and (:flags output-format+)
                                AVFMT_GLOBALHEADER)))
             (doto encoder-context
               (.writeField "flags"
                            (int (bit-or (:flags encoder-context )
                                         AV_CODEC_FLAG_GLOBAL_HEADER)))))
         err (avcodec_open2 encoder-context output-codec nil)
         _ (when (neg? err)
             (throw (ex-info "Could not open codec"
                             {:err err})))

         ;; stream
         stream (avformat_new_stream output-format-context output-codec)
         _ (when (nil? stream)
             (throw (Exception. "Could not create stream.")))

         _ (doto stream
             (.writeField "time_base" (:time_base encoder-context )))

         _ (def my-stream stream)
         ;; error = avcodec_parameters_from_context(stream->codecpar, avctx);
         err (avcodec_parameters_from_context (:codecpar stream )
                                              encoder-context)]
     (when (neg? err)
       (throw (Exception. "Could not initialize stream params")))
     encoder-context)))

(defn transcode-frame [decoder-context sample-fmt]
  (fn [rf]
    (let [frame (av_frame_alloc)

          filter-graph (avfilter_graph_alloc)

          buffer (avfilter_get_by_name "abuffer")
          _ (when (nil? buffer)
              (throw (Exception.)))
          buffer-context (avfilter_graph_alloc_filter filter-graph buffer "src")

          args (format "channel_layout=%d:sample_fmt=%d:sample_rate=%d"
                       (:channel_layout decoder-context )
                       (:sample_fmt decoder-context )
                       (:sample_rate decoder-context ))
          err (avfilter_init_str buffer-context args)
          _ (when (not (zero? err))
              (throw (Exception.)))

          buffersink (avfilter_get_by_name "abuffersink")
          _ (when (nil? buffersink)
              (throw (Exception.)))
          buffersink-context* (PointerByReference.)
          _ (avfilter_graph_create_filter buffersink-context*
                                          buffersink
                                          "sink"
                                          nil
                                          nil
                                          filter-graph)

          buffersink-context (.getValue buffersink-context*)

          sample-fmts (doto (IntByReference.)
                        (.setValue sample-fmt))
          _ (av_opt_set_bin buffersink-context "sample_fmts"
                            sample-fmts
                            (* 1 4)
                            AV_OPT_SEARCH_CHILDREN)

          err (avfilter_link buffer-context 0
                             buffersink-context 0)
          _ (when (not (zero? err))
              (throw (Exception.)))
          err (avfilter_graph_config filter-graph nil)]
      (when (not (>= err 0))
        (throw (Exception.)))
      (fn
        ([]
         (rf))
        ([result]
         (av_frame_free (doto (PointerByReference.)
                          (.setValue (.getPointer frame))))
         (avfilter_graph_free
          (doto (PointerByReference.)
            (.setValue (.getPointer filter-graph))))
         (rf result))
        ([result input-frame]
         (av_buffersrc_add_frame buffer-context
                                 input-frame)
         (loop [result result]
           (let [err (av_buffersink_get_frame_flags buffersink-context
                                                    frame
                                                    0)]
             (cond
               (zero? err)
               (let [result (rf result frame) ]
                 (if (reduced? result)
                   result
                   (recur result)))

               (av/eagain? err)
               result

               (av/eof? err)
               (reduced result)

               :else
               (reduced {:error-code err
                         :error-msg (av/error->str err)
                         :type :transcode-error})))))))))


(defn transcode-frame2 [decoder-context encoder-context]
  (fn [rf]
    (let [frame (av_frame_alloc)

          filter-graph (avfilter_graph_alloc)

          buffer (avfilter_get_by_name "abuffer")
          _ (when (nil? buffer)
              (throw (Exception.)))
          buffer-context (avfilter_graph_alloc_filter filter-graph buffer "src")

          decoder-time-base (:time_base decoder-context)
          args (format "channel_layout=%d:sample_fmt=%d:sample_rate=%d:time_base=%d/%d"
                       (:channel_layout decoder-context )
                       (:sample_fmt decoder-context )
                       (:sample_rate decoder-context )
                       1
                       (:sample_rate decoder-context)
                       #_#_(:num decoder-time-base) (:den decoder-time-base))
          err (avfilter_init_str buffer-context args)
          _ (when (not (zero? err))
              (throw (Exception.)))

          buffersink (avfilter_get_by_name "abuffersink")
          _ (when (nil? buffersink)
              (throw (Exception.)))
          buffersink-context* (PointerByReference.)
          _ (avfilter_graph_create_filter buffersink-context*
                                          buffersink
                                          "sink"
                                          nil
                                          nil
                                          filter-graph)

          buffersink-context (.getValue buffersink-context*)

          sample-fmts (doto (IntByReference.)
                        (.setValue (:sample_fmt encoder-context)))
          _ (av_opt_set_bin buffersink-context "sample_fmts"
                            sample-fmts
                            (* 1 4)
                            AV_OPT_SEARCH_CHILDREN)

          sample-rates (doto (IntByReference.)
                         (.setValue (:sample_rate encoder-context)))
          _ (av_opt_set_bin buffersink-context "sample_rates"
                            sample-rates
                            (* 1 4)
                            AV_OPT_SEARCH_CHILDREN)


          channel-layouts (doto (LongByReference.)
                         (.setValue (:channel_layout encoder-context)))
          _ (av_opt_set_bin buffersink-context "channel_layouts"
                            channel-layouts
                            (* 1 8)
                            AV_OPT_SEARCH_CHILDREN)

          channel-counts (doto (IntByReference.)
                           (.setValue (:channels encoder-context)))
          _ (av_opt_set_bin buffersink-context "channel_counts"
                            channel-counts
                            (* 1 4)
                            AV_OPT_SEARCH_CHILDREN)
          

          err (avfilter_link buffer-context 0
                             buffersink-context 0)
          _ (when (not (zero? err))
              (throw (Exception.)))
          err (avfilter_graph_config filter-graph nil)]
      (when (not (>= err 0))
        (throw (Exception.)))
      (fn
        ([]
         (rf))
        ([result]
         (av_frame_free (doto (PointerByReference.)
                          (.setValue (.getPointer frame))))
         (avfilter_graph_free
          (doto (PointerByReference.)
            (.setValue (.getPointer filter-graph))))
         (rf result))
        ([result input-frame]
         (av_buffersrc_add_frame buffer-context
                                 input-frame)
         (loop [result result]
           (let [err (av_buffersink_get_frame_flags buffersink-context
                                                    frame
                                                    0)]
             (cond
               (zero? err)
               (let [result (rf result frame) ]
                 (if (reduced? result)
                   result
                   (recur result)))

               (av/eagain? err)
               result

               (av/eof? err)
               result
               #_(reduced result)

               :else
               (reduced {:error-code err
                         :error-msg (av/error->str err)
                         :type :transcode-error})))))))))

(def fname "file:///Users/adrian/workspace/eddie/megaman.mp3")

(defn play-sound []
  (let [
        sample-rate 44100
        sample-size-in-bits 16
        channels 2
        ;; this is an educated guess
        ;; channels * sample-size-in-bits/byte-size
        frame-size (* channels (/ sample-size-in-bits 8))

        frame-rate 44100
        big-endian? false
        audio-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                   sample-rate
                                   sample-size-in-bits
                                   channels
                                   frame-size
                                   frame-rate
                                   big-endian?)
        info (DataLine$Info. SourceDataLine
                             audio-format)
        source-data-line (^SourceDataLine AudioSystem/getLine info)
        source-data-line (doto ^SourceDataLine source-data-line
                           (.open audio-format)
                           (.start))]
    
    (fn
      ([])
      ([read-bytes]
       (.drain source-data-line)
       (.close source-data-line)
       read-bytes)
      ([read-bytes buf]
       (+ read-bytes
          (.write source-data-line buf 0 (alength buf)))))))



(defn frame->buf [sample-format]
  (let [bytes-per-sample (av_get_bytes_per_sample sample-format)]
    (map (fn [frame]
           (let [buf-size (* bytes-per-sample
                             (:nb_samples frame )
                             (:channels frame ))
                 buf (-> (nth (:data frame ) 0)
                         (.getPointer )
                         (.getByteArray 0 buf-size))]
             
             buf)))))



;; context -> packet --decode--> frame ----encode--> packet -> context?
(defn run
  ([]
   (run fname))
  ([fname]
   (av/with-free [input-format-context (av/open-context fname)
               decoder-context (av/find-decoder-context :audio input-format-context)
               #_#_#_#_output-format-context (open-output-context (str "file://"
                                                               (.getAbsolutePath (io/file fname))))

               encoder-context (find-encoder-codec output-format-context
                                                   {:sample-fmt (:sample_fmt decoder-context )
                                                    :sample-rate (:sample_rate decoder-context )
                                                    ;; AV_SAMPLE_FMT_S16
                                                    :channel-layout (:channel_layout decoder-context )
                                                    :bit-rate (:bit_rate decoder-context )})]
     (try
       (transduce (comp av/read-frame
                          (av/decode-frame decoder-context)

                          (transcode-frame decoder-context AV_SAMPLE_FMT_S16)
                          (frame->buf AV_SAMPLE_FMT_S16)
                          (take 100)
                          )
                  (play-sound)
                  #_(completing
                   (fn [count input]
                     (when (zero? (mod count 1000))
                       (prn count input))
                     (inc count)))
                    0
                    [input-format-context])

       #_(prn (transduce (comp read-frame
                         (decode-frame decoder-context)

                         ;; (transcode-frame decoder-context AV_SAMPLE_FMT_S16)
                         (encode-frame encoder-context))
                   (write-packet output-format-context)
                   #_(completing
                      (fn [count input]
                        (prn input)
                        (flush)
                        (when (zero? (mod count 1000))
                          (prn count input))
                        (inc count)))
                  
                   0
                   [input-format-context]))
       (finally
         ;; (avio_closep)
         ))

     
     )))

(defn resample [decoder-context encoder-context]
  (let [resample-ctx (swr_alloc_set_opts nil
                                         (:channel_layout encoder-context)
                                         (:sample_fmt encoder-context)
                                         (:sample_rate encoder-context)
                                         (:channel_layout decoder-context)
                                         (:sample_fmt decoder-context)
                                         (:sample_rate decoder-context)
                                         0
                                         nil)
        err (swr_init resample-ctx)
        _ (when (neg? err)
            (throw (Exception. "Could not initialize resample context")))
        resample-ctx-ptr (Pointer/nativeValue resample-ctx)
        _ (.register av/cleaner resample-ctx
                     (fn []
                       (swr_free (doto (PointerByReference.)
                                   (.setValue (Pointer. resample-ctx-ptr))) )))

        bytes-per-sample (av_get_bytes_per_sample (:sample_fmt encoder-context))
        output-frame-size (:frame_size encoder-context)

        output-frame (doto (av_frame_alloc)
                       (.writeField "nb_samples" output-frame-size)
                       (.writeField "channel_layout" (:channel_layout encoder-context))
                       (.writeField "format" (:sample_fmt encoder-context))
                       (.writeField "sample_rate" (:sample_rate encoder-context)))
        err (av_frame_get_buffer output-frame 0)
        _ (when (neg? err)
            (throw (Exception. "Could not alloc resample output frame.")))

        ptr (Pointer/nativeValue (.getPointer output-frame))]
    (.register av/cleaner output-frame
               (fn []
                 (av_frame_free
                  (doto (PointerByReference.)
                    (.setValue (Pointer. ptr))))))
    (fn [rf]
      (fn
        ([] (rf))
        ([result]
         (rf result))
        ([result input-frame]
         (if input-frame
           (let [num-samples (:nb_samples input-frame)

                 current-samples (:nb_samples output-frame)
                 samples-wanted (- output-frame-size
                                   current-samples)
                 data-ptr (.share (-> (:data output-frame)
                                      (nth 0)
                                      (.getPointer))
                                  (* bytes-per-sample current-samples))
                 err (swr_convert resample-ctx
                                  data-ptr samples-wanted
                                  (:extended_data input-frame) num-samples)]

             (when (neg? err)
               (throw (Exception. "Error resampling.")))

             (if (pos? err)
               (do
                 (let [old-nb-samples (.readField output-frame "nb_samples")
                       total-samples (+ err old-nb-samples)]
                   (.writeField output-frame "nb_samples" total-samples)
                   (if (= total-samples output-frame-size)
                     (let [result (rf result output-frame)]
                       (.writeField output-frame "nb_samples" 0)
                       result)
                     ;; not enough samples yet
                     result)))
               ;; else
               result))
           ;; else flush
           (let [result
                 (loop [result result]
                   (let [err (swr_convert resample-ctx
                                          (:data output-frame) output-frame-size
                                          nil 0)]
                     (cond

                       (neg? err)
                       (throw (Exception. "Error flushing audio resampler."))

                       (zero? err) result

                       (pos? err)
                       (let [_ (.writeField output-frame "nb_samples" err)
                             result (rf result output-frame)]
                         (if (reduced? result)
                           result
                           (recur result))))))]
             result)))))))

(defn resample2 [input-format output-format]
  (let [resample-ctx* (PointerByReference. Pointer/NULL)
        err (swr_alloc_set_opts2 resample-ctx*
                                 (.getPointer
                                    (:ch-layout output-format))
                                 (:sample-format output-format)
                                 (:sample-rate output-format)
                                 (.getPointer
                                  (:ch-layout input-format))
                                 (:sample-format input-format)
                                 (:sample-rate input-format)
                                 0
                                 nil)

        _ (when (not (zero? err))
            (throw (Exception. "Could not initialize resample context")))
        resample-ctx (.getValue resample-ctx*)

        err (swr_init resample-ctx)
        _ (when (neg? err)
            (throw (Exception. "Could not initialize resample context")))
        resample-ctx-ptr (Pointer/nativeValue resample-ctx)
        _ (.register av/cleaner resample-ctx
                     (fn []
                       (swr_free (doto (PointerByReference.)
                                   (.setValue (Pointer. resample-ctx-ptr))) )))

        bytes-per-sample (av_get_bytes_per_sample (:sample-format output-format))
        num-output-channels (-> output-format
                                :ch-layout
                                :nb_channels)
        sample-offset-multiplier
        (if (= 1 (av_sample_fmt_is_planar (:sample-format output-format)))
          bytes-per-sample
          (* bytes-per-sample num-output-channels))

        _ (assert (pos? bytes-per-sample))
        ;; should maybe check for AV_CODEC_CAP_VARIABLE_FRAME_SIZE?
        output-frame-size (or (:frame-size output-format)
                              (int 1024))

        new-output-frame
        (let [{:keys [ch-layout
                      sample-format
                      sample-rate]}
              output-format]
          (fn []
            (let [frame
                  (doto (av/new-frame)
                    ;; set to output-frame size
                    ;; for av_frame_get_buffer
                    (.writeField "nb_samples" output-frame-size)
                    (.writeField "format" sample-format)
                    (.writeField "sample_rate" sample-rate))]
              (assert
               (zero? (av_channel_layout_copy
                       (.getPointer (:ch_layout frame))
                       (.getPointer ch-layout))))
              (assert
               (>= (av_frame_get_buffer frame 0)
                   0))
              ;; set back to zero now that we've
              ;; alloced the frame's buffer
              ;; we'll be using nb_samples to keep
              ;; track of how many samples we've collected
              ;; as we go.
              (.writeField frame "nb_samples" (int 0))

              frame)))

        output-frame* (volatile!
                       (new-output-frame))]
    (assert (<= num-output-channels (alength (:data (AVFrame.)))))
    (fn [rf]
      (fn
        ([] (rf))
        ([result]
         (rf result))
        ([result input-frame]
         (if input-frame
           (let [output-frame @output-frame*
                 num-samples (:nb_samples input-frame)
                 current-samples (:nb_samples output-frame)
                 samples-wanted (- output-frame-size
                                   current-samples)

                 data-ptr (into-array Pointer
                                      (eduction
                                       (map (fn [p]
                                              (when p
                                                (.share (.getPointer p)
                                                        (* sample-offset-multiplier
                                                           current-samples)))))
                                       (:data output-frame)))

                 err (swr_convert resample-ctx
                                  data-ptr samples-wanted
                                  (:extended_data input-frame) num-samples)]

             (when (neg? err)
               (throw (Exception. "Error resampling.")))

             (if (pos? err)
               (let [total-samples (+ err current-samples)]
                 (.writeField output-frame "nb_samples" (int total-samples))
                 (if (= total-samples output-frame-size)
                   (do
                     (vreset! output-frame* (new-output-frame))
                     (rf result output-frame))
                   ;; not enough samples yet
                   result))
               ;; else
               result))
           ;; else no input frame, flush
           (loop [result result]
             (let [output-frame @output-frame*
                   current-samples (:nb_samples output-frame)
                   samples-wanted (- output-frame-size
                                     current-samples)

                   data-ptr (into-array Pointer
                                        (eduction
                                         (map (fn [p]
                                                (when p
                                                  (.share (.getPointer p)
                                                          (* sample-offset-multiplier
                                                             current-samples)))))
                                         (:data output-frame)))

                   err (swr_convert resample-ctx
                                    data-ptr output-frame-size
                                    nil 0)]
               (cond

                 (neg? err)
                 (throw (Exception. "Error flushing audio resampler."))

                 (zero? err) result

                 (pos? err)
                 (let [total-samples (+ err current-samples)]
                   (.writeField output-frame "nb_samples" (int total-samples))
                   (if (= total-samples output-frame-size)
                     (do
                       (vreset! output-frame* (new-output-frame))
                       (let [result (rf result output-frame)]
                         (if (reduced? result)
                           result
                           (recur result))))
                     ;; send last frame
                     (rf result output-frame))))))))))))

(defn -main [& args]
  (run))



