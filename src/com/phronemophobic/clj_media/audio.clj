(ns com.phronemophobic.clj-media.audio
  (:require [clojure.java.io :as io]
            [com.phronemophobic.clj-media.av :as av]
            [com.phronemophobic.clj-media.av.raw :as raw
             :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.IntByReference
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


(comment

  (def fname "file:///Users/adrian/workspace/eddie/megaman.mp3")
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

    (def num-streams (.readField format-ctx "nb_streams"))
    (def streams (.getPointerArray
                  (.readField format-ctx "streams")
                  0 num-streams))

    (def stream (aget streams best-stream))
    (def stream+ (Structure/newInstance AVStreamByReference
                                        stream))

    (.readField stream+ "codecpar")
    (def codec-parameters (.readField stream+ "codecpar"))

    (def codec-id (.readField codec-parameters "codec_id" ))
    

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

    
    (.readField frame "nb_samples")
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
                                                        channel-layout]}]
  (let [
        output-format (.readField output-format-context
                                  "oformat")
        output-format+ (Structure/newInstance AVOutputFormatByReference
                                              output-format)
        codec-id (.readField output-format+ "audio_codec")

        output-codec (avcodec_find_encoder codec-id)
        _ (when (nil? output-codec)
            (throw (Exception. "could not find encoder")))

        encoder-context (avcodec_alloc_context3 output-codec)
        _ (when (nil? encoder-context)
            (throw (Exception. "Could not create encoder")))

        num-channels 2
        ;; sample-rate (int 44100)
        #_#_sample-fmt (-> output-codec
                       (.readField "sample_fmts")
                       (.getValue))

        _ (doto encoder-context
            (.writeField "channels" (int num-channels))
            (.writeField "channel_layout" channel-layout
                         ;;(av_get_default_channel_layout num-channels)
                         )
            (.writeField "sample_rate" sample-rate)
            (.writeField "sample_fmt" sample-fmt)
            (.writeField "bit_rate" bit-rate))

        _ (when (not (zero?
                      (bit-and (-> output-format+
                                   (.readField "flags"))
                               AVFMT_GLOBALHEADER)))
            (doto encoder-context
              (.writeField "flags"
                           (int (bit-or (.readField encoder-context "flags")
                                        AV_CODEC_FLAG_GLOBAL_HEADER)))))
        err (avcodec_open2 encoder-context output-codec nil)
        _ (when (neg? err)
            (throw (Exception.)))

        ;; stream
        stream (avformat_new_stream output-format-context output-codec)
        _ (when (nil? stream)
            (throw (Exception. "Could not create stream.")))

        sample-rate 44100
        time-base (doto (AVRational.)
                    (.writeField "den" (int sample-rate))
                    (.writeField "num" (int 1)))
        _ (doto stream
            (.writeField "time_base" (.readField encoder-context "time_base")))

        _ (def my-stream stream)
        ;; error = avcodec_parameters_from_context(stream->codecpar, avctx);
        err (avcodec_parameters_from_context (.readField stream "codecpar")
                                             encoder-context)]
    (when (neg? err)
      (throw (Exception. "Could not initialize stream params")))
    encoder-context))

(defn transcode-frame [decoder-context sample-fmt]
  (fn [rf]
    (let [frame (av_frame_alloc)

          filter-graph (avfilter_graph_alloc)

          buffer (avfilter_get_by_name "abuffer")
          _ (when (nil? buffer)
              (throw (Exception.)))
          buffer-context (avfilter_graph_alloc_filter filter-graph buffer "src")

          args (format "channel_layout=%d:sample_fmt=%d:sample_rate=%d"
                       (.readField decoder-context "channel_layout")
                       (.readField decoder-context "sample_fmt")
                       (.readField decoder-context "sample_rate"))
          _ (prn args)
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
        (prn err)
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
                             (.readField frame "nb_samples")
                             (.readField frame "channels"))
                 buf (-> (nth (.readField frame "data") 0)
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
                                                   {:sample-fmt (.readField decoder-context "sample_fmt")
                                                    :sample-rate (.readField decoder-context "sample_rate")
                                                    ;; AV_SAMPLE_FMT_S16
                                                    :channel-layout (.readField decoder-context "channel_layout")
                                                    :bit-rate (.readField decoder-context "bit_rate")})]
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


(defn -main [& args]
  (run))




