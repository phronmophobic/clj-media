(ns com.phronemophobic.clj-media.audio
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [com.phronemophobic.clong.clang :as clang]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.gen.jna :as gen]

            [com.rpl.specter :as specter])
  (:import
   java.io.PushbackReader
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


(defn pf [& args]
  (apply prn args)
  (flush))

(defonce handles (atom #{}))
(defn ref! [o]
  (swap! handles conj o)
  o)

(def AV_OPT_SEARCH_CHILDREN 1)
(def averror-eagains #{-11 -35})
(def AVERROR_EOF -541478725)
(def AVERROR_INVALIDDATA -1094995529)
(def AVIO_FLAG_READ  1)
(def AVIO_FLAG_WRITE 2)
(def AVIO_FLAG_READ_WRITE 3)
(def AVFMT_GLOBALHEADER  0x0040)
(def AV_CODEC_FLAG_GLOBAL_HEADER (bit-shift-left 1 22))

(def lib
  (com.sun.jna.NativeLibrary/getProcess))

(def lib-names ["avutil"
                "avresample"
                "avfilter"
                "avformat"
                "avdevice"
                "swresample"
                "swscale"
                "avcodec"])
(def header-files
  (into []
        (comp (map #(io/file "/opt/local/include" (str "lib" %)))
              (mapcat #(.listFiles %))
              (map #(.getAbsolutePath %))
              (filter #(str/ends-with? % ".h")))
        lib-names))

(defonce libs (atom #{}))
(defn load-libs
  ([]
   (load-libs lib-names))
  ([lib-names]
   (let [new-libs
         (into []
               (map #(com.sun.jna.NativeLibrary/getInstance %))
               lib-names)]
     
     (swap! libs into new-libs))))
(load-libs)

(defn- slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn parse-av-api []
  (clang/easy-api nil ;; (first header-files)
                  (into (conj clang/default-arguments
                              (first header-files))
                        (mapcat (fn [h]
                                  ["-include" h]))
                        (rest header-files))))

(defn dump-av-api []
  (let [api (parse-av-api)]
    (with-open [w (io/writer (io/file
                              "resources"
                              "av-api.edn"))]
      (clang/write-edn w (parse-av-api)))))

(def oformat-path [:structs
                   specter/ALL
                   #(= :clong/AVFormatContext (:id %))
                   :fields
                   specter/ALL
                   #(#{"oformat" "pb"}
                       (:name %))
                   :datatype])

;; JNA has trouble `.writeField`ing
;; to StructureByReference fields
(defn with-hacks [api]
  (specter/setval oformat-path
                  :coffi.mem/pointer
                  api))

(def av-api
  (with-hacks
    (with-open [rdr (io/reader (io/file "resources"
                                        "av-api.edn"))
                pbr (PushbackReader. rdr)]
      (edn/read pbr))))


(comment
  (gen/remove-forward-references
   av-api)
  ,)


(defonce generated-api (gen/def-api lib av-api))
(import '(com.phronemophobic.clj_media.audio.structs
          AVRational
          AVFormatContextByReference
          AVOutputFormatByReference
          AVIOContextByReference
          AVFrameByReference))

(defn error->str [err]
  (let [buf (byte-array 255)]
    (av_strerror err buf (alength buf))
    (let [s (String. buf 0 (transduce
                            (take-while #(not (zero? %)))
                            (completing
                             (fn [cnt _]
                               (inc cnt)))
                            0
                            buf))]
      s)))

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
    (def stream+ (Structure/newInstance com.phronemophobic.clj_media.audio.structs.AVStreamByReference
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



(defn eof? [err]
  (= err AVERROR_EOF))
(defn eagain? [err]
  (averror-eagains err))
(defn einvalid? [err]
  (= err AVERROR_INVALIDDATA))

(defn read-frame [rf]
  (let [packet (av_packet_alloc)]
    (fn
      ([] (rf))
      ([result]
       (av_packet_free
        (doto (PointerByReference.)
          (.setValue (.getPointer packet))))
       (rf result))
      ([result format-context]

       (loop [result result]
         (let []
           (if (nil? packet)
             (reduced {:error-msg "Could not alloc packet"})
             (let [err (av_read_frame (.getPointer format-context) packet)
                   result (cond
                            (zero? err)
                            (rf result packet)
                            
                            (eof? err)
                            (ensure-reduced
                             (rf result packet))
                            
                            :else ;; some other error
                            (reduced {:error-code err
                                      :type :read-error}))]
               (if (reduced? result)
                 result
                 (recur result))))))))))



(defn decode-frame [decoder-context]
  (fn [rf]
    (let [frame (av_frame_alloc)]
      (fn
        ;; init
        ([]
          (rf))
        ;; completion
        ([result]
         (av_frame_free (doto (PointerByReference.)
                          (.setValue (.getPointer frame))))
         (rf result))
        ;; step
        ([result packet]
         (let [err (avcodec_send_packet decoder-context packet)
               result
               (if (and (not (zero? err))
                        (not= err -22)
                        (eagain? err))
                 (reduced {:error-code err
                           :type :send-packet-failure})
                 (loop [result result]
                   (assert frame)
                   (let [err (avcodec_receive_frame decoder-context frame)]
                     (cond
                       (or (zero? err)
                           (einvalid? err))
                       (let [result (rf result frame)]
                         (if (reduced? result)
                           result
                           (recur result)))

                       (eagain? err)
                       result
                       
                       (eof? err)
                       (reduced result)

                       ;; some other error
                       :else
                       (reduced {:error-code err
                                 :error-msg (error->str err)
                                 :type :decode-error})))))]
           result))))))



(defn encode-frame [encoder-context]
  (fn [rf]
    (let [packet (av_packet_alloc)]
      (fn
        ;; init
        ([]
         (rf))
        ;; completion
        ([result]
         (av_packet_free (doto (PointerByReference.)
                           (.setValue (.getPointer packet))))
         (rf result))
        ;; step
        ([result frame]
         (let [err (avcodec_send_frame encoder-context frame)
               result
               (if (and (not (zero? err))
                        (not= err -22)
                        (eagain? err))
                 (reduced {:error-code err
                           :type :send-packet-failure})
                 (loop [result result]
                   (let [err (avcodec_receive_packet encoder-context packet)]
                     (cond
                       (or (zero? err)
                           (einvalid? err))
                       (let [result (rf result packet)]
                         (if (reduced? result)
                           result
                           (recur result)))

                       (eagain? err)
                       result
                       
                       (eof? err)
                       (reduced result)

                       ;; some other error
                       :else
                       (reduced {:error-code err
                                 :error-msg (error->str err)
                                 :type :encode-error})))))]
           result))))))

(defn write-packet [output-format-context]
  (let [err (avformat_write_header output-format-context nil)]
    (when (neg? err)
      (throw (Exception.))))

  (fn
    ([])
    ([result]
     (let [result
           (loop []
             (let [err (av_write_frame output-format-context nil)]
               (cond

                 (neg? err)
                 (reduced {:error-code err
                           :type :write-flush-error})

                 ;; we're done
                 (= 1 err)
                 nil

                 ;; continue flushing
                 :else
                 (recur))))

           result (if (reduced? result)
                    result
                    (let [err (av_write_trailer output-format-context)]
                      (if (zero? err)
                        nil
                        (reduced {:error-code err
                                  :type :write-tailer-error}))))]
       result))
    ([result packet]
     (let [err (av_write_frame output-format-context packet)
           result (if (neg? err)
                    (reduced {:error-code err
                              :type :write-error})
                    ;; else
                    nil)]
       result))))

(defn filename->format-context [rf]
  (fn
    ([] (rf))
    ([result] (rf result))
    ([result fname]
     (let [format-ctx (avformat_alloc_context)
           format-ctx* (doto (PointerByReference.)
                         (.setValue (.getPointer format-ctx)))
           err (avformat_open_input format-ctx* fname nil nil)]
       (if (zero? err)
         (let [result (rf result format-ctx)]
           (avformat_close_input format-ctx*)
           result)
         (reduced {:error-code err}))))))


(defn open-context [fname]
  (let [format-ctx (avformat_alloc_context)
        _ (when (nil? format-ctx)
            (throw (ex-info "Error allocating format context."
                            {:filename fname})))
        format-ctx* (doto (PointerByReference.)
                      (.setValue (.getPointer format-ctx)))
        err (avformat_open_input format-ctx* fname nil nil)]
    (if (zero? err)
      format-ctx
      (throw (ex-info "Error opening format context"
                      {:error-code err})))))

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
        num-streams (.readField format-context "nb_streams")
        streams (.getPointerArray
                 (.readField format-context "streams")
                 0 num-streams)
        stream (aget streams best-stream)
        stream+ (Structure/newInstance com.phronemophobic.clj_media.audio.structs.AVStreamByReference
                                       stream)
        codec-parameters (.readField stream+ "codecpar")
        codec-id (.readField codec-parameters "codec_id" )
        decoder (avcodec_find_decoder codec-id)
        _ (when (nil? decoder)
            (throw (ex-info "Could not find decoder"
                            {:codec-id codec-id})))
        decoder-context (avcodec_alloc_context3 (.getPointer decoder))
        _ (when (nil? decoder-context)
            (throw (ex-info "Could not allocate decoder"
                            {})))]
    (avcodec_parameters_to_context decoder-context codec-parameters)
    (let [err (avcodec_open2 decoder-context decoder nil)]
      (when (neg? err)
        (throw (Exception. "Could not open codec"
                           {:error-code err})))
      (prn "codec id: " codec-id)
      decoder-context)))

(defn open-output-context [fname]
  (let [output-io-context* (PointerByReference.)
        err (avio_open output-io-context*
                       fname
                       AVIO_FLAG_WRITE)
        _ (when (neg? err)
            (throw (Exception.)))

        output-io-context (Structure/newInstance AVIOContextByReference
                                                 (.getValue output-io-context*))
        
        output-format (av_guess_format nil fname nil)
        _ (when (nil? output-format)
            (throw (Exception. "Could not guess output format")))

        fname-bytes (.getBytes fname "utf-8")
        url-mem (ref!
                 (doto (Memory. (inc (alength fname-bytes)))
                   (.write 0 fname-bytes 0 (alength fname-bytes))
                   (.setByte (alength fname-bytes) 0)))
        url (doto (ByteByReference.)
              (.setPointer url-mem))

        output-format-context (avformat_alloc_context)
        output-format-context (doto output-format-context
                                (.writeField "oformat" (.getPointer output-format))
                                (.writeField "pb" (.getValue output-io-context*))
                                (.writeField "url" url)
                                )]
    output-format-context))

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

               (eagain? err)
               result

               (eof? err)
               (reduced result)

               :else
               (reduced {:error-code err
                         :error-msg (error->str err)
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

;; context -> packet --decode--> frame ----encode--> packet -> context?
(defn run
  ([]
   (run fname))
  ([fname]
   (with-free [input-format-context (open-context fname)
               decoder-context (find-decoder-context :audio input-format-context)
               #_#_#_#_output-format-context (open-output-context (str "file://"
                                                               (.getAbsolutePath (io/file fname))))

               encoder-context (find-encoder-codec output-format-context
                                                   {:sample-fmt (.readField decoder-context "sample_fmt")
                                                    :sample-rate (.readField decoder-context "sample_rate")
                                                    ;; AV_SAMPLE_FMT_S16
                                                    :channel-layout (.readField decoder-context "channel_layout")
                                                    :bit-rate (.readField decoder-context "bit_rate")})]
     (try
       (transduce (comp read-frame
                          (decode-frame decoder-context)

                          (transcode-frame decoder-context AV_SAMPLE_FMT_S16)
                          (frame->buf AV_SAMPLE_FMT_S16)
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



