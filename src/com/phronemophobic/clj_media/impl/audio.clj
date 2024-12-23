(ns com.phronemophobic.clj-media.impl.audio
  (:require [clojure.java.io :as io]
            [com.phronemophobic.clj-media.impl.av :as av]
            [clojure.core.async :as async]
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
   java.io.ByteArrayOutputStream

   (javax.sound.sampled AudioFormat
                        AudioFormat$Encoding
                        AudioInputStream
                        AudioSystem
                        DataLine
                        DataLine$Info
                        Port$Info
                        Line
                        LineUnavailableException
                        SourceDataLine
                        TargetDataLine
                        UnsupportedAudioFileException
                        Mixer
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
                       (.setValue (Structure/.getPointer format-ctx))))

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


(defn default-stereo-format []
  (let [sample-rate 44100
        sample-size-in-bits 16
        channels 2
        frame-size (* channels (/ sample-size-in-bits 8))

        frame-rate 44100
        big-endian? false
        audio-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                   sample-rate
                                   sample-size-in-bits
                                   channels
                                   frame-size
                                   frame-rate
                                   big-endian?)]
    audio-format))

(defn default-mono-format []
  (let [sample-rate 44100
        sample-size-in-bits 16
        channels 1
        frame-size (* channels (/ sample-size-in-bits 8))

        frame-rate 44100
        big-endian? false
        audio-format (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                   sample-rate
                                   sample-size-in-bits
                                   channels
                                   frame-size
                                   frame-rate
                                   big-endian?)]
    audio-format))

(defn play-sound
  ([]
   (play-sound {}))
  ([opts]
   (let [audio-format (or (:audio-format opts)
                          (default-stereo-format))
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
       ([read-bytes ^byte/1 buf]
        (+ read-bytes
           (.write source-data-line buf 0 (alength buf))))))))



(defn frame->buf [sample-format]
  (let [bytes-per-sample (av_get_bytes_per_sample sample-format)]
    (map (fn [frame]
           (let [buf-size (* bytes-per-sample
                             (:nb_samples frame )
                             (:channels frame ))
                 buf (-> (nth (:data frame ) 0)
                         Structure/.getPointer
                         (.getByteArray 0 buf-size))]
             
             buf)))))

(defn resample2 [input-format output-format]
  (let [resample-ctx* (PointerByReference. Pointer/NULL)
        err (swr_alloc_set_opts2 resample-ctx*
                                 (Structure/.getPointer
                                    (:ch-layout output-format))
                                 (:sample-format output-format)
                                 (:sample-rate output-format)
                                 (Structure/.getPointer
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
        (let [{:keys [sample-format
                      sample-rate]}
              output-format
              ch-layout (AVChannelLayout.)]
          ;; create out own copy since original copy may change :(
          (av_channel_layout_copy (Structure/.getPointer ch-layout)
                                  (Structure/.getPointer (:ch-layout output-format)))
          (comment
            (supers AVChannelLayout)
            )
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
                       (Structure/.getPointer (:ch_layout frame))
                       (Structure/.getPointer ch-layout))))
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
    (assert (<= num-output-channels (alength ^byte/1 (:data (AVFrame.)))))
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
                                       (map (fn [^AVChannelLayout p]
                                              (when p
                                                (.share (Structure/.getPointer p)
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
                                                  (.share (Structure/.getPointer p)
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

(defn frame->buf [frame]
  (assert
   (not= 1 (av_sample_fmt_is_planar (:format frame)))
   "Getting buffer data from planar frames not supported.")
  (let [buf-size (first (:linesize frame))
        buf (-> (:data frame)
                (nth 0)
                (Structure/.getPointer)
                (.getByteBuffer 0 buf-size))]
    buf))



(defn default-recording-audio-format []
  (let [
        sample-rate 44100
        sample-size-in-bits 16
        ;; mono!
        channels 1
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
                                   big-endian?)]
    audio-format))

(defn get-microphone-mixer
  ^Mixer []
  (->> (AudioSystem/getMixerInfo)
       (filter #(= "MacBook Air Microphone"
                   (.getName %)))
       (map #(AudioSystem/getMixer %))
       
       first))

(defn record-audio
  ([]
   (record-audio {}))
  ([opts]
   (let [
         audio-format (or (:audio-format opts)
                          (default-mono-format))


         line-info (DataLine$Info. TargetDataLine audio-format)
         ^TargetDataLine
         ;; line (.getLine (get-microphone-mixer) line-info)
         line (AudioSystem/getLine line-info)
         ;; line (AudioSystem/getLine Port$Info/MICROPHONE)
         ;; _ (assert (.matches (default-recording-audio-format) (.getFormat line)))

         _ (.open line (default-recording-audio-format))

         out (ByteArrayOutputStream.)
         buf-size (quot (.getBufferSize line) 5)
         ;; The number of bytes to be read must represent an integral number of sample frames
         buf-size (- buf-size (mod buf-size (-> (.getFormat line) (.getFrameSize))))
         
         buf (byte-array buf-size)
         running? (atom true)]

     (.start line)
     (async/thread
       (loop []
         (when @running?
           (let [bytes-read (.read line buf 0 (alength buf))]
             (.write out buf 0 bytes-read)
             (recur)))))

     (fn []
       (reset! running? false)
       (.stop line)
       (.drain line)
       (loop []
         (let [bytes-read (.read line buf 0 (alength buf))]
           (when (pos? bytes-read)
             (.write out buf 0 bytes-read)
             (recur))))
       (.toByteArray out)))))

(comment
  (def line-info (DataLine$Info. TargetDataLine (default-recording-audio-format)))
  (AudioSystem/isLineSupported line-info)


  (def get-bytes (record-audio {}))

  (def my-bytes (get-bytes))

  (every? zero? my-bytes)

  (with-open [fos (java.io.FileOutputStream. "foo.wav")]
    (.write fos my-bytes))

  (def mixinfos (AudioSystem/getMixerInfo))

  (def rf (play-sound {:audio-format (default-mono-format)}))
  (rf 0 my-bytes)
  (rf 0)

  ,)


(defn ^:private print-supported-formats []
  (let [mixers (AudioSystem/getMixerInfo)]
    (doseq [info mixers]
      (let [mixer (AudioSystem/getMixer info)
            line-info (AudioSystem/getTargetLineInfo (DataLine$Info. TargetDataLine (AudioFormat. 44100 16 1 true false)))]
        (when (seq line-info)
          (println "Mixer name:" (.getName info))
          (doseq [format (map #(.getFormats %) line-info)]
            (doseq [f format]
              (println "Supported format:" f))))))))

;; Call the function to print supported formats
;; (print-supported-formats)

(defn -main [& arg]

  (let [get-bytes (record-audio {})]
    (Thread/sleep 2000)
    (prn (every? zero? (get-bytes)))))
