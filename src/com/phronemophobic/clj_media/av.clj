(ns com.phronemophobic.clj-media.av
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.gen.jna :as gen]
            [com.phronemophobic.clj-media.av.raw :as raw
             :refer :all]
            [com.rpl.specter :as specter])
  (:import
   java.io.PushbackReader
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.IntByReference
   com.sun.jna.ptr.ByteByReference
   com.sun.jna.Structure)
  (:gen-class))

(raw/import-structs!)

(defonce handles (atom #{}))
(defn ref! [o]
  (swap! handles conj o)
  o)


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
        stream+ (Structure/newInstance AVStreamByReference
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



