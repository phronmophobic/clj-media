(ns com.phronemophobic.clj-media.impl.flow
  (:require [clojure.core.async.flow :as flow]
            [clojure.core.async :as async]
            [com.phronemophobic.clj-media.impl.filter.media :as fm]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.raw :as raw]
            [tech.v3.tensor :as dtt]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [tech.v3.datatype.casting :as dt-casting])
  (:import
   ;; com.sun.jna.Pointer
   ;;         com.sun.jna.ptr.PointerByReference
   ;;         com.sun.jna.Structure
   ;; (com.phronemophobic.clj_media.impl.raw.structs
   ;;          AVOutputFormatByReference
   ;;          AVFormatContextByReference
   ;;          AVStreamByReference
   ;;          AVIOContextByReference)
))

(defprotocol IEOFData
  (eof-data? [_]))

(extend-protocol IEOFData
  Object
  (eof-data? [_] false))

(defrecord AEOFData []
  IEOFData
  (eof-data? [_] true))

(defn make-eof-data []
  (->AEOFData))


(defn queue
  "Create an empty persistent queue or a persistent queue from a sequence."
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([xs] (into (queue) xs)))

;; * Problems
;; - no cleanup yet. at least need to close, unref, and free memory  
;; - resources like format-contexts are scattered and not managed by the flow

;; * Use cases
;; copy a media file
;; remove audio/video
;; transcode media file
;; create media from raw frame data
;; adjust audio volume
;; crop video
;; trim media
;; speed up media

;; Encoding Packets and Writing packets to a file are distinct
;; but the format-context also needs codec parameters set for each stream.  




;; * Input
;; use file to create a format context
;; format contexts can scane for stream infos


;; * Output
;; can be guessed based on filename
;; can be explicitly passed


;; need to somehow connect inputs with outputs

;; decoding
;; format context creates packets
;; each packet has a stream index
;; you can create decoder context for a stream
;; decoder contexts can convert packets into frames.

;; decoding flow processes
;; 1. format-context
;;    inputs: none (except config)
;;    outputs: packets, one for each stream
;; 2. decoder context (one for each stream)
;;    inputs: packets
;;    ouputs: frames






;; (raw/avformat_seek_file)

(defonce all-flows (atom []))
(defn stop-all-flows! []
  (let [[flows _] (reset-vals! all-flows [])]
    (doseq [flow flows]
      (flow/stop flow))))
(defn track-flow [flow]
  (swap! all-flows conj flow)
  flow)

(comment
  (stop-all-flows!)
  ,)


(defn monitoring [{:keys [report-chan error-chan]}]
  (prn "========= monitoring start")
  (async/thread
    (loop []
      (let [[val port] (async/alts!! [report-chan error-chan])]
        (if (nil? val)
          (prn "========= monitoring shutdown")
          (do
            (prn (str "======== message from " (if (= port error-chan) :error-chan :report-chan)))
            (clojure.pprint/pprint (select-keys val [::flow/pid ::flow/cid ::flow/ex]))
            (tap> val)
            
            (recur))))))
  nil)

(defn wrap-producer
  "given a process map. Return a process that allows process to recur when state contains a true ::produce key."
  [{:keys [describe init transition transform]}]
  {:describe describe
   :init
   (fn [m]
     (let [ch (async/chan (async/sliding-buffer 1))]
       (-> (if init (init m) m)
           (update ::flow/in-ports
                   assoc
                   ::kickstart ch)
           (update ::flow/out-ports
                   assoc ::recur ch))))
   :transition
   (fn [state status]
     (let [state (if transition
                   (transition state status)
                   state)]
       (when (and (::produce state)
                  (= status ::flow/resume))
         (let [kickstart-ch (-> state
                                ::flow/in-ports
                                ::kickstart)]
           (async/>!! kickstart-ch true)))
       (when (= status ::flow/stop)
         (let [kickstart-ch (-> state
                                ::flow/in-ports
                                ::kickstart)]
           (async/close! kickstart-ch true)))
       state))
   :transform
   (fn [state in msg]
     (let [[state outs] (transform state in msg)
           
           outs (if (::produce state)
                  (conj (into [] outs)
                        [::recur [true]])
                  outs)]
       [state outs]))})


{:describe (fn []
             {:outs {:packet "Packets from file."}})
 :init (fn [m]
         m)
 :transition (fn [m] m)
 :transform
 (fn [state in msg]
   state)
 }

(defn ^:private format-context-streams* [format-ctx]
  (let [
        num-streams (:nb_streams format-ctx)
        streams (-> (native-buffer/wrap-address (:streams format-ctx)
                                                (* 8 num-streams))
                    (native-buffer/set-native-datatype :uint64))
        ;; streams (.getPointerArray
        ;;          (.readField format-ctx "streams")
        ;;          0 num-streams)
]
    (into []
          (map #(dt-ffi/ptr->struct :AVStream (dt-ffi/->pointer %)))
          streams)))

(defn ^:private find-stream-info* [format-ctx]
  (let [err (raw/avformat_find_stream_info format-ctx nil)
        _ (when (not (zero? err))
            (throw (ex-info "Could not find stream info."
                            {:error-code err})))]
    nil))


(defn open-context [fname]
  (let [format-ctx (raw/avformat_alloc_context)
        _ (when (nil? format-ctx)
            (throw (ex-info "Error allocating format context."
                            {:filename fname})))
        
        format-ctx* (dt-ffi/make-ptr :pointer (-> format-ctx dt-ffi/->pointer .address))

        err (raw/avformat_open_input format-ctx* (dt-ffi/string->c fname) nil nil)]
    (if (zero? err)
      (reify 
        dt-ffi/PToPointer
        (convertible-to-pointer? [_] true)
        (->pointer [_] (dt-ffi/->pointer format-ctx))
        
        clojure.lang.ILookup
        (valAt [_ k]
          nil
          (case k
            :streams (format-context-streams* format-ctx)
            
            ;; else
            nil))
        java.lang.AutoCloseable
        (close [_]
          (raw/avformat_close_input format-ctx*)
          ;; use value of format-ctx*, which may be nulled by close_input
          ;; it's possible that free_context is redundant with close_input
          (raw/avformat_free_context (first format-ctx*))))
      
      #_(proxy [Pointer
              clojure.lang.ILookup
              java.lang.AutoCloseable]
        [(Pointer/nativeValue (.getPointer format-ctx))]
        (valAt [k]
          nil
          (case k
            :streams (format-context-streams* format-ctx)
            
            ;; else
            nil))
        (close []
          (raw/avformat_close_input format-ctx*)
          ;; use value of format-ctx*, which may be nulled by close_input
          ;; it's possible that free_context is redundant with close_input
          (raw/avformat_free_context (first format-ctx*))))
      (do
        (raw/avformat_close_input format-ctx*)
        ;; use value of format-ctx*, which may be nulled by close_input
        ;; it's possible that free_context is redundant with close_input
        (raw/avformat_free_context (first format-ctx*))
        (throw (ex-info "Error opening format context"
                      {:error-code err}))))))

(defn open-output-context [fname]
  (let [output-io-context* (dt-ffi/make-ptr :pointer 0)
        fname* (dt-ffi/string->c fname)
        err (raw/avio_open output-io-context*
                           fname*
                           raw/AVIO_FLAG_WRITE)
        _ (when (neg? err)
            (throw (Exception.)))

        
        output-format-context* (dt-ffi/make-ptr :pointer 0)
        err (raw/avformat_alloc_output_context2 output-format-context*
                                                nil
                                                nil
                                                fname*)
        _ (when (neg? err)
            (raw/avio_closep output-io-context*)
            (throw (Exception. "Could not create output context.")))
        output-format-context (dt-ffi/ptr->struct :AVFormatContext
                                                  (first output-format-context*))
        output-format-context (doto output-format-context
                                (.put :pb (first output-io-context*)))]
    output-format-context
    (reify
      dt-ffi/PToPointer
      (convertible-to-pointer? [_] true)
      (->pointer [_] (dt-ffi/->pointer output-format-context))
      clojure.lang.ILookup
      (valAt [_ k]
        (case k
          :oformat (:oformat output-format-context)
          :pb (:pb output-format-context)
          
          ;; else
          nil))
      java.lang.AutoCloseable
      (close [_]
        (raw/avio_closep output-io-context*)
        (raw/avformat_free_context output-format-context)))

    #_(proxy [Pointer
            clojure.lang.ILookup
            java.lang.AutoCloseable]
      [(Pointer/nativeValue (.getPointer output-format-context))]
        (valAt [k]
        (case k
          :oformat (:oformat output-format-context)
          :pb (:pb output-format-context)
          
          ;; else
          nil))
      (close []
        (raw/avio_closep output-io-context*)
        (raw/avformat_free_context output-format-context)))))



(defn stream->decoder-ctx [stream]
  (let [codec-parameters (dt-ffi/ptr->struct 
                          :AVCodecParameters
                          (:codecpar stream))
        codec-id (:codec_id codec-parameters)

        decoder (raw/avcodec_find_decoder codec-id)
        _ (when (nil? decoder)
            (throw (ex-info "Could not find decoder"
                            {:codec-id codec-id})))
        decoder-ctx (raw/avcodec_alloc_context3 decoder)
        
        _ (when (nil? decoder-ctx)
            (throw (ex-info "Could not allocate decoder"
                            {})))
        _ (doto decoder-ctx
            (.put :time_base (:time_base stream)))
        
        _ (raw/avcodec_parameters_to_context decoder-ctx codec-parameters)
        err (raw/avcodec_open2 decoder-ctx decoder nil)
        _ (when (neg? err)
            (throw (Exception. "Could not open codec"
                               {:error-code err})))
        
        format (merge {:time-base (:time_base stream)}
                      (av/codec-context-format decoder-ctx))
        
        time-base (condp = (:codec_type decoder-ctx)
                    raw/AVMEDIA_TYPE_AUDIO [1 (:sample-rate format)]
                    raw/AVMEDIA_TYPE_VIDEO (let [tb (:time_base stream)]
                                         [(:num tb) (:den tb)]))
        format (assoc format
                      :time-base
                      (av/->avrational (first time-base)
                                       (second time-base)))
        ]
    (reify 
      dt-ffi/PToPointer
      (convertible-to-pointer? [_] true)
      (->pointer [_] (dt-ffi/->pointer decoder-ctx))
      clojure.lang.ILookup
      (valAt [_ k]
        (case k
          :format format
          
          ;; else
          nil))
      java.lang.AutoCloseable
      (close [_]
        (raw/avcodec_free_context (dt-ffi/make-ptr :pointer decoder-ctx))))
    ;; (proxy [Pointer
    ;;         clojure.lang.ILookup
    ;;         java.lang.AutoCloseable]
    ;;   [(Pointer/nativeValue (.getPointer decoder-ctx))]
    ;;   (valAt [k]
    ;;     (case k
    ;;       :format format
          
    ;;       ;; else
    ;;       nil))
    ;;   (close []
    ;;     (raw/avcodec_free_context (PointerByReference. decoder-ctx))))
))

(comment
  
  (open-context "")
  ,)
(def media-fname
  ;;"../bowsertalk/bowsertalkv3-fall-udpated-final3.mp4"
  "../clj-media/my-fade-in-out.mp4"
)



(defn media-file []
  (wrap-producer
   {:describe (fn []
                {:ins {;; :fresh-packet "allocated packets."
                       }
                 :params {:format-context "The format context to read from."
                          :fresh-packet-chan "Channel to acquire fresh packets."}
                 :outs {:packet "Packets from file."}})
    :init (fn [{:keys [fresh-packet-chan] :as state}]
            (assoc state
                   :fresh-packets (queue)
                   ::flow/in-ports {:fresh-packet fresh-packet-chan}
                   ;; :format-context (open-context fname)
                   ::produce false))
    :transition (fn [state status] 
                  (if (= status ::flow/stop)
                    ;; (do
                    ;;   (when-let [ctx (:format-context state)]
                    ;;     (java.lang.AutoCloseable/.close ctx))
                    ;;   (dissoc state :format-context))
                    state
                    state))
    :transform
    (fn [state in msg]
      (case in
        :fresh-packet
        [(-> state
             (update :fresh-packets conj msg)
             (assoc ::produce true))]
        ;; else
        (let [packet (peek (:fresh-packets state))
              state (update state :fresh-packets pop)
              state (if (seq (:fresh-packets state))
                      state
                      (assoc state ::produce false))
              
              err (raw/av_read_frame (:format-context state) packet)]
          (cond
            (zero? err) [state {:packet [packet]}]
            (av/eof? err) 
            (do
              (prn "sending packet eof!")
              [(assoc state
                      ::produce false
                      ::flow/input-filter (constantly false))
               {:packet [(make-eof-data)]}])
            
            :else
            (throw (ex-info "Error reading file"
                            {:error-code err
                             :error-msg (av/error->str err)
                             :type :decode-error}))))))})) 

(defn ^:private update-packet-decoder-input-filter [state]
  (let [{:keys [fresh-frames pending-frames? frame-buf-size]} state
        has-frame-space? (< (count fresh-frames) frame-buf-size)]
    (if (and has-frame-space?
             (not pending-frames?))
      (dissoc state ::flow/input-filter)
      (assoc state ::flow/input-filter
             (fn [cid]
               (case cid
                 :packet (and (not pending-frames?) (seq fresh-frames))
                 :fresh-frame has-frame-space?
                 true))))))

(defn ^:private flush-frames
  ([state]
   (flush-frames state {}))
  ([state outs]
   (if (not (:pending-frames? state))
     [(update-packet-decoder-input-filter state) outs]
     (let [decoder-ctx (:decoder-ctx state)
           [fresh-frames output-frames pending-frames? eof?]
           (loop [fresh-frames (:fresh-frames state)
                  output-frames []]
             (if-let [frame (peek fresh-frames)]
               (let [err (raw/avcodec_receive_frame decoder-ctx frame)]
                 (cond
                   (or (zero? err)
                       ;;(av/einvalid? err)
                       )
                   (recur (pop fresh-frames)
                          (conj output-frames frame))
                   
                   (av/eagain? err)
                   [fresh-frames output-frames false]
                   
                   (av/eof? err)
                   (do
                     (prn "sending frame eof")
                     [fresh-frames (conj output-frames (make-eof-data)) false true])
                   
                   ;; some other error
                   :else
                   (throw (ex-info
                           "Error decoding packet"
                           {:error-code err
                            :error-msg (av/error->str err)
                            :type :decode-error}))))
               ;; else, have pending frames, but no frame
               [fresh-frames output-frames true]))
           
           state (assoc state
                        :fresh-frames fresh-frames
                        :pending-frames? pending-frames?)
           state (if eof?
                   ;; state
                   (assoc state ::flow/input-filter (constantly false))
                   (update-packet-decoder-input-filter state))
           outs (assoc outs :frame output-frames)]
       [state outs]))))



(defn packet-decoder-proc []
  
  {:describe (fn []
               {:params {:decoder-ctx "The decoder ctx to use"
                         :frame-buf-size "Number of fresh frames to buffer"
                         :fresh-frame-chan "Channel to get fresh frames from."}
                :ins {;; :fresh-frame "Fresh frames to use"
                      :packet "packet to decode"}
                :outs {:frame "Decoded Frames"
                       :recycle-packet "Packet to recycle"}})
   :init (fn [{:keys [fresh-frame-chan frame-buf-size] :as state}]
           (assoc state
                  :frame-buf-size (or frame-buf-size 12)
                  :fresh-frames (queue)
                  ::flow/in-ports {:fresh-frame fresh-frame-chan}
                  ::flow/input-filter (fn [cid]
                                        (not= cid :packet))))
   :transition (fn [state status]
                 state)
   :transform
   (fn [state in msg]
     (case in
       :fresh-frame
       (-> state
           (update :fresh-frames conj msg)
           (flush-frames))
       :packet
       (let [packet msg
             decoder-ctx (:decoder-ctx state)
             _ (when (eof-data? packet)
                 (prn "got packet eof"))
             err (raw/avcodec_send_packet decoder-ctx 
                                          ;; nil signals flush
                                          (if (eof-data? packet)
                                            nil
                                            packet))
             _ (when (and (not (zero? err))
                          (not= err -22)
                          (not (av/eagain? err)))
                 (prn :error in (boolean ((::flow/input-filter state) in))
                      )
                 (throw (ex-info
                         "Error decoding packet"
                         {:error-code err
                          :error-msg (av/error->str err)
                          :type :decode-error})))
             
             state (assoc state :pending-frames? true)
             outs {:recycle-packet [packet]}]
         (flush-frames state outs))))})

(defn ^:private update-frame-encoder-input-filter [state]
  (let [{:keys [fresh-packets pending-packets? packet-buf-size]} state
        has-packet-space? (< (count fresh-packets) packet-buf-size)]
    
    (if (and has-packet-space?
             (not pending-packets?))
      (dissoc state ::flow/input-filter)
      (assoc state ::flow/input-filter
             (fn [cid]
               (case cid
                 :frame (and (not pending-packets?) (seq fresh-packets))
                 :fresh-packet has-packet-space?
                 true))))))

(defn ^:private flush-packets
  ([state]
   (flush-packets state {}))
  ([state outs]
   (if (not (:pending-packets? state))
     [(update-frame-encoder-input-filter state) outs]
     (let [encoder-ctx (:encoder-ctx state)
           [fresh-packets output-packets pending-packets? eof?]
           (loop [fresh-packets (:fresh-packets state)
                  output-packets []]
             (if-let [packet (peek fresh-packets)]
               (let [err (raw/avcodec_receive_packet encoder-ctx packet)]
                 (cond
                   (or (zero? err)
                       (av/einvalid? err))
                   (recur (pop fresh-packets)
                          (conj output-packets packet))
                   
                   (or (av/eof? err)
                       (and (av/eagain? err)
                            (:eof? state)))
                   (do
                     (prn "sending packet eof")
                     [fresh-packets (conj output-packets (make-eof-data)) false true])

                   (av/eagain? err)
                   [fresh-packets output-packets false]
                   
                   ;; some other error
                   :else
                   (throw (ex-info
                           "Error encoding frame"
                           {:error-code err
                            :error-msg (av/error->str err)
                            :type :decode-error}))))
               ;; else, have pending packets, but no packet
               [fresh-packets output-packets true]))
           
           state (assoc state
                        :fresh-packets fresh-packets
                        :pending-packets? pending-packets?)
           state (if eof?
                   (assoc state ::flow/input-filter (constantly false))
                   (update-frame-encoder-input-filter state))
           
           output-packets
           (into []
                 (map (fn [packet]
                        (if (eof-data? packet)
                          packet
                          (let [{:keys [duration pts]} packet
                                {:keys [stream input-format]} state]
                            (.put packet :time_base (:time_base stream))
                            (raw/av_packet_rescale_ts packet
                                                      (:time-base input-format)
                                                      (:time_base stream))))
                        packet))
                 output-packets)

           outs (assoc outs :packet output-packets)]
       [state outs]))))



(defn frame-encoder-proc []
  {:describe (fn []
               {:params {:encoder-ctx "The encoder ctx to use"
                         :packet-buf-size "Number of fresh packets to buffer"
                         :fresh-packet-chan "Channel to accept fresh packets"
                         
                         ;; used for rescaling ts
                         :stream ""
                         :input-format ""
                         }
                :ins {;; :fresh-packet "Fresh packets to use  "
                      :frame "frame to encode"}
                :outs {:packet "Decoded Frames"
                       :recycle-frame "frame to recycle"}})
   :init (fn [{:keys [fresh-packet-chan packet-buf-size] :as state}]
           (assoc state
                  :packet-buf-size (or packet-buf-size 12)
                  :fresh-packets (queue)
                  ::flow/in-ports {:fresh-packet fresh-packet-chan}
                  ::flow/input-filter (fn [cid]
                                        (not= cid :frame))))
   :transition (fn [state status]
                 state)
   :transform
   (fn [state in msg]
     (case in
       :fresh-packet
       (-> state
           (update :fresh-packets conj msg)
           (flush-packets))
       :frame
       (let [frame msg
             encoder-ctx (:encoder-ctx state)

             eof? (eof-data? frame)
             err (raw/avcodec_send_frame encoder-ctx 
                                         ;; nil signals flush
                                         (if eof?
                                           nil
                                           frame))
             _ (when (and (not (zero? err))
                          (not= err -22)
                          (not (av/eagain? err)))
                 (throw (ex-info
                           "Error encoding frame"
                           {:error-code err
                            :error-msg (av/error->str err)
                            :type :decode-error})))

             state (assoc state :pending-packets? true)
             state (if eof?
                     (assoc state :eof? true)
                     state)
             outs {:recycle-frame [frame]}]
         (flush-packets state outs))))})

(defn write-file-proc []
  {:describe (fn []
               {:params {:format-ctx ""
                         ;; :stream ""
                         ;; :input-format ""
                         }
                :ins {:packet ""}
                :outs {:recycle-packet ""
                       :done ""}})
   :init (fn [state]
           state)
   :transition (fn [state status]
                 (if (and (= status ::flow/resume)
                          (not (:wrote-header? state)))
                   (let [{:keys [format-ctx]} state
                         ;;err (raw/avformat_write_header format-ctx nil)
                         ]
                     #_(when (neg? err)
                       (throw (ex-info
                               "Error writing header"
                               {:error-code err
                                :error-msg (av/error->str err)})))
                     (assoc state :wrote-header? true))
                   ;; else
                   state))
   :transform
   (fn [{:keys [format-ctx] :as state} in packet]
     (let [eof? (eof-data? packet)
           err (raw/av_interleaved_write_frame format-ctx (if (eof-data? packet)
                                                            nil
                                                            packet))

           

           ;; err (raw/av_write_frame format-ctx (if (eof-data? packet)
           ;;                                        nil
           ;;                                      packet))

           _ (when (neg? err)
               (throw (ex-info
                       "Error writing file"
                       {:error-code err
                        :error-msg (av/error->str err)})))
           outs {:recycle-packet [packet]}]
       (if eof?
         (do
           (when (zero? err)
           (loop []
             (let [err (raw/av_write_frame format-ctx nil)]
               (cond
                 ;; done
                 (= 1 err) nil
                 (zero? err) (recur)
                 :else (throw (ex-info "error flushing"
                                       {:error-code err
                                        :error-msg (av/error->str err)}))))))

           (raw/av_write_trailer format-ctx)
           ;; (avio_closep)
           (raw/avio_closep 
            (dt-ffi/make-ptr :pointer (:pb format-ctx)))
           (raw/avformat_free_context format-ctx)

           (println "we done.")
           [ ;;(assoc state ::flow/input-filter (constantly false)                   ) 
            state
            (assoc outs :done [true])])
         [state outs])))})

(defn packet-recycler []
  (wrap-producer
   {:describe (fn []
                {:params {:n "Number of packets"
                          :fresh-packet-chan "Channel to put fresh packets on"}
                 :ins {:recycle-packet "Packets to recycle"}
                 :outs {;;:fresh-packet "Fresh packets"
                        }})
    :init (fn [{:keys [n fresh-packet-chan] :as state}]
            (assoc state
                   ::flow/out-ports {:fresh-packet fresh-packet-chan}
                   :fresh-packets (into (queue)
                                        (repeatedly n #(raw/av_packet_alloc)))
                   ::produce true))
    :transition (fn [state status] 
                  (if (= status ::flow/stop)
                    (do
                      (run! (fn [packet]
                              (raw/av_packet_free 
                               (dt-ffi/make-ptr :pointer packet)))
                            (:fresh-packets state))
                      (assoc state :fresh-packets (queue)))
                    state))
    :transform
    (fn [state in msg]
      (case in
        :recycle-packet
        (if (eof-data? msg)
          [state]
          (do
            (raw/av_packet_unref msg)
            [(-> state
                 (update :fresh-packets conj msg)
                 (assoc ::produce true))]))
        ;; else
        (let [packet (peek (:fresh-packets state))
              state (update state :fresh-packets pop)
              state (if (seq (:fresh-packets state))
                      state
                      (assoc state ::produce false))]
          [state {:fresh-packet [packet]}])))}))

;; 

(defn frame-recycler []
  {:describe (fn []
               {:params {:n "Number of frames"
                         :fresh-frame-chan "Channel to put fresh frames on."}
                :ins {:recycle-frame "frames to recycle"}
                :outs {;;:fresh-frame "Fresh frames"
                       
                       }})
   :init (fn [{:keys [n fresh-frame-chan] :as state}]
           (let [recycle-frame-internal (async/chan 10)]
             (async/go
              (try
                (loop [fresh-frames (into (queue)
                                          (repeatedly n #(raw/av_frame_alloc)))]
                  (let [ports [recycle-frame-internal]
                        ports (if (seq fresh-frames)
                                (conj ports [fresh-frame-chan (peek fresh-frames)])
                                ports)
                        [val port] (async/alts! ports)]
                    (cond
                      (= port recycle-frame-internal) (when-let [frame val]
                                                        (if (eof-data? frame)
                                                          (recur fresh-frames)
                                                          (do
                                                            (raw/av_frame_unref frame)
                                                            (recur (conj fresh-frames frame)))))
                      (= port fresh-frame-chan) (when val
                                                  (recur (pop fresh-frames)))
                      
                      :else (throw (ex-info "Unrecognized chan" {})))))
                (catch Exception e
                  (prn e))
                (finally
                  (prn "qutting frame recycler"))))
             
             (assoc state
                    ::flow/out-ports {;;:fresh-frame fresh-frame-chan
                                      :internal/recycle-frame recycle-frame-internal}
                    ;;::produce true
                    )))
   :transition (fn [state status] 
                 (if (= status ::flow/stop)
                   (do
                     #_(run! (fn [frame]
                               (raw/av_frame_free (PointerByReference. (.getPointer frame))))
                             (:fresh-frames state))
                     (assoc state :fresh-frames (queue)))
                   state))
   :transform
   (fn [state in msg]
     (case in
       :recycle-frame
       [state {:internal/recycle-frame [msg]}])
     
     #_(case in
         :recycle-frame
         (if (eof-data? msg)
           [state]
           (do
             (raw/av_frame_unref msg)
             [(-> state
                  (update :fresh-frames conj msg)
                  (assoc ::produce true))]))
         ;; else
         (let [frame (peek (:fresh-frames state))
               state (update state :fresh-frames pop)
               state (if (seq (:fresh-frames state))
                       state
                       (assoc state ::produce false))]
           [state {:fresh-frame [frame]}])))})

(defn counter-proc []
  (flow/map->step
   {:describe (fn []
                {:args {:prefix ""}
                 :ins {:in ""}})
    :init
    (fn [state]
      (assoc state :n 0))
    :transform
    (fn [{:keys [n prefix] :as state} in msg]
      (prn prefix (:n state))
      [(update state :n inc)])}))

(defn onto-chan-proc []
  (flow/map->step
   {:describe (fn [] {:ins {:in "  "}
                      :params {:chan "Channel to put values onto"}})
    :init (fn [m]
            (assoc m ::flow/out-ports {:out (:chan m)}))
    :transform (fn [_ _ v]
                 [_ {:out [v]}])}))

(def gdef
  {:procs
   {:media-packets
    {:proc (-> (media-file)
               flow/map->step
               flow/process)}
    :packet-recycler
    {:proc (-> (packet-recycler)
               flow/map->step
               flow/process)
     :args {:n 50}}
    :frame-recycler
    {:proc (-> (frame-recycler)
               flow/map->step
               flow/process)
     :args {:n 50}}
    
    :stream-filter
    {:proc (flow/process
            (flow/map->step
             {:describe (fn []
                          {:params {:stream-index "The stream index to filter for"}
                           :ins {:in ""}
                           :outs {:recycle-packet ""
                                  :out ""}})
              :init (fn [m] m)
              :transform
              (fn [state in packet]
                (if (or (= (:stream-index state) (:stream_index packet))
                        (eof-data? packet))
                  [state {:out [packet]}]
                  [state {:recycle-packet [packet]}]))}))}


    :onto-chan
    {:args {:chan (async/chan 10)}
     :proc
     (flow/process
      (flow/map->step
       {:describe (fn [] {:ins {:in "  "}
                          :params {:chan "Channel to put values onto"}})
        :init (fn [m]
                (assoc m ::flow/out-ports {:out (:chan m)}))
        :transform (fn [_ _ v]
                     [_ {:out [v]}])}))}

    :slow-packet-user
    {:proc
     (flow/process
      (flow/lift1->step
       (fn [packet]
         ;; (raw/av_new_packet packet 256)
         (prn
          {:pts (:pts packet)
           :dts (:dts packet)
           :stream_index (:stream_index packet)})
         (Thread/sleep (long 10))
         packet)))}

    :tap-sink
    {:proc (flow/process
            (flow/map->step
             {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
              :transform (fn [_ _ v]
                           (tap> [:flow v])
                           nil)}))}

    :frame-sink
    {:proc (flow/process
            (flow/lift1->step
             (fn [frame]
               
               (if (eof-data? frame)
                 (prn :eof)
                 (prn (:pts frame)))
               frame)))}
    
    :packet-sink
    {:proc (flow/process
            (flow/lift1->step
             (fn [packet]
               (if (eof-data? packet)
                 (prn :eof)
                 (prn :packet (:pts packet)))
               packet)))}

    :prn-sink
    {:proc (flow/process
            (flow/map->step
             {:describe (fn [] {:ins {:in "gimme stuff to print!"}})
              :transform (fn [_ _ v] (prn v))}))}}
   :conns
   [
    ;; [[:packet-recycler :fresh-packet] [:media-packets :fresh-packet]]
    [[:frame-sink :out] [:frame-recycler :recycle-frame]]
    [[:stream-filter :recycle-packet] [:packet-recycler :recycle-packet]]

    [[:packet-sink :out] [:packet-recycler :recycle-packet]]
    ;; [[:media-packets :packet] [:slow-packet-user :in]]
    
    
    ;; [[:slow-packet-user :out] [:packet-recycler :recycle-packet]]
    
    
    ,]
   
   ,})

(defn make-test-flow [done-ch]
  (let
    #_with-open
    [
     input-fname media-fname
     ;; "copy.mp4"
     output-fname "copy.mp4"
     stream-idx 1

     format-ctx (open-context input-fname)]
    
    
    (let [_ (find-stream-info* format-ctx)
          output-format-ctx (open-output-context output-fname)
          

          decoder-ctx (stream->decoder-ctx (nth (:streams format-ctx) stream-idx)) 
          output-format (fm/pick-output-format output-fname
                                               (dt-ffi/ptr->struct :AVOutputFormat (:oformat output-format-ctx))
                                               (:format decoder-ctx))
          encoder-ctx (av/encoder-context output-format)
          stream (av/add-stream output-format-ctx encoder-ctx)

          ;; output-format-ctx (open-output-context output-fname)
          ;; _ (doseq [stream [(second (:streams format-ctx))]
          ;;           :let [decoder-ctx (stream->decoder-ctx stream) 
                          
                          
          ;;                 output-format (fm/pick-output-format output-fname
          ;;                                                      (Structure/newInstance AVOutputFormatByReference
          ;;                                                                             (:oformat output-format-ctx))
          ;;                                                      (:format decoder-ctx))
          ;;                 encoder-ctx (av/encoder-context output-format)]]
              
          ;;          (av/add-stream output-format-ctx encoder-ctx))
          
          fresh-frame-chan (async/chan 12)
          fresh-packet-chan (async/chan 12)
          gdef (assoc-in gdef [:procs :frame-recycler :args :fresh-frame-chan] fresh-frame-chan)
          gdef (assoc-in gdef [:procs :packet-recycler :args :fresh-packet-chan] fresh-packet-chan)

          
          gdef (assoc-in gdef 
                         [:procs :media-packets :args] {:format-context format-ctx
                                                        :fresh-packet-chan fresh-packet-chan})
          gdef (assoc-in gdef 
                         [:procs :stream-filter :args :stream-index] stream-idx)
          
          
          gdef (update gdef
                       :procs
                       (fn [procs]
                         (assoc procs
                                :decoder
                                {:proc (-> (packet-decoder-proc)
                                           flow/map->step
                                           flow/process)
                                 :args {:decoder-ctx decoder-ctx
                                        :fresh-frame-chan fresh-frame-chan}})))
          
          gdef (update gdef
                       :procs
                       (fn [procs]
                         (assoc procs
                                :encoder
                                {:proc (-> (frame-encoder-proc)
                                           flow/map->step
                                           flow/process)
                                 :args {:encoder-ctx encoder-ctx
                                        :fresh-packet-chan fresh-packet-chan
                                        
                                        :stream stream
                                        :input-format (:format decoder-ctx)}})))
          

          gdef (update gdef
                       :procs
                       (fn [procs]
                         (assoc procs
                                :writer
                                {:proc (-> (write-file-proc)
                                           flow/map->step
                                           flow/process)
                                 :args {:format-ctx output-format-ctx
                                        }})))
          
          gdef (update gdef
                       :procs
                       (fn [procs]
                         (assoc procs
                                :done-proc
                                {:proc (-> (onto-chan-proc)
                                           flow/process)
                                 :args {:chan done-ch}})))

          
          gdef (update gdef
                       :procs
                       (fn [procs]
                         (assoc procs
                                :counter1
                                {:proc (-> (counter-proc)
                                           flow/process)
                                 :args {:prefix :read}}
                                :counter2
                                {:proc (-> (counter-proc)
                                           flow/process)
                                 :args {:prefix :write}}
                                :counter3
                                {:proc (-> (counter-proc)
                                           flow/process)
                                 :args {:prefix :frame}})))
          gdef (update gdef
                       :conns
                       (fn [conns]
                         (conj conns
                               [[:media-packets :packet] [:stream-filter :in]]
                               [[:stream-filter :out] [:decoder :packet]]
                               ;; [[:frame-recycler :fresh-frame] [:decoder :fresh-frame]]
                               [[:decoder :recycle-packet] [:packet-recycler :recycle-packet]]
                               ;; [[:decoder :recycle-packet] [:tap-sink :in]]
                               
                               
                               ;;[[:decoder :frame] [:frame-sink :in]]
                               [[:decoder :frame] [:encoder :frame]]
                               ;; [[:encoder :packet] [:packet-sink :in]]
                               ;; [[:packet-recycler :fresh-packet] [:encoder :fresh-packet]]
                               [[:encoder :recycle-frame] [:frame-recycler :recycle-frame]]
                               
                               [[:encoder :packet] [:writer :packet]]
                               [[:writer :done] [:done-proc :in]]
                               [[:writer :recycle-packet] [:packet-recycler :recycle-packet]]
                               
                               
                               ;; counters
                               ;; [[:stream-filter :out] [:counter1 :in]]
                               ;; [[:encoder :packet] [:counter2 :in]]
                               ;; [[:decoder :frame] [:counter3 :in]]
                               )))
          
          _   (raw/avformat_write_header output-format-ctx nil)
          ]
      
      gdef)))

(defn -main []
  (let [done-ch (async/chan)        
        flow (flow/create-flow (make-test-flow done-ch))]
    (-> flow flow/start monitoring)
    (flow/resume flow)
    
    (async/<!! done-ch)
    (flow/stop flow)))

(comment
  
  (av/probe media-fname)

  nil

  (def tflow (make-test-flow (async/chan 1)))
  (tap> tflow)

  (av/probe "part1-combined.mp4")

  

  ;; Make subflows for all sorts of use cases
  
  ;; file -> packets
  ;; file -> frames[stream-count]
  ;; packets -> file
  ;; frames[stream-count] -> file
  
  ;; filters frame -> frame

  ,)

(defn file->packets-flow [fname])
(defn file->frames-flow [fname])
;; how to deal with more than 2 streams?
(defn frames->file [fname {:keys [audio-format video-format] :as opts}])
