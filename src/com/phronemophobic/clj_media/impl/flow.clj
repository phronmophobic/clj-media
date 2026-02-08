(ns com.phronemophobic.clj-media.impl.flow
  (:refer-clojure :exclude [prn])
  (:require [clojure.core.async.flow :as flow]
            [clojure.core.async.flow.spi :as flow.spi]
            [clojure.core.async :as async]
            [clojure.java.io :as io]
            [com.phronemophobic.clj-media.impl.filter.media :as fm]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.raw :as raw]
            [com.phronemophobic.clj-media.impl.audio :as audio]
            [com.phronemophobic.clj-media.impl.datafy :as media.datafy]
            [com.phronemophobic.clj-media.impl.filter.avfilter :as avfilter]
            [tech.v3.tensor :as dtt]
            tech.v3.resource
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [tech.v3.datatype.protocols :as dtype-proto]
            [tech.v3.datatype.casting :as dt-casting])
  (:import java.util.Map))

(def my-filter-name "aecho")
(def media-fname "../clj-media/my-fade-in-out.mp4")

(defn prn [& args]
  (locking clojure.core/prn
    (apply clojure.core/prn args)))

(defn first-by [xf coll]
  (transduce xf (completing (fn [_ x] (reduced x))) nil coll))


(defonce in-transform (atom {}))
(defn wrap-in-transform [f]
  (fn [state in msg]
    (swap! in-transform assoc (::flow/pid state) true)
    (let [result (f state in msg)]
      (swap! in-transform assoc (::flow/pid state) false)
      result)))


(defn queue
  "Create an empty persistent queue or a persistent queue from a sequence."
  ([] clojure.lang.PersistentQueue/EMPTY)
  ([xs] (into (queue) xs)))

;; * Todo
;; - make sure packets and frames get cleaned up when flows stop.
;; - need to add seeking to reading files(raw/avformat_seek_file)
;; - eventually need to remove spurious logging
;; - need to document all flows.
;; - make names consistent 
;; - try to add back pressure by synchronizing on pts when writing to file
;; - make sure context vs ctx is used consistently
;; - update impl.raw so that functions that pass strings don't need dt-ffi/string->c

;; - need to figure the right way to set pts
;; copying code from `filter.media` sets audio-pts, but not for video?
;; but for video, the pts is rescaled.
;; maybe we just need to do a better job of settings time_base on packets/frames?
;; and rescaling as appropriate.

;; * Use cases
;; copy a media file
;; remove audio/video
;; transcode media file
;; create media from raw frame data
;; adjust audio volume
;; crop video
;; trim media
;; speed up media
;; synthesize a 440hz tone
;; synthesize a video from raw pixel data
;; make a video player ui component



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

(defn wrap-transform-tap [f]
  (fn [& args]
    (tap> [:transform args])
    (let [result (apply f args)]
      (tap> [:transform-done args result])
      result)))

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
           (async/put! kickstart-ch true)))
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

(defn ^:private format-context-streams* [format-context]
  (let [num-streams (:nb_streams format-context)

        num-bytes (* 8 num-streams)

        streams (-> (native-buffer/wrap-address (:streams format-context)
                                                num-bytes)
                    (native-buffer/set-native-datatype :uint64))]
    (into []
          (map #(dt-ffi/ptr->struct :AVStream (dt-ffi/->pointer %)))
          streams)))

(defn ^:private find-stream-info* [format-context]
  (let [err (raw/avformat_find_stream_info format-context nil)
        _ (when (not (zero? err))
            (throw (ex-info "Could not find stream info."
                            {:error-code err})))]
    nil))


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
            nil))
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

(defn open-output-context [fname oformat]
  (let [output-io-context* (dt-ffi/make-ptr :pointer 0)
        fname* (dt-ffi/string->c fname)
        err (raw/avio_open output-io-context*
                           fname*
                           raw/AVIO_FLAG_WRITE)
        _ (when (neg? err)
            (throw (Exception.)))

        
        output-format-context* (dt-ffi/make-ptr :pointer 0)
        err (raw/avformat_alloc_output_context2 output-format-context*
                                                oformat
                                                nil
                                                fname*)
        _ (when (neg? err)
            (raw/avio_closep output-io-context*)
            (throw (Exception. "Could not create output context.")))
        output-format-context (dt-ffi/ptr->struct :AVFormatContext
                                                  (first output-format-context*))
        output-format-context (doto output-format-context
                                (Map/.put :pb (first output-io-context*)))]
    output-format-context
    (reify
      dt-ffi/PToPointer
      (convertible-to-pointer? [_] true)
      (->pointer [_] (dt-ffi/->pointer output-format-context))
      clojure.lang.ILookup
      (valAt [_ k]
        (get output-format-context k))
      java.lang.AutoCloseable
      (close [_]
        (raw/avio_closep output-io-context*)
        (raw/avformat_free_context output-format-context)))))


(defn stream->decoder-context [stream]
  (let [codec-parameters (dt-ffi/ptr->struct 
                          :AVCodecParameters
                          (:codecpar stream))
        codec-id (:codec_id codec-parameters)

        decoder (raw/avcodec_find_decoder codec-id)
        _ (when (nil? decoder)
            (throw (ex-info "Could not find decoder"
                            {:codec-id codec-id})))
        decoder-context (raw/avcodec_alloc_context3 decoder)
        
        _ (when (nil? decoder-context)
            (throw (ex-info "Could not allocate decoder"
                            {})))
        _ (doto decoder-context
            (Map/.put :time_base (:time_base stream)))
        
        _ (raw/avcodec_parameters_to_context decoder-context codec-parameters)
        err (raw/avcodec_open2 decoder-context decoder nil)
        _ (when (neg? err)
            (throw (Exception. "Could not open codec"
                               {:error-code err})))
        
        format (merge {:time-base (:time_base stream)}
                      (av/codec-context-format decoder-context))
        
        time-base (condp = (:codec_type decoder-context)
                    raw/AVMEDIA_TYPE_AUDIO [1 (:sample-rate format)]
                    raw/AVMEDIA_TYPE_VIDEO (let [tb (:time_base stream)]
                                         [(:num tb) (:den tb)]))
        media-type (condp = (:codec_type decoder-context)
                     raw/AVMEDIA_TYPE_AUDIO :media-type/audio
                     raw/AVMEDIA_TYPE_VIDEO :media-type/video)

        format (assoc format
                      :media-type media-type
                      :time-base
                      (av/->avrational (first time-base)
                                       (second time-base)))
        ]
    (reify 
      dt-ffi/PToPointer
      (convertible-to-pointer? [_] true)
      (->pointer [_] (dt-ffi/->pointer decoder-context))
      clojure.lang.ILookup
      (valAt [_ k]
        (case k
          :format format
          
          ;; else
          nil))
      java.lang.AutoCloseable
      (close [_]
        (prn "closing decoder context")
        (raw/avcodec_free_context (dt-ffi/make-ptr :pointer (.address (dt-ffi/->pointer decoder-context))))))))

(comment
  
  (open-context "")
  ,)

(defn frame-encoder-init
  "output-format is a map of stream-id -> codec"
  [state input-formats encoders]

  (let [streams (into []
                      (map-indexed (fn [i input-format]
                                     
                                     (let [encoder-info (nth encoders i)
                                           encoder-format (assoc input-format
                                                                 :codec (:codec encoder-info))
                                           _ (prn "creating stream"  encoder-format)

                                           encoder-context (av/encoder-context encoder-format)
                                           _ (when-let [flags (:flags encoder-info)]
                                               (when (not (zero? flags))
                                                 (doto encoder-context
                                                   (Map/.put :flags
                                                         (int (bit-or (:flags encoder-context)
                                                                      (:flags encoder-info)))))))
                                           
                                           output-codec (:codec encoder-context)
                                           _ (assert output-codec)

                                           err (raw/avcodec_open2 encoder-context output-codec nil)
                                           _ (when (neg? err)
                                               (throw (ex-info "Could not open codec"
                                                               {:error-code err
                                                                :error-msg (av/error->str err)})))
                                           
                                           codec-parameters (raw/avcodec_parameters_alloc)
                                           codec-parameters-addr (-> codec-parameters
                                                                     dt-ffi/->pointer
                                                                     .address)
                                           _ (tech.v3.resource/track codec-parameters
                                                                     {:dispose-fn (fn []
                                                                                    (println "freeing codec parameters")
                                                                                    (raw/avcodec_parameters_free
                                                                                     (dt-ffi/make-ptr :pointer codec-parameters-addr)))})
                                           err (raw/avcodec_parameters_from_context codec-parameters
                                                                                    encoder-context)
                                           _ (when (neg? err)
                                               (throw (ex-info "Error initializing encoder codec parameters"
                                                               {:error-code err
                                                                :error-msg (av/error->str err)})))]
                                       {:codec-parameters codec-parameters
                                        :input-format input-format
                                        :encoder-context encoder-context})))
                      input-formats)]
    (assoc state :streams streams)))

(defn frame-encoder-close [state]
  (doseq [{:keys [encoder-context]} (:streams state)]
    (raw/avcodec_free_context (dt-ffi/make-ptr
                               :pointer
                               (-> encoder-context
                                   dt-ffi/->pointer
                                   .address))))
  
  (dissoc state :streams))

(defn frame-encoder-thread [encoders
                            ;; ins
                            in-chans
                            fresh-packet-chan
                            ;; outs
                            ready-for-packet-chan
                            recycle-frame-chan
                            out-chan]
  (let [port->idx (into {}
                        (map-indexed (fn [i ch]
                                       [ch i]))
                        in-chans)]
    (async/thread
     (try
       (loop [state {}
              output-packet nil]
         (async/>!! ready-for-packet-chan true)
         (let [[msg port] (async/alts!! in-chans)]
           (if (nil? msg)
             ;; in-chan closed. do cleanup
             (frame-encoder-close state)
             ;; else, process message
             (case (:type msg)
               :stream-opened
               (let [input-format (:format msg)
                     state (assoc-in state [:input-formats (port->idx port)] input-format)
                     
                     state (assoc state :last-pts 0)
                     state (if (= (count (:input-formats state))
                                  (count in-chans))
                             
                             (let [state (assoc state :input-formats (into [] 
                                                                           (->> (:input-formats state)
                                                                                (sort-by first)
                                                                                (map second))))
                                   state (frame-encoder-init state (:input-formats state) encoders)]
                               (async/>!! out-chan {:type :stream-opened
                                                    :streams (:streams state)})
                               state)
                             ;; else
                             state)]
                 (recur state output-packet))
               
               :new-frame
               (let [
                     input-frame (:frame msg)
                     
                     idx (port->idx port)
                     encoder-context (-> state
                                         :streams
                                         (nth idx)
                                         :encoder-context)
                     ;; _ (prn encoder-context)
                     ;; _ (prn input-frame)
                     
                                                                                             
                     ;; set pts somewhere else?
                     input-format (nth (:input-formats state) idx)
                     state (case (:media-type input-format)
                             :media-type/audio
                             (let [pts (+ (:last-pts state)
                                          (:nb_samples input-frame))
                                   _ (Map/.put input-frame :pts pts)
                                   _ (Map/.put (:time_base input-frame) :num 1)
                                   _ (Map/.put (:time_base input-frame) :den (:sample_rate input-frame))
                                   state (assoc state :last-pts pts)]
                               state)
                             
                             ;;else
                             state)

                     err (raw/avcodec_send_frame encoder-context
                                                 input-frame)
                     _ (when (and (not (zero? err))
                                  (not= err -22)
                                  (not (av/eagain? err)))
                         (throw (ex-info
                                 "Error encoding frame"
                                 {:error-code err
                                  :error-msg (av/error->str err)
                                  :type :decode-error})))]
                 (let [output-packet
                       (loop [output-packet output-packet]
                         (let [output-packet (or output-packet
                                                 (async/<!! fresh-packet-chan))
                               err (raw/avcodec_receive_packet encoder-context output-packet)
                               
]
                           (cond
                             (or (zero? err)
                                 (av/einvalid? err))
                             (do 
                               (let [time-base (case (:media-type input-format)
                                                 :media-type/audio (av/->avrational 1 (:sample-rate input-format))
                                                 :media-type/video (:time-base input-format))]
                                 (assert time-base)
                                 (Map/.put output-packet :time_base time-base))
                               
                               (Map/.put output-packet :stream_index idx)
                               (async/>!! out-chan {:packet output-packet
                                                    :type :new-packet})
                               (recur nil))
                             
                             (av/eof? err)
                             (throw (ex-info "Unexpected EOF"
                                             {}))
                             
                             (av/eagain? err)
                             output-packet
                             
                             ;; some other error
                             :else
                             (throw (ex-info
                                     "Error encoding frame"
                                     {:error-code err
                                      :error-msg (av/error->str err)
                                      :type :decode-error})))))]
                   (async/put! recycle-frame-chan input-frame)
                   (recur state output-packet)))
               
               :stream-closed
               (let [idx (port->idx port)
                     encoder-context (-> state
                                         :streams
                                         (nth idx)
                                         :encoder-context)
                     
                     _ (prn "closing encoder stream")
                     err (raw/avcodec_send_frame encoder-context nil)
                     
                     output-packet
                     (loop [output-packet output-packet]
                       (let [output-packet (or output-packet
                                               (async/<!! fresh-packet-chan))
                             
                             err (raw/avcodec_receive_packet encoder-context output-packet)]
                         (cond
                           (zero? err) (do
                                         (let [input-format (nth (:input-formats state) idx)
                                               time-base (case (:media-type input-format)
                                                           :media-type/audio (av/->avrational 1 (:sample-rate input-format))
                                                           :media-type/video (:time-base input-format))]
                                           (assert time-base)
                                           (Map/.put output-packet :time_base time-base))

                                         (Map/.put output-packet :stream_index idx)
                                         (async/>!! out-chan {:type :new-packet
                                                                :packet output-packet})
                                           (recur nil))
                           
                           (av/eagain? err) output-packet
                           
                           
                           (av/eof? err) output-packet
                           
                           :else
                           (throw (ex-info "Error Encoding" 
                                           {:error-code err
                                            :error-msg (av/error->str err)})))))
                     
                     state (update state :closed (fnil conj #{}) (port->idx port))
                     _ (prn (:closed state)
                            (count in-chans)
                            {:will-close (not (< (count (:closed state))
                                                 (count in-chans)))})
                     state (if (< (count (:closed state))
                                  (count in-chans))
                             state
                             ;; else, everyone is closed.
                             ;; cleanup
                             (do 
                               (prn "sending stream close")
                               (async/>!! out-chan {:type :stream-closed})
                               (-> state
                                   (frame-encoder-close)
                                   (assoc :closed #{})
                                   (dissoc :input-formats))))]
                 (recur state output-packet))))))
       (catch Throwable t
         (tap> t)
         (prn t))
       (finally
         (println "exiting encoder")))))
  
  )

(defn wrap-frame-encoder-input-filter [ins transform]
  (fn [state in msg]
    (let [[state outs] (transform state in msg)
          
          state (if (and (contains? ins in)
                         (#{:stream-opened
                            :stream-closed} (:type msg)))
                  (case (:type msg)
                    
                    :stream-opened (let [state (update state :ready-ins conj in)]
                                     (if (= (count ins)
                                            (count (:ready-ins state)))
                                       (assoc state :status :open)
                                       (assoc state :status :opening)))
                    :stream-closed (let [state (update state :ready-ins disj in)]
                                     (if (zero? (count (:ready-ins state)))
                                       (assoc state :status :closed)
                                       (assoc state :status :closing)))
                    ;; else
                    state)
                  ;; else
                  state)
          
          state (case (:status state)
                  (:opening :closed) (assoc state
                                            ::flow/input-filter
                                            (fn [id] 
                                              (or 
                                               (not (contains? ins id))
                                               (and (not (contains? (:ready-ins state)
                                                                    id))
                                                    (:ready? state)))))
                  (:open :closing) (assoc state
                                          ::flow/input-filter 
                                          (fn [id]
                                            (or (not (contains? ins id))
                                                (and (contains? (:ready-ins state) id)
                                                     (:ready? state))))))]
      [state outs])))

(defn frame-encoder-proc
  "`ins` is a vector of [stream-id doc-string]"
  [ins]
  {:describe (fn []
               {:params {:encoders
                         "map of stream-id -> encoder-info. keys can be :codec and :flags."}
                :ins (into {} ins)
                :outs {:packet "Decoded Frames"
                       ::recycle-frame "frame to recycle"}})
   :init (fn [{::keys [fresh-packet-chan] :as state}]
           (let [internal-in-chans (repeatedly (count ins) #(async/chan))
                 in->internal (into {}
                                    (map (fn [[id doc]]
                                           [id (keyword "internal" (name id))]))
                                    ins)
                 internal-ready-for-packet-chan (async/chan 1)
                 internal-recycle-frame-chan (async/chan 1)
                 internal-out-chan (async/chan 5)]
             (frame-encoder-thread (into []
                                         (map (fn [[stream-id _]]
                                                (get (:encoders state) stream-id)))
                                         ins)
                                   internal-in-chans
                                   fresh-packet-chan
                                   internal-ready-for-packet-chan
                                   internal-recycle-frame-chan
                                   internal-out-chan)
             (assoc state
                    :status :closed
                    :ready? true
                    :ready-ins #{}
                    :in->internal in->internal
                    ::flow/out-ports (zipmap (map second in->internal)  
                                             internal-in-chans)
                    ::flow/in-ports {:internal/ready-for-packet internal-ready-for-packet-chan
                                     :internal/recycle internal-recycle-frame-chan
                                     :internal/output-packet internal-out-chan
                                     })))
   :transition (fn [state status]
                 (if  (= status ::flow/stop)
                   (do
                     (doseq [[k ch] (::flow/in-ports state)]
                       (async/close! ch))
                     (doseq [[k ch] (::flow/out-ports state)]
                       (async/close! ch))
                     state)
                   state))
   :transform
   (wrap-frame-encoder-input-filter
    (into #{} (map first) ins)
    (fn [state in msg]
      (case in
        :in [(assoc state :ready? false)
             {(get-in state [:in->internal in]) [msg]}]
        :internal/ready-for-packet [(assoc state :ready? true)]
        :internal/recycle [state
                           {:recycle-frame [msg]}]
        :internal/output-packet [state {:packet [msg]}]
        ;; else
        [(assoc state :ready? false) {(get-in state [:in->internal in]) [msg]}])))})




(defn write-file-proc
  "Streams should be a map of :in -> docstring"
  []
  {:describe (fn []
               {:params {:fname "Name of file to write to."
                         :format "outputformat to use."}
                :ins {:in "packets to write to file."}
                :outs {::recycle-packet ""
                       :status ""}})
   :init (fn [state]
           state)
   :transition (fn [state status]
                 state)
   :transform
   (fn [state in msg]
     (case (:type msg)
       :stream-opened
       (let [format-context (open-output-context (:fname state)
                                                 (:format state))
             codec-parameters (:codec-parameters msg)
             
             streams (into []
                           (map (fn [{:keys [codec-parameters]}]
                                  (let [stream (raw/avformat_new_stream format-context
                                                                        nil)
                                        _ (when (nil? stream)
                                            (throw (Exception. "Could not create stream.")))
                                        err (raw/avcodec_parameters_copy (:codecpar stream)
                                                                         codec-parameters)
                                        _ (when (neg? err)
                                            (throw (ex-info "Error creating stream"
                                                            {:msg msg})))]
                                    stream)))
                           (:streams msg))]
         
         (raw/avformat_write_header format-context  nil)
         [(assoc state
                 :format-context format-context
                 :stream-formats (:streams msg)
                 :streams streams)])
       
       :stream-closed
       (let [format-context (:format-context state)
             err (raw/av_interleaved_write_frame format-context nil)
             _ (when (neg? err)
                 (throw (ex-info
                         "Error writing file"
                         {:error-code err
                          :error-msg (av/error->str err)})))]
         ;; (flush)
         (raw/av_write_trailer format-context)
         (raw/avio_closep (dt-ffi/make-ptr :pointer (:pb format-context)))
         (raw/avformat_free_context format-context)
         [state {:status [{:type :stream-closed}]}])
       
       :new-packet
       (let [format-context (:format-context state)
             packet (:packet msg)
             
             
             stream (nth (:streams state) (:stream_index packet))
             
             _ (raw/av_packet_rescale_ts packet
                                         (:time_base packet )
                                         (:time_base stream)
                                         #_#_{:num (-> packet :time_base :num int)
                                              :den (-> packet :time_base :den int)}
                                         {:num (-> stream :time_base :num int)
                                          :den (-> stream :time_base :den int)})
             ;; time base on packet doesn't currently do anything
             ;; but we update it here for consistency
             _ (Map/.put packet :time_base (:time_base stream))
             
             
             err (raw/av_interleaved_write_frame format-context packet)
             _ (when (neg? err)
                 (throw (ex-info
                         "Error writing file"
                         {:error-code err
                          :error-msg (av/error->str err)})))
             
             ;; passing `nil` for packet will flush to file as we go
             err (raw/av_interleaved_write_frame format-context nil)
             _ (when (neg? err)
                 (throw (ex-info
                         "Error writing file"
                         {:error-code err
                          :error-msg (av/error->str err)})))
             outs {::recycle-packet [packet]}]
         ;; (flush)
         [state outs])))})

(defn packet-recycler []
  {:describe (fn []
               {:params {:n "Number of packets"
                         ::fresh-packet-chan "Channel to put fresh packets on"}
                :ins {::recycle-packet "Packets to recycle"}
                :outs {}})
   :init (fn [{:keys [n] ::keys [fresh-packet-chan] :as state}]
           (let [recycle-packet-internal (async/chan)
                 update-state-chan (async/chan (async/sliding-buffer 1))]
             (async/go
              (let [initial-packets (into (queue)
                                          (repeatedly n #(raw/av_packet_alloc)))]
                (doseq [packet initial-packets]
                  (tech.v3.resource/track 
                   packet
                   {:dispose-fn
                    (let [addr (-> packet dt-ffi/->pointer .address)]
                      (fn []
                        (raw/av_packet_free (dt-ffi/make-ptr :pointer addr))))}))
                (try
                  (loop [fresh-packets initial-packets]
                    (async/put! update-state-chan {:fresh-count (count fresh-packets)})
                    ;; (prn "fresh packet count " (count fresh-packets))
                    (let [ports [recycle-packet-internal]
                          ports (if (seq fresh-packets)
                                  (conj ports [fresh-packet-chan
                                               (peek fresh-packets)])
                                  ports)
                          [val port] (async/alts! ports)]
                      
                      (cond
                        (= port recycle-packet-internal) (when-let [packet val]
                                                           (raw/av_packet_unref packet)
                                                           (recur (conj fresh-packets
                                                                        packet)))
                        (= port fresh-packet-chan) (when val
                                                     (recur (pop fresh-packets)))
                        
                        :else (throw (ex-info "Unrecognized chan" {})))))
                  (catch Exception e
                    (prn e))
                  (finally
                    ;; make sure fresh-packet-chan is closed
                    ;; (loop []
                    ;;   (when-let [_ (async/<! fresh-packet-chan)]
                    ;;     (recur)))
                    
                    ;; no way to ensure that this is run after shutdown
                    ;; so someone might still be using frames. 
                    #_#_(let [packet* (dt-ffi/make-ptr :pointer 0)]
                      (doseq [packet initial-packets]
                        (dt/set-value! packet* 0 (-> packet dt-ffi/->pointer .address))
                        (raw/av_packet_free packet*)))
                    (prn "all packets freed")))))
             (assoc state
                    ::flow/out-ports {:internal/recycle-packet recycle-packet-internal}
                    ::flow/in-ports {:update-state update-state-chan})))
   :transition (fn [state status] 
                 (if (= status ::flow/stop)
                   (do
                     (-> state ::flow/out-ports :internal/recycle-packet async/close!)
                     (assoc state :fresh-packets (queue)))
                   state))
   :transform
   (fn [state in msg]
     (case in
       :update-state [(merge state msg)]
       ::recycle-packet
       [state {:internal/recycle-packet [msg]}]))})

(defn frame-recycler []
  {:describe (fn []
               {:params {:n "Number of frames"
                         ::fresh-frame-chan "Channel to put fresh frames on."
                         :recycle-frame "An external channel for recycling frames."}
                :ins {::recycle-frame "frames to recycle"}})
   :init (fn [{:keys [n] ::keys [fresh-frame-chan] :as state}]
           (let [recycle-frame-internal (or 
                                         (:recycle-frame state)
                                         (async/chan 10))
                 update-state-chan (async/chan (async/sliding-buffer 1))]
             (async/go
              (let [initial-frames (into (queue)
                                         (repeatedly n #(raw/av_frame_alloc)))]
                (doseq [frame initial-frames]
                  (tech.v3.resource/track 
                   frame
                   {:dispose-fn
                    (let [addr (-> frame dt-ffi/->pointer .address)]
                      (fn []
                        (raw/av_frame_free (dt-ffi/make-ptr :pointer addr))))}))

                (try
                  (loop [fresh-frames initial-frames]
                    (async/put! update-state-chan {:fresh-count (count fresh-frames)})
                    (let [ports [recycle-frame-internal]
                          ports (if (seq fresh-frames)
                                  (conj ports [fresh-frame-chan (peek fresh-frames)])
                                  ports)
                          [val port] (async/alts! ports)]
                      (cond
                        (= port recycle-frame-internal) (when-let [frame val]
                                                          (raw/av_frame_unref frame)
                                                          (recur (conj fresh-frames
                                                                           frame)))
                        (= port fresh-frame-chan) (when val
                                                    (recur (pop fresh-frames)))
                        
                        :else (throw (ex-info "Unrecognized chan" {})))))
                  (catch Exception e
                    (prn e))
                  (finally
                    ;; make sure fresh-frame-chan is closed
                    ;; (loop []
                    ;;   (when-let [_ (async/<! fresh-frame-chan)]
                    ;;     (recur)))

                    ;; no way to ensure that this is run after shutdown
                    ;; so someone might still be using frames. 
                    #_(let [frame* (dt-ffi/make-ptr :pointer 0)]
                      (doseq [frame initial-frames]
                        (dt/set-value! frame* 0 (-> frame dt-ffi/->pointer .address))
                        (raw/av_frame_free frame*)))
                    #_(prn "all frames freed")))))
             
             (assoc state
                    ::flow/out-ports {:internal/recycle-frame recycle-frame-internal}
                    ::flow/in-ports {:update-state update-state-chan})))
   :transition (fn [state status] 
                 (if (= status ::flow/stop)
                   (do
                     (-> state ::flow/out-ports :internal/recycle-frame async/close!)
                     (assoc state :fresh-frames (queue)))
                   state))
   :transform
   (fn [state in msg]
     (case in
       :update-state
       [(merge state msg)]
       ::recycle-frame
       [state {:internal/recycle-frame [msg]}]))})

(defn counter-proc []
  {:describe (fn []
               {:args {:prefix ""}
                :ins {:in ""}})
   :init
   (fn [state]
     (assoc state :n 0))
   :transform
   (fn [{:keys [n prefix] :as state} in msg]
     (prn prefix (:n state))
     [(update state :n inc)])})

(defn onto-chan-proc []
  {:describe (fn [] {:ins {:in "  "}
                     :params {:chan "Channel to put values onto"}})
   :init (fn [m]
           (assoc m ::flow/out-ports {:out (:chan m)}))
   :transform (fn [_ _ v]
                [_ {:out [v]}])})


(defn stream-index-filter []
  {:describe (fn []
               {:params {:stream-index "The stream index to filter for"}
                :ins {:in ""}
                :outs {::recycle-packet ""
                       :out ""}})
   :init (fn [m] m)
   :transform
   (fn [state in msg]
     (let [type (:type msg)
           packet (:packet msg)
           stream-index (:stream-index state)]
       (case (:type msg)
         :streams-opened
         [state {:out [{:type :stream-opened
                        :stream (nth (:streams msg) stream-index)}]}]
         :streams-closed
         [state {:out [{:type :stream-closed}]}]
         
         :new-packet [state (if (= stream-index (-> msg :packet :stream_index))
                              {:out [msg]}
                              {::recycle-packet [msg]})])))})


(defn stream-splitter [n]
  {:describe (fn []
               {:ins {:in ""}
                :outs (into {}
                            (map (fn [i]
                                   [(keyword (str "out" i)) ""]))
                            (range n))})
   :init (fn [m] m)
   :transform
   (fn [state in msg]
     (let [type (:type msg)
           packet (:packet msg)
           stream-index (:stream-index state)]
       (case (:type msg)
         :streams-opened
         [state 
          (into {}
                (map (fn [i]
                       (let [port (keyword (str "out" i))]
                         [port [{:type :stream-opened
                                 :stream (nth (:streams msg) i)}]])))
                (range n))]
         :streams-closed
         [state
          (into {}
                (map (fn [i]
                       (let [port (keyword (str "out" i))]
                         [port [{:type :stream-closed}]])))
                (range n))]
         
         :new-packet [state 
                      (let [packet (:packet msg)
                            stream-index (-> msg :packet :stream_index)
                            port (keyword (str "out" stream-index))]
                        (assert packet)
                        {port [msg]})])))})

(defn media-type-splitter 
  "Splits packets by media type. Assumes at most one stream per media type."
  []
  {:describe (fn []
               {:ins {:in ""}                
                :outs {:media-type/audio ""
                       :media-type/video ""}})
   :init (fn [m] m)
   :transform
   (fn [state in msg]
     (let [type (:type msg)
           packet (:packet msg)
           stream-index (:stream-index state)]
       (case (:type msg)
         :streams-opened
         (let [idx->port
               (into {}
                     (map-indexed
                      (fn [i {:keys [codecpar] :as stream}]
                        (let [codecpar (dt-ffi/ptr->struct :AVCodecParameters
                                                           codecpar)
                              codec-type (:codec_type codecpar)
                              port (media.datafy/media-type->kw codec-type)]
                          [i port])))
                     (:streams msg))
               
               outs (into {}
                          (map (fn [[i port]]
                                 {port [{:type :stream-opened
                                         :stream (nth (:streams msg) i)}]}))
                          idx->port)
               state (assoc state :idx->port idx->port)]
           [state outs])
         :streams-closed
         [(dissoc state :idx->port)
          (into {}
                (map (fn [[i port]]
                       {port [{:type :stream-closed}]}))
                (:idx->port state))]
         
         :new-packet [state 
                      (let [packet (:packet msg)
                            stream-index (-> msg :packet :stream_index)
                            port (get (:idx->port state) stream-index)]
                        (assert packet)
                        {port [msg]})])))})

(defn stream-media-type-filter 
  "Filters a stream for packet of a specific media type."
  []
  {:describe (fn []
               {:ins {:in ""}                
                :outs {:out ""
                       ::recycle-packet ""}
                :params {:media-type "The media type to filter for"}})
   :init (fn [m] m)
   :transform
   (fn [state in msg]
     (let [type (:type msg)
           packet (:packet msg)
           stream-index (:stream-index state)]
       (case (:type msg)
         :streams-opened
         (let [media-type (:media-type state)
               [filter-index outs] (first-by 
                                      (comp (keep-indexed (fn [i {:keys [codecpar] :as stream}]
                                                            (let [codecpar (dt-ffi/ptr->struct :AVCodecParameters
                                                                                               codecpar)
                                                                  codec-type (media.datafy/media-type->kw
                                                                              (:codec_type codecpar))]
                                                              (when (= codec-type media-type)
                                                                [i 
                                                                 {:out [{:type :stream-opened
                                                                         :stream stream}]}])))))
                                      (:streams msg))
               state (assoc state :filter-index filter-index)]
           [state outs])
         :streams-closed
         [(dissoc state :filter-index)
          {:out [{:type :stream-closed}]}]
         
         :new-packet [state 
                      (let [packet (:packet msg)
                            stream-index (-> msg :packet :stream_index)]
                        (if (= stream-index (:filter-index state))
                          {:out [msg]}
                          {::recycle-packet [packet]}))])))})



(defn file-packet-flow-close [state]
  (if-let [format-context (:format-context state)]
    (do
      (java.lang.AutoCloseable/.close format-context)
      (dissoc state :format-context))
    state))

(defn file-packet-flow-init-context [state fname]
  (assert (nil? (:format-context state)))
  (let [format-context (open-context fname)]
    (find-stream-info* format-context)
    (assoc state :format-context format-context)))

(defn file-packet-flow-ban [state cid]
  (let [banned (conj (or (:banned state) #{})
                     cid)]
    (assoc state
           :banned banned
           ::flow/input-filter (fn [cid]
                                 (not (contains? banned cid))))))

(defn file-packet-flow-unban [state cid]
  (let [banned (disj (or (:banned state) #{})
                     cid)]
    (assoc state
           :banned banned
           ::flow/input-filter (fn [cid]
                                 (not (contains? banned cid))))))

(defn wrap-file-packet-input-filter [[state outs]]
  (if (:eof? state)
    [(assoc state ::flow-input-filter (constantly false)) outs]
    (let [state (if (> (count (:fresh-packets state)) 10)
                  (file-packet-flow-ban state :fresh-packet)
                  (file-packet-flow-unban state :fresh-packet))]
      [state outs])))

(defn file->packets-proc []
  (wrap-producer
   {:describe (fn []
                {:ins {:filename "File to start decoding"}
                 :params {::fresh-packet-chan "Channel to acquire fresh packets."}
                 :outs {:packet "Packets from file."}})
    :init (fn [{::keys [fresh-packet-chan] :as state}]
            (assoc state
                   :fresh-packets (queue)
                   ::flow/in-ports {:fresh-packet fresh-packet-chan}
                   ::produce false))
    :transition (fn [state status] 
                  (if (= status ::flow/stop)
                    (file-packet-flow-close state)
                    state))
    :transform
    (fn [state in msg]
      (wrap-file-packet-input-filter
       (case in
         :filename
         
         (let [state (-> state
                         (file-packet-flow-ban :filename)
                         (file-packet-flow-init-context msg)
                         (assoc ::produce (boolean (seq (:fresh-packets state)))))]
           [state
            {:packet 
             [{:type :streams-opened
               :streams (-> state :format-context :streams)}]}])
         
         :fresh-packet
         [(-> state
              (update :fresh-packets conj msg)
              (assoc ::produce (some? (:format-context state))))]
         
         ;; else
         (let [packet (peek (:fresh-packets state))
               state (update state :fresh-packets pop)
               state (if (seq (:fresh-packets state))
                       state
                       (assoc state ::produce false))
               
               format-context (:format-context state)
               _ (assert format-context)
               err (raw/av_read_frame (:format-context state) packet)]
           
           (cond
             (zero? err) [state {:packet [{:type :new-packet
                                           :packet packet}]}]
             (av/eof? err) 
             (do
               (prn "sending packet eof!")
               [(-> state
                    (assoc ::produce false)
                    (file-packet-flow-close)
                    (file-packet-flow-unban :filename))
                {:packet [{:type :streams-closed}]}])
             
             :else
             (throw (ex-info "Error reading file"
                             {:error-code err
                              :error-msg (av/error->str err)
                              :type :decode-error})))))))}))


(defn file->packets-proc2 []
  (wrap-producer
   {:describe (fn []
                {:ins {}
                 :params {::fresh-packet-chan "Channel to acquire fresh packets."
                          :filename "File to start decoding"}
                 :outs {:packet "Packets from file."}})
    :init (fn [{::keys [fresh-packet-chan] :as state}]
            (assoc state
                   :fresh-packets (queue)
                   ::flow/in-ports {:fresh-packet fresh-packet-chan}
                   ::produce false))
    :transition (fn [state status] 
                  (if (= status ::flow/stop)
                    (file-packet-flow-close state)
                    state))
    :transform
    (fn [state in msg]
      (wrap-file-packet-input-filter
       (case in
         :fresh-packet
         [(-> state
              (update :fresh-packets conj msg)
              (assoc ::produce true))]
         
         ;; else
         (if-let [format-context (:format-context state)]
           (let [packet (peek (:fresh-packets state))
                 
                 _ (assert (and format-context packet))

                 state (update state :fresh-packets pop)
                 state (if (seq (:fresh-packets state))
                         state
                         (assoc state ::produce false))
                 
                 err (raw/av_read_frame (:format-context state) packet)]
             
             (cond
               (zero? err) [state {:packet [{:type :new-packet
                                             :packet packet}]}]
               (av/eof? err) 
               (do
                 (prn "sending packet eof!")
                 [(-> state
                      (assoc ::produce false)
                      (file-packet-flow-close)
                      (assoc :eof? true))
                  {:packet [{:type :streams-closed}]}])
               
               :else
               (throw (ex-info "Error reading file"
                               {:error-code err
                                :error-msg (av/error->str err)
                                :type :decode-error}))))
           ;; else init
           (let [state (-> state
                           (file-packet-flow-init-context (:filename state)))]
             [state
              {:packet 
               [{:type :streams-opened
                 :streams (-> state :format-context :streams)}]}])))))}))

(defn packet-frames-init [state msg]
  (let [stream (:stream msg)]
    (assoc state :decoder-context (stream->decoder-context stream))))

(defn packet-frames-close [state]
  (java.lang.AutoCloseable/.close (:decoder-context state))
  (dissoc state :decoder-context))

(defn packet->frame-thread [;; ins
                            packet-chan
                            fresh-frame-chan
                            ready-for-packet-chan
                            ;; outs
                            recycle-packet-chan
                            frame-chan]
  
  (async/thread
   (try
     (loop [state {}
            frame nil]
       (async/>!! ready-for-packet-chan true)
       (if-let [msg (async/<!! packet-chan)]
         (case (:type msg)
           :stream-opened
           (let [_ (assert (not (:decoder-context state)))
                 state (packet-frames-init state msg)]
             (async/>!! frame-chan {:type :stream-opened
                                    :format (-> state :decoder-context :format)})
             (recur state frame))
           
           (:new-packet :stream-closed)
           (let [packet (:packet msg)
                 decoder-context (:decoder-context state)
                 err (raw/avcodec_send_packet decoder-context packet)
                 _ (when (and (not (zero? err))
                              (not= err -22)
                              (not (av/eagain? err)))
                     (throw (ex-info
                             "Error decoding packet"
                             {:error-code err
                              :error-msg (av/error->str err)
                              :type :decode-error})))]
             
             ;; now flush frames
             (let [[state frame]
                   (loop [state state
                          frame frame]
                     (let [
                           frame (or frame (async/<!! fresh-frame-chan))
                           
                           err (raw/avcodec_receive_frame decoder-context frame)]
                       
                       (cond
                         (zero? err)
                         (do (async/>!! frame-chan {:type :new-frame
                                                    :frame frame})
                             (recur state nil))
                         
                         (av/eagain? err) [state frame]
                         
                         (av/eof? err)
                         (do
                           (prn "sending frame eof")
                           (async/>!! frame-chan {:type :stream-closed})
                           (let [state (-> state
                                           (packet-frames-close))]
                             [state frame]))
                         
                         ;; some other error
                         :else
                         (do
                           (throw (ex-info
                                   "Error decoding packet"
                                   {:error-code err
                                    :error-msg (av/error->str err)
                                    :type :decode-error}))))))]
               (when packet
                 (async/put! recycle-packet-chan packet))
               
               (recur state frame))))
         
         ;; else cleanup
         (when-let [decoder-context (:decoder-context state)]
           (java.lang.AutoCloseable/.close decoder-context)
           nil)))
     (catch Throwable t
       (tap> t)
       (prn t))
     (finally 
       (prn "closing thread")))))

(defn packet->frames []
  {:describe (fn []
               {
                :params {::fresh-frame-chan "Channel to get fresh frames from."}
                :ins {:packet "packet to decode"}
                :outs {:frame "Decoded Frames"
                       ::recycle-packet "Packet to recycle"}})
   :init (fn [{::keys [fresh-frame-chan] :as state}]
           (let [internal-packet-chan (async/chan)
                 ready-for-packet-chan (async/chan 1)
                 internal-recycle-chan (async/chan 1)
                 internal-frame-chan (async/chan 5)]
             (packet->frame-thread internal-packet-chan
                                   fresh-frame-chan
                                   ready-for-packet-chan
                                   internal-recycle-chan
                                   internal-frame-chan)
             (assoc state
                    ::flow/in-ports {:internal/recycle2 internal-recycle-chan
                                     :internal/ready-for-packet ready-for-packet-chan
                                     :internal/frame2 internal-frame-chan}
                    ::flow/out-ports {:internal/packet2 internal-packet-chan})))
   :transition (fn [state status]
                 (if  (= status ::flow/stop)
                   (do
                     (-> state ::flow/in-ports :internal/recycle2 async/close!)
                     (-> state ::flow/in-ports :internal/frame2 async/close!)
                     (-> state ::flow/out-ports :internal/packet2 async/close!)
                     state)
                   state))
   :transform
   (fn [state in msg]
     (case in
       :packet [(assoc state ::flow/input-filter (fn [cid]
                                                   (not= cid :packet)))
                {:internal/packet2 [msg]}]
       :internal/ready-for-packet [(dissoc state ::flow/input-filter)]
       :internal/recycle2 [state
                           {::recycle-packet [msg]}]
       :internal/frame2 [state {:frame [msg]}]))})

#_(defn undatafy-audio-format [{:keys [channel-layout
                                     sample-format
                                     sample-rate]}]
  (let [sample-rate (if (some #{44100} sample-rates)
                          44100
                          (first sample-rates))

            sample-format (first sample-formats)
            channel-layout (if (some #{audio/AV_CH_LAYOUT_STEREO} channel-layouts)
                             audio/AV_CH_LAYOUT_STEREO
                             (or (first channel-layouts)
                                 channel-layout))
            format {:sample-rate sample-rate
                    :sample-format sample-format
                    :channel-layout channel-layout}]
    format))

(defn test-packet-flow []
  (let [fresh-frame-chan (async/chan 12)
        fresh-packet-chan (async/chan 12)
        done-chan (async/chan (async/sliding-buffer 10))
        gdef {:procs
              {:packet-recycler
               {:proc (-> (packet-recycler)
                          flow/map->step
                          flow/process)
                :args {:n 100}}
               :frame-recycler
               {:proc (-> (frame-recycler)
                          flow/map->step
                          flow/process)
                :args {:n 1000}}
               
               :media-packets {:proc (-> (file->packets-proc)
                                         flow/map->step
                                         flow/process)
                               :args {::fresh-packet-chan fresh-packet-chan}}
               :packet-sink
               {:proc (flow/process
                       (flow/lift1->step
                        (fn [msg]
                          (case (:type msg)
                            :stream-opened (prn "stream opened!")
                            :stream-closed (prn "stream closed")
                            :new-packet (let [packet (:packet msg)]
                                          (prn {:pts (:pts packet)
                                                :dts (:dts packet)
                                                :time-base (str (-> packet :time_base :num) "/" (-> packet :time_base :den)) 
                                                :stream_index (:stream_index packet)})
                                          packet)))))}
               
               :report-done {:proc (-> (onto-chan-proc)
                                       flow/map->step
                                       flow/process)
                             :args {:chan done-chan}}
               
               
               :stream-splitter {:proc (-> (media-type-splitter)
                                           flow/map->step
                                           flow/process)}

               :decoder0 {:proc (-> (packet->frames)
                                    flow/map->step
                                    flow/process)
                          :args {::fresh-frame-chan fresh-frame-chan}}
               :decoder1 {:proc (-> (packet->frames)
                                    flow/map->step
                                    flow/process)
                          :args {::fresh-frame-chan fresh-frame-chan}}
               :resampler {:proc (-> (audio/resample-audio-proc)
                                     flow/map->step
                                     flow/process)
                           :args {::fresh-frame-chan fresh-frame-chan
                                  :output-format {:ch-layout (media.datafy/str->ch-layout "stereo")
                                                  :sample-format (media.datafy/kw->sample-format :sample-format/fltp)
                                                  :sample-rate 44100
                                                  ;; :frame-size 4608
                                                  :media-type :media-type/audio}}}
               :play-sound {:proc (-> (audio/play-sound-proc)
                                      flow/map->step
                                      flow/process)}
               
               :my-audio-filter {:proc (-> (avfilter/filter-proc [[:in "input"]])
                                     flow/map->step
                                     flow/process)
                           :args {:opts {}
                                  :output-format {:ch-layout (media.datafy/str->ch-layout "mono")
                                                  :sample-format (media.datafy/kw->sample-format :sample-format/flt)
                                                  :sample-rate 44100
                                                  :media-type :media-type/audio}
                                  :filter-name my-filter-name
                                  ::fresh-frame-chan fresh-frame-chan}}
               
               :my-video-filter {:proc (-> (avfilter/filter-proc [[:in "input"]])
                                           flow/map->step
                                           flow/process)
                                 :args {:opts {}
                                        :output-format {:pixel-format (media.datafy/kw->pixel-format :pixel-format/yuv420p)}
                                        :filter-name "edgedetect"
                                        ::fresh-frame-chan fresh-frame-chan}}
               
               :frame-encoder {:proc (-> (frame-encoder-proc [[:audio ""]
                                                              [:video ""]])
                                         flow/map->step
                                         flow/process)
                               :args {:encoders {:audio {:codec {:id 86018}
                                                         :flags raw/AV_CODEC_FLAG_GLOBAL_HEADER}
                                                 :video {:codec {:id 27}
                                                         :flags raw/AV_CODEC_FLAG_GLOBAL_HEADER}}
                                      
                                      ::fresh-packet-chan fresh-packet-chan}}
               
               :file-writer {:proc (-> (write-file-proc)
                                       flow/map->step
                                       flow/process)
                             :args {:fname "output.mp4"
                                    :format (raw/av_guess_format nil
                                                                 (dt-ffi/string->c ".mp4")
                                                                 nil)}}
               
               :frame-sink0
               {:proc (flow/process
                       (flow/lift1->step
                        (fn [msg]
                          (case (:type msg)
                            :stream-closed (prn "closed0")
                            :stream-opened (prn (-> msg :format))
                            :new-frame nil #_ (prn "frame0" (-> msg :frame :pts))
                            )
                          (:frame msg))))}
               :frame-sink1
               {:proc (flow/process
                       (flow/lift1->step
                        (fn [msg]
                          ;; (prn "frame1" (-> msg :frame :pts))
                          (:frame msg))))}}
              :conns
              [
               [[:media-packets :packet] [:stream-splitter :in]]
               [[:stream-splitter :media-type/audio] [:decoder0 :packet]]
               
               [[:stream-splitter :media-type/video] [:decoder1 :packet]]

               [[:decoder0 :frame]
                
                ;; [:frame-encoder :audio]
                ;; [:my-audio-filter :in]
                [:resampler :in]
                ]


               ;; [[:decoder0 :frame] [:frame-sink0 :in]]
               
               [[:my-audio-filter :out]
                
                [:resampler :in]
                ;;[:frame-encoder :audio]
                ]
               
               [[:resampler :out] 
                
                #_[:play-sound :in]
                [:frame-encoder :audio]]
               [[:decoder1 :frame] [:my-video-filter :in]]
               
               [[:my-video-filter :out] [:frame-encoder :video]]

               [[:frame-encoder :packet] 
                ;;[:packet-sink :in]
                [:file-writer :in]
                ]
               
               [[:file-writer :status] [:report-done :in]]
               
               
               
               [[:packet-sink :out] [:packet-recycler ::recycle-packet]]
               ;; [[:decoder1 :frame] [:frame-sink1 :in]]
               [[:decoder0 ::recycle-packet] [:packet-recycler ::recycle-packet]]
               [[:play-sound :recycle-frame] [:frame-recycler :recycle-frame]]
               [[:resampler :recycle-frame] [:frame-recycler :recycle-frame]]
               [[:my-audio-filter :recycle-frame] [:frame-recycler :recycle-frame]]
               [[:my-video-filter :recycle-frame] [:frame-recycler :recycle-frame]]
               [[:frame-encoder :recycle-frame] [:frame-recycler :recycle-frame]]
               [[:decoder1 ::recycle-packet] [:packet-recycler ::recycle-packet]]
               [[:file-writer ::recycle-packet] [:packet-recycler ::recycle-packet]]
               
               
               [[:frame-sink0 :out] [:frame-recycler :recycle-frame]]
               ;; [[:frame-sink1 :out] [:frame-recycler :recycle-frame]]
               ;; [[:media-packets :packet] [:packet-sink :in]]
               ;; [[:packet-sink :out] [:packet-recycler ::recycle-packet]]
               ]}
        

        gdef (assoc-in gdef [:procs :frame-recycler :args ::fresh-frame-chan] fresh-frame-chan)
        gdef (assoc-in gdef [:procs :packet-recycler :args ::fresh-packet-chan] fresh-packet-chan)

        ]
    gdef))

(defn test-packet-flow! []
  (let [
        gdef (test-packet-flow)
        done-chan (-> gdef
                      :procs
                      :report-done
                      :args
                      :chan)
        
        fresh-frame-chan (-> gdef
                             :procs
                             :frame-recycler
                             :args
                             ::fresh-frame-chan)
        fresh-packet-chan (-> gdef
                              :procs
                              :packet-recycler
                              :args
                              ::fresh-packet-chan)

        flow (flow/create-flow gdef)
        ]
    (-> flow flow/start monitoring)
  (flow/resume flow)
  
    (flow/inject flow [:media-packets :filename] [media-fname])
    (async/<!! done-chan)
    (prn "stopping flow.")
    (flow/stop flow)
    ;; close these chans after stopping flow
    ;; (async/close! fresh-frame-chan)
    
    (^[long] Thread/sleep (long 2e3))
    (System/gc)

  
    (^[long] Thread/sleep (long 15e3))))

(defn -main []
  
  ;; (test-old-flow )
  (test-packet-flow!)
  )


(comment
  (def my-filter-name "anull")

  (test-packet-flow)
  media-fname
  "/Users/adrian/workspace/eddie/al_super-mario-world-map2.mp3"
  

  
  ,)


(defn add-recycler 
  ([g recycler-proc recycle-pid recycle-port fresh-param fresh-chan n]
   (let [recycle-conns
         (into []
               (keep (fn [[pid {:keys [proc]}]]
                       (let [m (flow.spi/describe proc)]
                         (when (-> m :outs recycle-port)
                           [[pid recycle-port] [recycle-pid recycle-port]]))))
               (:procs g))
         
         fresh-pids
         (into #{}
               (keep (fn [[pid {:keys [proc]}]]
                       (let [m (flow.spi/describe proc)]
                         (when (-> m :params fresh-param)
                           pid))))
               (:procs g))
         
         
         
         g (update g :conns into recycle-conns)
         g (update g :procs
                   (fn [procs]
                     (reduce (fn [procs pid]
                               (assoc-in procs [pid :args fresh-param] fresh-chan))
                             procs
                             fresh-pids)))
         
         g (assoc-in g
                     [:procs recycle-pid]
                     {:proc recycler-proc
                      :args {:n n
                             fresh-param fresh-chan}})]
     g)))

(defn add-frame-recycler 
  ([g]
   (add-frame-recycler g (async/chan 12) 100))
  ([g fresh-frame-chan n]
   (add-recycler g 
                 (-> (frame-recycler)
                       flow/map->step
                       flow/process)
                 ::frame-recycler
                 ::recycle-frame
                 ::fresh-frame-chan
                 fresh-frame-chan
                 n)))

(defn add-packet-recycler
  ([g]
   (add-packet-recycler g (async/chan 12) 100))
  ([g fresh-packet-chan n]
   (add-recycler g 
                 (-> (packet-recycler)
                     flow/map->step
                     flow/process)
                 ::packet-recycler
                 ::recycle-packet
                 ::fresh-packet-chan
                 fresh-packet-chan
                 n)))


(defn frames-flow
  "Creates a flow for receiving frames from a a media source.
  
  A channel for receiving frames must be assoc-in to [:procs :frame-out :args :chan].
  A channel for recycling frames must be assoc-in to [:procs ::frame-recycler :args :recycle-frame].

  "
  [media stream]
  ;; assume media is a file?
  (let [file (io/file (:file media))

        stream-filter (case stream
                        :audio {:proc (-> (stream-media-type-filter)
                                          flow/map->step
                                          flow/process)
                                :args {:media-type :media-type/audio}}
                        :video {:proc (-> (stream-media-type-filter)
                                          flow/map->step
                                          flow/process)
                                :args {:media-type :media-type/video}}
                        
                        
                        ;; else
                        (do
                          (when (not (integer? stream))
                            (throw (ex-info "stream specifier must be :audio, :video or a stream index"
                                            {:stream stream
                                             :media media})))
                          {:proc (-> (stream-index-filter)
                                     flow/map->step
                                     flow/process)
                           :args {:stream-index stream}}))
        
        g {:procs {:media-packets {:proc (-> (file->packets-proc2)
                                             flow/map->step
                                             flow/process)
                                   :args {:filename (java.io.File/.getPath file)}}
                   :stream-filter stream-filter
                   :decoder {:proc (-> (packet->frames)
                                       flow/map->step
                                       flow/process)}
                   
                   :transcode {:proc (-> (avfilter/filter-proc [[:in "input"]])
                                         flow/map->step
                                         flow/process)
                               :args {:opts {}
                                      :output-format {:pixel-format (media.datafy/kw->pixel-format :pixel-format/bgra)}
                                      :filter-name "null"}}
                   
                   :frame-out {:proc (flow/process
                                      (flow/map->step
                                       {:describe (fn [] {:ins {:in "  "}
                                                          :params {:chan "Channel to put values onto"}})
                                        :init (fn [m] 
                                                (assoc m
                                                       ::flow/out-ports {:out (:chan m)}))
                                        :transform (fn [state _ msg]
                                                     (case (:type msg)
                                                       :stream-opened [state]
                                                       :stream-closed
                                                       (do
                                                         (async/close! (:chan state))
                                                         [state])
                                                       :new-frame
                                                       [state {:out [(:frame msg)]}]))}))}}
           :conns [
                   [[:media-packets :packet] [:stream-filter :in]]
                   [[:stream-filter :out] [:decoder :packet]]
                   [[:decoder :frame] [:transcode :in]]
                   [[:transcode :out] [:frame-out :in]]
                   
                   ;; [[:decoder ::recycle-packet] [:packet-recycler ::recycle-packet]]
                   ;; [[:transcode :recycle-frame] [:frame-recycler :recycle-frame]]
                   ;; [[:stream-filter ::recycle-packet] [:packet-recycler ::recycle-packet]]
                   ]}]
    g))


(defn frames-reducible [media stream]
  (reify clojure.lang.IReduceInit
    (reduce [_ f init]
      (let [g (frames-flow media stream)
            frame-chan (async/chan 12)
            recycle-frame-chan (async/chan 10)
            
            g (-> g
                  (add-frame-recycler)
                  (add-packet-recycler)
                  (assoc-in [:procs :frame-out :args :chan] frame-chan)
                  (assoc-in [:procs ::frame-recycler :args :recycle-frame] recycle-frame-chan))
            
            flow (flow/create-flow g)
            _ (-> flow flow/start monitoring)
            
            
            ]
        (track-flow flow)
        (flow/resume flow)
        
        (let [result (loop [result init]
                       (if-let [frame (async/<!! frame-chan)]
                         (let [result (f result frame)]
                           (async/put! recycle-frame-chan frame)
                           (if (reduced? result)
                             @result
                             (recur result)))
                         result))]
          (flow/stop flow)
          result)))))

(require 'membrane.skia)
(def pixmap #'membrane.skia/pixmap)

(defn -main [& args]
  (let [uuid (random-uuid)
        next-int (let [atm (atom 0)]
                   (fn []
                     (swap! atm inc)))
        width 640
        height 360]
    (run! (fn [frame]
            (prn (:pts frame))
            
            (let [linesize (-> frame :linesize first)
                  buf-size (* linesize height)
                  i (next-int)]
              (membrane.skia/save-image
               (str "frames/frame" i ".png")
               (pixmap [uuid i]
                       (dt/->byte-array
                        (native-buffer/wrap-address (first (:data frame))
                                                    buf-size))
                       width height membrane.skia/kBGRA_8888_SkColorType membrane.skia/kOpaque_SkAlphaType 
                       linesize))))
          (frames-reducible {:file media-fname} :video)))
  (prn "done")
  (Thread/sleep (long 5e3)))


