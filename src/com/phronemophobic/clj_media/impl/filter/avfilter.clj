(ns com.phronemophobic.clj-media.impl.filter.avfilter
  (:require [com.phronemophobic.clj-media.impl.filter.frame
             :as ff]
            [com.phronemophobic.clj-media.impl.filter.media
             :as fm]
            [clojure.string :as str]
            [clojure.datafy :as d]
            [clojure.core.async :as async]
            [clojure.core.async.flow :as flow]
            [net.cgrand.xforms :as x]
            [com.phronemophobic.clj-media.impl.datafy
             :refer [set-filter-context-options
                     supported-filter-option?
                     list-filters]
             :as datafy-media]
            [clojure.java.io :as io]
            [tech.v3.tensor :as dtt]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [tech.v3.datatype.casting :as dt-casting]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.audio :as audio]
            [com.phronemophobic.clj-media.impl.video :as video]
            [com.phronemophobic.clj-media.impl.util
             :refer [distinct-by
                     interleave-all
                     str->kw
                     str->symbol
                     insert-last]]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all]
            [com.phronemophobic.clj-media.impl.flow :as-alias impl.flow])
  (:import java.io.PushbackReader
           java.lang.ref.Cleaner))


(defn video-filter [input-formats filter-name opts]
  #_(let [filter-graph (avfilter_graph_alloc)
        _ (when (nil? filter-graph)
            (throw (Exception. "Failed to create filter graph")))
        _ (let [ptr (.getPointer filter-graph)]
            (.register av/cleaner filter-graph
                       (fn []
                         (avfilter_graph_free
                          (PointerByReference. ptr)))))

        input-contexts
        (into []
              (map (fn [input-format]
                     (let [buffer (avfilter_get_by_name "buffer")
                           _ (when (nil? buffer)
                               (throw (Exception.)))
                           buffer-context (avfilter_graph_alloc_filter filter-graph buffer nil)
                           time-base (:time-base input-format)
                           args (format "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d"
                                        (:width input-format)
                                        (:height input-format)
                                        (:pixel-format input-format)
                                        (:num time-base)
                                        (:den time-base))

                           err (avfilter_init_str buffer-context args)
                           _ (when (not (zero? err))
                               (throw (Exception.)))]
                       buffer-context)))
              input-formats)

        buffersink (avfilter_get_by_name "buffersink")
        _ (when (nil? buffersink)
            (throw (Exception.)))
        ;; buffersink-context* (PointerByReference.)
        buffersink-context* (dt-ffi/make-ptr :pointer 0)
        _ (avfilter_graph_create_filter buffersink-context*
                                        buffersink
                                        nil
                                        nil
                                        nil
                                        filter-graph)

        buffersink-context (first buffersink-context*)

        pix-fmts (doto (IntByReference.)
                   (.setValue
                    (if-let [pixel-format (:avfilter/pixel-format opts)]
                      (datafy-media/kw->pixel-format pixel-format)
                      (:pixel-format (first input-formats)))))
        _ (av_opt_set_bin buffersink-context "pix_fmts"
                          pix-fmts
                          (* 1 4)
                          AV_OPT_SEARCH_CHILDREN)

        ;; create the filter
        filter-context (avfilter_graph_alloc_filter
                        filter-graph
                        (avfilter_get_by_name filter-name)
                        nil)
        _ (assert filter-context)
        _ (set-filter-context-options filter-context filter-name
                                      (dissoc opts :avfilter/pixel-format))

        _ (avfilter_init_str filter-context nil)

        _ (doseq [[i input-context] (map-indexed vector input-contexts)]
            (let [err (avfilter_link input-context 0
                                     filter-context i)]
              (when (not (zero? err))
                (throw (Exception.)))))

        err (avfilter_link filter-context 0
                           buffersink-context 0)
        _ (when (not (zero? err))
            (throw (Exception.)))

        err (avfilter_graph_config filter-graph nil)
        _ (when (not (>= err 0))
            (throw (Exception.)))

        filter-graph* (doto (PointerByReference.)
                        (.setValue (.getPointer filter-graph)))
        filter-graph-ref (volatile! filter-graph)
        ;; hold reference to filter graph
        ;; so that it's not cleaned before
        ;; we're done.
        _ (.register av/cleaner buffersink-context
                     (fn []
                       (vreset! filter-graph-ref nil)))

        time-base (av_buffersink_get_time_base buffersink-context)
        output-format
        {:width (av_buffersink_get_w buffersink-context)
         :height (av_buffersink_get_h buffersink-context)
         ;; copy. time bases known to mutate in place
         :time-base (av/->avrational (:num time-base)
                          (:den time-base))
         :pixel-format (av_buffersink_get_format buffersink-context)
         :media-type :media-type/video}
        frame (av_frame_alloc)]

    {:output-format output-format
     :xform
     (fn [rf]

       (fn
         ([]
          (rf))
         ([result]
          ;; flush
          (doseq [buffer-context input-contexts]
            (av_buffersrc_write_frame buffer-context nil))
          (let [result
                (loop [result result]
                  (let [err (av_buffersink_get_frame_flags buffersink-context
                                                           frame
                                                           0)]
                    (cond
                      (zero? err)
                      (let [result (rf result frame)]
                        (av_frame_unref frame)
                        (if (reduced? result)
                          result
                          (recur result)))

                      (av/eagain? err)
                      result

                      (av/eof? err)
                      result
                      ;; (reduced result)

                      :else
                      (reduced {:error-code err
                                :error-msg (av/error->str err)
                                :type :transcode-error}))))]
            (av_frame_free (doto (PointerByReference.)
                              (.setValue (.getPointer frame))))
            (rf result)))
         ([result [input-idx input-frame]]
          (let [buffer-context (nth input-contexts input-idx)]
            (av_buffersrc_write_frame buffer-context
                                      input-frame))
          (loop [result result]
            (let [err (av_buffersink_get_frame_flags buffersink-context
                                                     frame
                                                     0)]
              (cond
                (zero? err)
                (let [result (rf result frame)]
                  (av_frame_unref frame)
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
                          :type :transcode-error})))))))}))

#_(defn audio-filter [input-formats filter-name opts]
  (let [filter-graph (avfilter_graph_alloc)
        _ (let [ptr (.getPointer filter-graph)]
            (.register av/cleaner filter-graph
                       (fn []
                         (avfilter_graph_free
                          (PointerByReference. ptr)))))


        input-contexts
        (into []
              (map (fn [input-format]
                     (let [buffer (avfilter_get_by_name "abuffer")
                           _ (when (nil? buffer)
                               (throw (Exception.)))
                           buffer-context (avfilter_graph_alloc_filter filter-graph buffer nil)

                           args (format "channel_layout=%s:sample_fmt=%d:sample_rate=%d"
                                        (datafy-media/ch-layout->str
                                         (:ch-layout input-format ))
                                        (:sample-format input-format)
                                        (:sample-rate input-format))

                           err (avfilter_init_str buffer-context args)
                           _ (when (not (zero? err))
                               (throw
                                (ex-info "Could not create audio filter"
                                         {:error-code err
                                          :error-msg (av/error->str err)})))]
                       buffer-context)))
              input-formats)

        buffersink (avfilter_get_by_name "abuffersink")
        _ (when (nil? buffersink)
            (throw (Exception.)))
        buffersink-context* (PointerByReference.)
        _ (avfilter_graph_create_filter buffersink-context*
                                        buffersink
                                        nil
                                        nil
                                        nil
                                        filter-graph)

        buffersink-context (.getValue buffersink-context*)

        sample-fmts (doto (IntByReference.)
                      (.setValue (:sample-format (first input-formats))))
        _ (av_opt_set_bin buffersink-context "sample_fmts"
                          sample-fmts
                          (* 1 4)
                          AV_OPT_SEARCH_CHILDREN)

        ;; create the filter
        filter-context (avfilter_graph_alloc_filter
                        filter-graph
                        (avfilter_get_by_name filter-name)
                        nil)
        _ (assert filter-context)
        _ (set-filter-context-options filter-context filter-name opts)

        _ (avfilter_init_str filter-context nil)

        _ (doseq [[i input-context] (map-indexed vector input-contexts)]
            (let [err (avfilter_link input-context 0
                                     filter-context i)]
              (when (not (zero? err))
                (throw (Exception.)))))

        err (avfilter_link filter-context 0
                           buffersink-context 0)
        _ (when (not (zero? err))
            (throw (Exception.)))

        err (avfilter_graph_config filter-graph nil)
        _ (when (not (>= err 0))
            (throw (Exception.)))
        filter-graph-ref (volatile! filter-graph)
        _ (.register av/cleaner buffersink-context
                     (fn []
                       (vreset! filter-graph-ref nil)))

        time-base (av_buffersink_get_time_base buffersink-context)
        ch-layout (AVChannelLayoutByReference.)
        _ (av_channel_layout_copy (.getPointer ch-layout)
                                  (.getPointer (:ch-layout (first input-formats))))
        output-format
        {:sample-rate (av_buffersink_get_sample_rate buffersink-context)
         :sample-format (av_buffersink_get_format buffersink-context)
         ;; assume channel layout doesn't change
         :ch-layout ch-layout
         ;; copy structs. known to mutate in place
         :time-base (av/->avrational (:num time-base)
                                     (:den time-base))
         :media-type :media-type/audio}

        frame (av_frame_alloc)]


    {:output-format output-format
     :xform
     (fn [rf]
       (fn
         ([]
          (rf))
         ([result]
          ;; flush
          (doseq [buffer-context input-contexts]
            (av_buffersrc_write_frame buffer-context nil))
          (let [result
                (loop [result result]
                  (let [err (av_buffersink_get_frame_flags buffersink-context
                                                           frame
                                                           0)]
                    (cond
                      (zero? err)
                      (let [result (rf result frame)]
                        (av_frame_unref frame)
                        (if (reduced? result)
                          result
                          (recur result)))

                      (av/eagain? err)
                      result

                      (av/eof? err)
                      result
                      ;; (reduced result)

                      :else
                      (reduced {:error-code err
                                :error-msg (av/error->str err)
                                :type :transcode-error}))))]
            (av_frame_free (doto (PointerByReference.)
                          (.setValue (.getPointer frame))))
            (rf result)))
         ([result [input-index input-frame]]
          (let [buffer-context (nth input-contexts input-index)]
            (av_buffersrc_write_frame buffer-context
                                      input-frame))
          (loop [result result]
            (let [err (av_buffersink_get_frame_flags buffersink-context
                                                     frame
                                                     0)]
              (cond
                (zero? err)
                (let [result (rf result frame)]
                  (av_frame_unref frame)
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
                          :type :transcode-error})))))))}))




(defn filter-state-init [state filter-name input-formats output-format opts]
  (let [filter-graph (avfilter_graph_alloc)
        
        output-format (merge (first input-formats)
                             output-format)
        
        media-type (:media-type output-format)
        
        input-contexts
        (into []
              (map (fn [input-format]
                     (let [buffer (avfilter_get_by_name (dt-ffi/string->c
                                                         (case media-type
                                                           :media-type/audio "abuffer"
                                                           :media-type/video "buffer")))
                           _ (when (nil? buffer)
                               (throw (Exception.)))
                           buffer-context (avfilter_graph_alloc_filter filter-graph buffer nil)
                           
                           args (case media-type
                                  :media-type/audio (format "channel_layout=%s:sample_fmt=%d:sample_rate=%d"
                                                            (datafy-media/ch-layout->str
                                                             (:ch-layout input-format ))
                                                            (:sample-format input-format)
                                                            (:sample-rate input-format))
                                  :media-type/video (format "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d"
                                                            (:width input-format)
                                                            (:height input-format)
                                                            (:pixel-format input-format)
                                                            (-> input-format :time-base :num)
                                                            (-> input-format :time-base :den)))
                           
                           err (avfilter_init_str buffer-context (dt-ffi/string->c args))
                           _ (when (not (zero? err))
                               (throw
                                (ex-info "Could not create audio filter"
                                         {:error-code err
                                          :error-msg (av/error->str err)})))]
                       buffer-context)
                     ))
              input-formats)
        
        buffersink (avfilter_get_by_name (dt-ffi/string->c
                                          (case media-type
                                            :media-type/audio "abuffersink"
                                            :media-type/video "buffersink")))
        _ (when (nil? buffersink)
            (throw (Exception.)))
        buffersink-context* (dt-ffi/make-ptr :pointer 0)
        _ (avfilter_graph_create_filter buffersink-context*
                                        buffersink
                                        nil
                                        nil
                                        nil
                                        filter-graph)
        
        buffersink-context (first buffersink-context*)
        
        ;; sample-fmts (doto (IntByReference.)
        ;;               (.setValue (:sample-format (first input-formats))))
        _ (case media-type
            :media-type/audio
            (let [sample-fmts (dt-ffi/make-ptr :int32 (:sample-format output-format)) ]
              (av_opt_set buffersink-context
                          (dt-ffi/string->c "ch_layouts") 
                          (dt-ffi/string->c (datafy-media/ch-layout->str (:ch-layout output-format)))
                          AV_OPT_SEARCH_CHILDREN)
              (av_opt_set_bin buffersink-context (dt-ffi/string->c "sample_fmts")
                              sample-fmts
                              (* 1 4)
                              AV_OPT_SEARCH_CHILDREN))
            
            :media-type/video
            (let [pix-fmts (dt-ffi/make-ptr
                            :pointer (:pixel-format output-format))]
              (av_opt_set_bin buffersink-context (dt-ffi/string->c "pix_fmts")
                              pix-fmts
                              (* 1 4)
                              AV_OPT_SEARCH_CHILDREN)))
        
        ;; create the filter
        filter-context (avfilter_graph_alloc_filter
                        filter-graph
                        (avfilter_get_by_name (dt-ffi/string->c filter-name))
                        nil)
        _ (assert filter-context)
        _ (set-filter-context-options filter-context filter-name opts)
        
        _ (avfilter_init_str filter-context nil)
        
        _ (doseq [[i input-context] (map-indexed vector input-contexts)]
            (let [err (avfilter_link input-context 0
                                     filter-context i)]
              (when (not (zero? err))
                (throw (Exception.)))))
        
        err (avfilter_link filter-context 0
                           buffersink-context 0)
        _ (when (not (zero? err))
            (throw (Exception.)))
        
        err (avfilter_graph_config filter-graph nil)
        _ (when (not (>= err 0))
            (throw (Exception.)))
        
        time-base (av_buffersink_get_time_base buffersink-context)

        output-format
        (case media-type
          :media-type/audio {:sample-rate (av_buffersink_get_sample_rate buffersink-context)
                             :sample-format (av_buffersink_get_format buffersink-context)
                             ;; assume channel layout doesn't change
                             :ch-layout (let [ch-layout (dt-struct/new-struct :AVChannelLayout 
                                                                              {:container-type :native-heap})
                                              err (av_buffersink_get_ch_layout buffersink-context ch-layout)]
                                          ch-layout
                                          )
                             ;; copy structs. known to mutate in place
                             :time-base (av/->avrational (:num time-base)
                                                         (:den time-base))
                             :media-type :media-type/audio}
          :media-type/video{:width (av_buffersink_get_w buffersink-context)
                            :height (av_buffersink_get_h buffersink-context)
                            ;; copy. time bases known to mutate in place
                            :time-base (av/->avrational (:num time-base)
                                                        (:den time-base))
                            :pixel-format (av_buffersink_get_format buffersink-context)
                            :media-type :media-type/video})]
    {:output-format output-format
     :filter-graph filter-graph
     :buffersink-context buffersink-context
     :input-contexts input-contexts}))

(defn filter-state-close [state]
  (when-let [filter-graph (:filter-graph state)]
    (avfilter_graph_free (dt-ffi/make-ptr
                          :pointer
                          (-> filter-graph
                              dt-ffi/->pointer
                              .address))))
  (dissoc state :filter-graph))

(defn filter-proc-thread [filter-name
                          opts
                          output-format
                          ;; inputs
                          in-chans
                          fresh-frame-chan
                          ;; outputs
                          ready-frame-chan
                          out-chan
                          eof-chan
                          recycle-frame-chan]
  (let [port->idx (into {}
                        (map-indexed (fn [i ch]
                                       [ch i]))
                        in-chans)]
    (async/thread
     (try
       (loop [state {}
              output-frame nil]
         
         (when (not (:eof? state))
           (async/>!! ready-frame-chan true)
           (let [[msg port] (async/alts!! in-chans)]
             (if (nil? msg)
               ;; frame-chan closed. do cleanup
               (filter-state-close state)
               ;; else, process message
               (case (:type msg)
                 :stream-opened
                 (let [input-format (:format msg)
                       state (assoc-in state [:input-formats (port->idx port)] input-format)
                       
                       state (if (= (count (:input-formats state))
                                    (count in-chans))
                               
                               (let [state (assoc state :input-formats (into [] 
                                                                             (->> (:input-formats state)
                                                                                  (sort-by first)
                                                                                  (map second))))
                                     state (filter-state-init state filter-name (:input-formats state) output-format opts)]
                                 (async/>!! out-chan {:type :stream-opened
                                                      :format (:output-format state)})
                                 state)
                               ;; else
                               state)]
                   (recur state output-frame))
                 
                 :new-frame
                 (let [
                       input-frame (:frame msg)
                       
                       buffer-context (nth (:input-contexts state) (port->idx port))]
                   
                   ;; write frame
                   (av_buffersrc_write_frame buffer-context input-frame)
                   (async/put! recycle-frame-chan input-frame)
                   
                   ;; try to get next frame
                   (let [[state output-frame] 
                         (loop [output-frame output-frame]
                           (let [output-frame (or output-frame
                                                  (async/<!! fresh-frame-chan))
                                 err (av_buffersink_get_frame_flags (:buffersink-context state)
                                                                    output-frame
                                                                    0)]
                             (cond
                               (zero? err)
                               (do (async/>!! out-chan {:type :new-frame
                                                        :frame output-frame})
                                   (recur nil))
                               
                               (av/eagain? err) [state output-frame]
                               
                               (av/eof? err)
                               (let [;; not totally sure what the right thing to do is
                                     ;; for now, close everything and stop receiving input
                                     state (-> state
                                               (filter-state-close)
                                               (assoc :eof? true)
                                               (assoc :closed #{})
                                               (dissoc :input-formats))]
                                 (async/>!! out-chan {:type :stream-closed})
                                 (async/>!! eof-chan true)
                                 [state output-frame])
                               
                               :else
                               (throw (ex-info "Error filtering" 
                                               {:filter-name filter-name
                                                :opts opts
                                                :error-code err
                                                :error-msg (av/error->str err)})))))]
                     (recur state output-frame)))
                 
                 :stream-closed
                 (let [_ (av_buffersrc_write_frame (nth (:input-contexts state) (port->idx port)) nil)
                       
                       output-frame
                       (loop [output-frame output-frame]
                         (let [output-frame (or output-frame
                                                (async/<!! fresh-frame-chan))
                               err (av_buffersink_get_frame_flags (:buffersink-context state)
                                                                  output-frame
                                                                  0)]
                           (cond
                             (zero? err) (do (async/>!! out-chan {:type :new-frame
                                                                  :frame output-frame})
                                             (recur nil))
                             
                             (av/eagain? err) output-frame
                             
                             
                             (av/eof? err) output-frame
                             
                             :else
                             (throw (ex-info "Error filtering" 
                                             {:filter-name filter-name
                                              :opts opts
                                              :error-code err
                                              :error-msg (av/error->str err)})))))
                       
                       state (update state :closed (fnil conj #{}) (port->idx port))
                       state (if (< (count (:closed state))
                                    (count in-chans))
                               state
                               ;; else, everyone is closed.
                               ;; cleanup
                               (do 
                                 (async/>!! out-chan {:type :stream-closed})
                                 (async/>!! eof-chan true)
                                 (-> state
                                     (filter-state-close)
                                     (assoc :closed #{})
                                     (assoc :eof? true)
                                     (dissoc :input-formats))))]
                   (recur state output-frame)))))))
       (catch Throwable t
         (tap> t)
         (prn t))
       (finally
         (run! async/close! in-chans)
         (async/close! ready-frame-chan)

         (println "exiting filter"))))))

(defn wrap-filter-input-filter [ins transform]
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
          
          state (if (:eof? state)
                  (dissoc state ::flow/input-filter)
                  (case (:status state)
                    (:closed :opening) (assoc state
                                              ::flow/input-filter
                                              (fn [id] 
                                                (or (not (contains? ins id))
                                                    (and (not (contains? (:ready-ins state)
                                                                         id))
                                                         (:ready? state)))))
                    (:open :closing) (assoc state
                                            ::flow/input-filter 
                                            (fn [id]
                                              (or (not (contains? ins id))
                                                  (and (contains? (:ready-ins state) id)
                                                       (:ready? state)))))))]
      [state outs])))

(defn filter-proc
  "`ins` be a vector of [id doc].
  
  `ins` must be ordered because filter inputs are ordered."
  [ins]
  {:describe (fn []
               {:params {:filter-name "Name of the avfilter"
                         :filter-options "Options to pass to the filter" 
                         ::impl.flow/fresh-frame-chan "Channel to get fresh frames from."
                         :output-format "optional output-format"}
                :ins (into {} ins)
                :outs {:out "filtered frames"
                       ::impl.flow/recycle-frame "Frames to recycle"}})
   :init (fn [{:keys [filter-name opts]
               ::impl.flow/keys [fresh-frame-chan]
               :as state}]
           (let [internal-in-chans (repeatedly (count ins) #(async/chan))
                 in->internal (into {}
                                    (map (fn [[id doc]]
                                           [id (keyword "internal" (name id))]))
                                    ins)

                 internal-ready-for-frame-chan (async/chan 1)
                 internal-recycle-chan (async/chan 1)
                 internal-output-frame-chan (async/chan 5)
                 internal-eof-chan (async/chan 1)]
             (filter-proc-thread filter-name
                                 opts
                                 (:output-format state)
                                 internal-in-chans
                                 fresh-frame-chan
                                 internal-ready-for-frame-chan
                                 internal-output-frame-chan
                                 internal-eof-chan
                                 internal-recycle-chan)
             (assoc state
                    :status :closed
                    :ready? true
                    :ready-ins #{}
                    :in->internal in->internal
                    ::flow/in-ports {:internal/ready-for-frame internal-ready-for-frame-chan
                                     :internal/output-frame internal-output-frame-chan
                                     :internal/recycle internal-recycle-chan
                                     :internal/eof internal-eof-chan}
                    ::flow/out-ports (zipmap (map second in->internal)  
                                             internal-in-chans))))
   :transition (fn [state status]
                 (if  (= status ::flow/stop)
                   (do
                     (-> state ::flow/in-ports :internal/ready-for-frame  async/close!)
                     (-> state ::flow/in-ports :internal/output-frame  async/close!)
                     (-> state ::flow/in-ports :internal/recycle  async/close!)
                     (doseq [[k ch] (::flow/out-ports state)]
                       (async/close! ch))
                     state)
                   state))
   :transform
   (wrap-filter-input-filter
    (into #{} (map first) ins)
    (fn [state in msg]
      (case in
        :internal/eof [(assoc state :eof? true)]
        :internal/ready-for-frame [(assoc state :ready? true)]
        :internal/recycle [state
                           {::impl.flow/recycle-frame [msg]}]
        :internal/output-frame [state {:out [msg]}]
        
        ;; else
        
        (if (:eof? state)
          [state (when-let [frame (:frame msg)]
                   {::impl.flow/recycle-frame [frame]})]
          [(assoc state :ready? false)
           {(get-in state [:in->internal in]) [msg]}]))))})


(defrecord AVFilterMedia [filter-name opts media-type media]
  fm/IComputeNode
  (configure! [this input-ports]
    #_(let [matches-media-type?
          (fn [port]
            (= media-type
               (-> port :format :media-type)))

          unfiltered-ports
          (into []
                (remove matches-media-type?)
                input-ports)

          filtered-port-info
          (into []
                (comp
                 (filter matches-media-type?)
                 (map (fn [port]
                        (let [f (case media-type
                                  :media-type/audio audio-filter
                                  :media-type/video video-filter)

                              port-id [this
                                       (:id port)]

                              {:keys [output-format
                                      xform]}
                              (f [(:format port)] filter-name opts)

                              xf (comp (map (fn [frame]
                                              [0 frame]))
                                       xform)


                              rf (xf
                                  (fn sub
                                    ([send]
                                     (sub send nil)
                                     (send port-id))
                                    ([send frame]
                                     (send port-id frame)
                                     ;; always return send
                                     send)))

                              subscriptions
                              {(:id port) rf}]
                          {:subscriptions subscriptions
                           :port {:id port-id
                                  :format output-format}}))))
                input-ports)]
      {:subscriptions
       (transduce (map :subscriptions)
                  merge
                  {}
                  filtered-port-info)
       :ports
       (into []
             cat
             [unfiltered-ports
              (eduction
               (map :port)
               filtered-port-info)])})

    )
  fm/IMediaSource
  (-media-inputs [this]
    [media])
  (-media [this]
    #_(mapv (fn [src]
            (let [input-format (fm/-format src)]
              (if (= media-type (:media-type input-format))
                (let [f (case media-type
                          :media-type/audio audio-filter
                          :media-type/video video-filter)
                      {:keys [xform]}
                      (f [input-format] filter-name opts)

                      frames (sequence
                              (comp (map (fn [frame]
                                           [0 frame]))
                                    ;; signal EOF
                                    ;; prevents extra buffering for some filters
                                    (insert-last [0 nil])
                                    xform)
                              (fm/-frames src))
                      first-frame (first frames)
                      output-format
                      (case media-type
                        :media-type/audio
                        (assoc input-format
                               :ch-layout (:ch_layout first-frame)
                               :sample-format (:format first-frame)
                               :sample-rate (:sample_rate first-frame))
                        :media-type/video
                        (assoc input-format
                               :width (:width first-frame)
                               :height (:height first-frame)
                               :pixel-format (:format first-frame)))]
                  (fm/frame-source
                   frames
                   output-format
                   [src]
                   filter-name))
                ;; else, don't change
                src)))
          (fm/-media media)))
  )


(defrecord AVMultiFilterMedia [filter-name opts media-type medias]
  fm/IComputeNode
  (configure! [this input-ports]
    #_(let [matches-media-type? (fn [port]
                                (= media-type
                                   (-> port :format :media-type)))
          unfiltered-ports
          (into []
                (remove matches-media-type?)
                input-ports)

          filter-input-ports
          (into []
                (mapcat
                 (fn [media]
                   (into []
                         (comp
                          (filter matches-media-type?)
                          ;; medias can contain duplicates!
                          (filter (fn [port]
                                    (= media
                                       (:media port)))))
                         input-ports)))
                medias)

          _ (when (not= (count medias)
                        (count filter-input-ports))
              (throw (ex-info (str "Filter " filter-name " expects all input media to have exactly one media of type " media-type)
                              {:filter this})))

          make-filter (case media-type
                        :media-type/audio audio-filter
                        :media-type/video video-filter)

          filter-input-port-id [this :input
                                (into []
                                      (map :id)
                                      filter-input-ports)]
          filter-output-port-id [this :output
                                 (into []
                                       (map :id)
                                       filter-input-ports)]

          input-formats (into []
                              (map :format)
                              filter-input-ports)
          {:keys [xform
                  output-format]}
          (make-filter input-formats
                          filter-name
                          opts)

          rf (xform
              (fn sub
                ([send]
                 (send filter-output-port-id))
                ([send frame]
                 (send filter-output-port-id frame)
                 send)))

          active-inputs (atom (set
                               (range (count filter-input-ports))))
          subscriptions
          (into
           [[filter-input-port-id rf]]
           (comp (map-indexed
                  (fn [i port]
                    [(:id port)
                     (fn sub
                       ([send]
                        (let [remaining (swap! active-inputs disj i)]
                          (sub send nil)
                          (when (empty? remaining)
                            (send filter-input-port-id))))
                       ([send frame]
                        (send filter-input-port-id [i frame])))])))
           filter-input-ports)]
      {:ports
       (into [{:id filter-output-port-id
               :format output-format}]
             unfiltered-ports)
       :subscriptions subscriptions}))

  fm/IMediaSource
  (-media-inputs [this]
    medias)

  (-media [this]
    #_(let [matches-media-type? #(= media-type
                                  (:media-type (fm/-format %)))
          filter-inputs (mapv (fn [media]
                                (filterv matches-media-type? (fm/-media media)))
                              medias)
          _ (when (not (every? #(= 1 (count %))
                               filter-inputs))
              (throw (ex-info (str "Filter " filter-name " expects all input media to have exactly one media of type " media-type)
                              {:filter this})))
          filter-inputs (into []
                              (map first)
                              filter-inputs)

          unfiltered-outputs (into []
                                   (comp (map fm/-media)
                                         cat
                                         (remove matches-media-type?)
                                         (distinct-by #(System/identityHashCode %)))
                                   medias)

          make-filter (case media-type
                        :media-type/audio audio-filter
                        :media-type/video video-filter)
          filter-input-formats (mapv fm/-format filter-inputs)
          {:keys [xform]} (make-filter filter-input-formats filter-name opts)

          frames (sequence
                  xform
                  (apply
                   interleave-all
                   (sequence
                    (map-indexed (fn [input-index src]
                                   (sequence
                                    (map (fn [frame]
                                           [input-index frame]))
                                    (fm/-frames src))))
                    filter-inputs)))
          first-frame (first frames)
          output-format
          (case media-type
            :media-type/audio
            (assoc (first filter-input-formats)
                   :ch-layout (:ch_layout first-frame)
                   :sample-format (:format first-frame)
                   :sample-rate (:sample_rate first-frame))
            :media-type/video
            (assoc (first filter-input-formats)
                   :width (:width first-frame)
                   :height (:height first-frame)
                   :pixel-format (:format first-frame)))
          filtered-output (fm/frame-source frames
                                           output-format
                                           filter-inputs
                                           filter-name)]
      (conj unfiltered-outputs filtered-output))))


(defn concat-filter [input-formats opts]
  #_(let [filter-graph (avfilter_graph_alloc)
        _ (when (nil? filter-graph)
            (throw (Exception. "Failed to create filter graph")))
        _ (let [ptr (.getPointer filter-graph)]
            (.register av/cleaner filter-graph
                       (fn []
                         (avfilter_graph_free
                          (PointerByReference. ptr)))))


        input-contexts
        (into []
              (map (fn [input-format]
                     (case (:media-type input-format)
                       :media-type/audio
                       (let [buffer (avfilter_get_by_name "abuffer")
                             _ (when (nil? buffer)
                                 (throw (Exception.)))
                             buffer-context (avfilter_graph_alloc_filter filter-graph buffer nil)

                             args (format "channel_layout=%s:sample_fmt=%d:sample_rate=%d"
                                          (datafy-media/ch-layout->str
                                           (:ch-layout input-format ))
                                          (:sample-format input-format)
                                          (:sample-rate input-format))

                             err (avfilter_init_str buffer-context args)
                             _ (when (not (zero? err))
                                 (throw (Exception.)))]
                         buffer-context)

                       :media-type/video
                       (let [buffer (avfilter_get_by_name "buffer")
                             _ (when (nil? buffer)
                                 (throw (Exception.)))
                             buffer-context (avfilter_graph_alloc_filter filter-graph buffer nil)
                             time-base (:time-base input-format)
                             args (format "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d"
                                          (:width input-format)
                                          (:height input-format)
                                          (:pixel-format input-format)
                                          (:num time-base)
                                          (:den time-base))

                             err (avfilter_init_str buffer-context args)
                             _ (when (not (zero? err))
                                 (throw (Exception.)))]
                         buffer-context))))
              input-formats)

        output-contexts
        (into []
              (map (fn [input-format]
                     (case (:media-type input-format)
                       :media-type/audio
                       (let [buffersink (avfilter_get_by_name "abuffersink")
                             _ (when (nil? buffersink)
                                 (throw (Exception.)))
                             buffersink-context* (PointerByReference.)
                             _ (avfilter_graph_create_filter buffersink-context*
                                                             buffersink
                                                             nil
                                                             nil
                                                             nil
                                                             filter-graph)

                             buffersink-context (.getValue buffersink-context*)

                             sample-fmts (doto (IntByReference.)
                                           (.setValue (:sample-format input-format)))
                             _ (av_opt_set_bin buffersink-context "sample_fmts"
                                               sample-fmts
                                               (* 1 4)
                                               AV_OPT_SEARCH_CHILDREN)]
                         buffersink-context)

                       :media-type/video
                       (let [buffersink (avfilter_get_by_name "buffersink")
                             _ (when (nil? buffersink)
                                 (throw (Exception.)))
                             buffersink-context* (PointerByReference.)
                             _ (avfilter_graph_create_filter buffersink-context*
                                                             buffersink
                                                             nil
                                                             nil
                                                             nil
                                                             filter-graph)

                             buffersink-context (.getValue buffersink-context*)

                             pix-fmts (doto (IntByReference.)
                                        (.setValue (:pixel-format input-format)))
                             _ (av_opt_set_bin buffersink-context "pix_fmts"
                                               pix-fmts
                                               (* 1 4)
                                               AV_OPT_SEARCH_CHILDREN)]
                         buffersink-context))))
              (take (+ (:v opts) (:a opts)) input-formats))

        ;; create the filter
        filter-name "concat"
        filter-context (avfilter_graph_alloc_filter
                        filter-graph
                        (avfilter_get_by_name filter-name)
                        nil)
        _ (assert filter-context)
        _ (set-filter-context-options filter-context filter-name opts)

        _ (avfilter_init_str filter-context nil)

        _ (doseq [[i input-context] (map-indexed vector input-contexts)]
            (let [err (avfilter_link input-context 0
                                     filter-context i)]
              (when (not (zero? err))
                (throw (Exception.)))))

        _ (doseq [[i output-context] (map-indexed vector output-contexts)]
            (let [err (avfilter_link filter-context i
                                     output-context 0)]
              (when (not (zero? err))
                (throw (Exception.)))))


        err (avfilter_graph_config filter-graph nil)
        _ (when (not (>= err 0))
            (throw (Exception.)))

        _ (doseq [output-context output-contexts]
            (let [filter-graph-ref (volatile! filter-graph)]
              (.register av/cleaner output-context
                         (fn []
                           (vreset! filter-graph-ref nil)))))

        output-formats
        (into []
              (map-indexed
               (fn [i buffersink-context]
                 (let [time-base (av_buffersink_get_time_base buffersink-context)
                       type (av_buffersink_get_type buffersink-context)

                       output-format
                       (merge
                        {:time-base (av/->avrational (:num time-base)
                                                     (:den time-base))}
                        (condp = type
                          AVMEDIA_TYPE_AUDIO
                          (let [ch-layout (AVChannelLayoutByReference.)
                                _ (av_channel_layout_copy (.getPointer ch-layout)
                                                          (.getPointer (:ch-layout (nth input-formats i))))]
                            {:sample-rate (av_buffersink_get_sample_rate buffersink-context)
                             :sample-format (av_buffersink_get_format buffersink-context)
                             ;; assume channel layout doesn't change
                             :ch-layout ch-layout
                             :media-type :media-type/audio})

                          AVMEDIA_TYPE_VIDEO
                          {:width (av_buffersink_get_w buffersink-context)
                           :height (av_buffersink_get_h buffersink-context)
                           :pixel-format (av_buffersink_get_format buffersink-context)
                           :media-type :media-type/video}))]
                   output-format)))
              output-contexts)
        frame (av_frame_alloc)]

    {:output-formats output-formats
     :xform
     (fn [rf]
       (fn
         ([]
          (rf))
         ([result]
          ;; flush
          (doseq [buffer-context input-contexts]
            (av_buffersrc_write_frame buffer-context nil))
          (let [result
                (reduce
                 (fn [result [i output-context]]
                   (loop [result result]
                     (let [err (av_buffersink_get_frame_flags output-context
                                                              frame
                                                              0)]
                       (cond
                         (zero? err)
                         (let [result (rf result [i frame])]
                           (av_frame_unref frame)
                           (if (reduced? result)
                             result
                             (recur result)))

                         (av/eagain? err)
                         result

                         (av/eof? err)
                         result

                         :else
                         (reduced {:error-code err
                                   :error-msg (av/error->str err)
                                   :type :transcode-error})))))
                 result
                 (map-indexed vector output-contexts))]
            (av_frame_free (doto (PointerByReference.)
                             (.setValue (.getPointer frame))))
            (rf result))
          )
         ([result [input-idx input-frame]]
          (let [buffer-context (nth input-contexts input-idx)]
            (av_buffersrc_write_frame buffer-context
                                      input-frame))
          (reduce
           (fn [result [i output-context]]
             (loop [result result]
               (let [err (av_buffersink_get_frame_flags output-context
                                                        frame
                                                        0)]
                 (cond
                   (zero? err)
                   (let [result (rf result [i frame])]
                     (av_frame_unref frame)
                     (if (reduced? result)
                       result
                       (recur result)))

                   (av/eagain? err)
                   result

                   (av/eof? err)
                   result

                   :else
                   (reduced {:error-code err
                             :error-msg (av/error->str err)
                             :type :transcode-error})))))
           result
           (map-indexed vector output-contexts)))))}))


(defrecord AVConcatFilterMedia [opts medias]
  fm/IComputeNode
  (configure! [this input-ports]
    (let [
          port-sort-fn (fn [port]
                           (= :media-type/audio
                              (:media-type (:format port))))
          partitioned-ports
          (into []
                (map (fn [media]
                       (into []
                             (comp
                              ;; medias can contain duplicates!
                              (filter (fn [port]
                                        (= media
                                           (:media port))))
                              (x/sort-by port-sort-fn))
                             input-ports)))
                medias)
          _ (let [fqs (into []
                            (map (fn [ports]
                                   (frequencies
                                    (mapv (fn [port]
                                            (-> port
                                                :format
                                                :media-type))
                                          ports))))
                            partitioned-ports)
                  first-fq (first fqs)]
              (when (not (every? #(= first-fq %)
                                 (rest fqs)))
                (throw (ex-info "All medias must match when concating."
                                {:concat this}))))

          input-formats
          (into []
                (comp cat
                      (map :format))
                partitioned-ports)

          first-media-ports (first partitioned-ports)
          opts
          {:n (count partitioned-ports)
           :v (->> first-media-ports
                   (filter #(= :media-type/video
                               (-> % :format :media-type)))
                   count)
           :a (->> first-media-ports
                   (filter #(= :media-type/audio
                               (-> % :format :media-type)))
                   count)}

          filter-input-port-id [this :input]

          {:keys [xform
                  output-formats]}
          (concat-filter input-formats opts)
          rf (xform
              (fn
                ([send]
                 ;; close all outputs
                 (doseq [i (range (count output-formats))]
                   (let [filter-output-port-id [this :output i]]
                     (send filter-output-port-id))))
                ([send [i frame]]
                 (let [filter-output-port-id [this :output i]]
                   (send filter-output-port-id frame))
                 send)))


          active-inputs (atom (into #{}
                                    (comp cat
                                          (map-indexed
                                           (fn [i _]
                                             i)))
                                    partitioned-ports))

          subscriptions
          (into
           ;; subscriptions is usually a map
           ;; but we might have multiple subscriptions
           ;; for the same port due to duplicate media inputs
           [[filter-input-port-id rf]]
           (comp cat
                 (map-indexed
                  (fn [i port]
                    [(:id port)
                     (fn sub
                       ([send]
                        ;; as each input ends, send a nil frame
                        ;; once all inputs end, close filter-input-port-id.
                        (let [remaining (swap! active-inputs disj i)]
                          (sub send nil)
                          (when (empty? remaining)
                            (send filter-input-port-id))))
                       ([send frame]
                        (send filter-input-port-id [i frame])))])))
           partitioned-ports)

          ports
          (into []
                (map-indexed
                 (fn [i output-format]
                   {:id [this :output i]
                    :format output-format}))
                output-formats)]
      {:ports ports
       :subscriptions subscriptions}))
  fm/IMediaSource
  (-media-inputs [this]
    medias)
  (-media [this]
    (let [all-medias (into []
                           (map fm/-media)
                           medias)
          stream-count (count
                        (first all-medias))
          _ (when (not (every? (fn [streams]
                                 (= (count streams)
                                    stream-count))
                               (rest all-medias)))
              (throw (ex-info "Concat filter expects all media to have the same number and kinds of streams."
                              {:filter opts
                               :medias medias})))

          stream-sort-fn (fn [format]
                           (= :media-type/audio
                              (:media-type format)))
          input-formats
          (into []
                (comp (map (fn [media]
                             (let [formats (into []
                                                 (map fm/-format)
                                                 media)]
                               (sort-by stream-sort-fn
                                        formats))))
                      cat)
                all-medias)

          opts
          {:n (count all-medias)
           :v (->> (first all-medias)
                   (filter fm/video?)
                   count)
           :a (->> (first all-medias)
                   (filter fm/audio?)
                   count)}

          ;; this mess is to try to achieve proper
          ;; interleaving
          interleaved-input-frames
          (sequence
           (comp (map-indexed
                  (fn [segment-idx media]
                    (apply
                     interleave-all
                     (sequence (map-indexed
                                (fn [stream-idx src]
                                  (let [input-idx (+ (* segment-idx stream-count)
                                                     stream-idx)]
                                    (sequence (comp
                                               (map (fn [frame]
                                                      [input-idx frame]))
                                               (insert-last [input-idx nil]))
                                              (fm/-frames src)))))
                               (sort-by
                                (fn [src]
                                  (stream-sort-fn (fm/-format src)))
                                media)))))
                 cat)
           all-medias)

          all-frames
          (sequence
           (concat-filter input-formats opts)
           interleaved-input-frames)

          frames-by-stream
          (into []
                (map (fn [i]
                       (sequence
                        (keep (fn [[stream-idx frame]]
                                (when (= stream-idx i)
                                  frame)))
                        all-frames)))
                (range stream-count))

          outputs
          (into []
                (map (fn [i]
                       (let [frames (nth frames-by-stream i)
                             input-format (nth input-formats i)
                             media-type (:media-type input-format)

                             first-frame (first frames)
                             ;; will this work for streams with different time bases?
                             output-format
                             (case media-type
                               :media-type/audio
                               (assoc input-format
                                      :ch-layout (:ch_layout first-frame)
                                      :sample-format (:format first-frame)
                                      :sample-rate (:sample_rate first-frame))
                               :media-type/video
                               (assoc input-format
                                      :width (:width first-frame)
                                      :height (:height first-frame)
                                      :pixel-format (:format first-frame)))]
                         (fm/frame-source frames
                                          output-format
                                          (into []
                                                cat
                                                all-medias)
                                          "concat"))))
                (range stream-count))]
      outputs)))



(defn filter-fn [filter-info]
  (let [opts## 'opts
        opts-or-media## 'opts-or-media

        default-input? (and (= 1 (count (:inputs filter-info)))
                            (= "default" (-> filter-info
                                             :inputs
                                             first
                                             :name)))

        inputs (if default-input?
                 '[media]
                 (into []
                       (comp (map :name)
                             (map str->symbol))
                       (:inputs filter-info)))

        filter-name (:name filter-info)
        fn-name (str->symbol filter-name)

        supported-options (filterv supported-filter-option? (:options filter-info))
        opt-keys (into []
                       (comp (map :name)
                             (map str->symbol))
                       supported-options)

        opts {:keys opt-keys
              :as opts##}

        consts (->> (:options filter-info)
                    (filter #(= (:type %)
                                :avoption-type/const))
                    (group-by :unit))

        doc-string
        (str filter-name ": " (:description filter-info)
             (when (not default-input?)
               (str
                "\n\n"
                "Inputs: " (str/join
                            ","
                            (eduction
                             (map :name)
                             (:inputs filter-info)))))
             "\n\n"
             "Supported options:

"
             (clojure.string/join
              "\n\n"
              (eduction
               (map (fn [{:keys [name type help default-val min max unit]}]
                      (str (str->kw name) " - " help
                           "\n"
                           (clojure.string/join
                            "\n"
                            (eduction
                             (remove nil?)
                             (map #(str "\t" %))
                             (if-let [const (get consts unit)]
                               (let [has-none? (and (zero? min)
                                                    (not (some (fn [c]
                                                                 (zero? (-> c :default-val :int)))
                                                               const)))
                                     const (if has-none?
                                             (conj const {:name "none"})
                                             const)]
                                 [(str "type: enum" )
                                  (str "default: " (some (fn [c]
                                                           (when (= (-> c :default-val :int)
                                                                    default-val)
                                                             (:name c)))
                                                         const))
                                  (str "values: "
                                       (clojure.string/join
                                        ", "
                                        (eduction
                                         (map :name)
                                         (map #(str "\"" % "\""))
                                         const)))])
                               (case type
                                 :avoption-type/duration
                                 [(str "type: duration in microseconds" )
                                  (when (not (map? default-val))
                                    (str "default: " default-val))
                                  (str "min: " min)
                                  (str "max: " max)]

                                 :avoption-type/bool
                                 [(str "type: " (clojure.core/name type))
                                  (when (not (map? default-val))
                                    (str "default: " default-val))
                                  "values: true, false"]

                                 ;; else
                                 [(str "type: " (clojure.core/name type))
                                  (when (not (map? default-val))
                                    (str "default: " default-val))
                                  (str "min: " min)
                                  (str "max: " max)])))))))
               (sort-by :name supported-options)))

             "\n\n"
             (let [unsupported-options
                   (eduction
                    (remove supported-filter-option?)
                    (remove #(= (:type %)
                                :avoption-type/const))
                    (map :name)
                    (:options filter-info))]
               (when (seq unsupported-options)
                 (str
                  "Unsupported options: "
                  (clojure.string/join ", "
                                       unsupported-options)))))

        media-type (-> filter-info
                       :inputs
                       first
                       :media-type)]
    `(defn ~fn-name
       ~doc-string
       ([~@inputs]
        ;; some filters use the same name for options as the name of the filter
        ;; needs to be qualified
        (~(symbol (name (ns-name *ns*))
                  (name fn-name))
         nil ~@inputs))
       ([~opts ~@inputs]
        {:type :avfilter
         :filter-name ~filter-name
         :opts ~opts##
         :inputs ~inputs}))))


(defn supported-filter-type? [filter-info]
  (and (= 1 (count (:outputs filter-info)))
       (every? #(= (:media-type (first (:inputs filter-info)))
                   (:media-type %))
               (rest (:inputs filter-info)))
       (= (:media-type (first (:outputs filter-info)))
          (:media-type (first (:inputs filter-info))))))

(defmacro make-fns []
  `(do
     ~@(into []
             (comp (filter supported-filter-type?)
                   (map filter-fn))
             (list-filters))))


(defrecord ForceFormat [format media]
  fm/IMediaSource
  (-media-inputs [this]
    [media])
  (-media [this]
    (mapv (fn [src]
            (fm/frame-source
             (fm/-frames src)
             (merge
              (fm/-format src)
              (datafy-media/map->format format))
             [src]
             "force-format"))
          (fm/-media media))))

(defn force-format
  "Used for testing."
  [format media]
  (->ForceFormat format media))

(defrecord Swscale [opts output-format media]
  fm/IComputeNode
  (configure! [this input-ports]
    (let [
          subscriptions
          (into {}
                  (comp
                   (filter (fn [port]
                             (let [input-format (:format port)]
                               (= :media-type/video
                                  (:media-type input-format)))))
                   (map (fn [port]
                          (let [
                                input-format (:format port)
                                output-format
                                (if output-format
                                  (merge
                                   input-format
                                   (datafy-media/map->format output-format
                                                             :media-type/video))
                                  input-format)
                                xf
                                (video/swscale input-format output-format opts)
                                rf
                                (xf
                                 (fn sub
                                   ([send]
                                    (sub send nil)
                                    (send [this (:id port)]))
                                   ([send frame]
                                    (send [this (:id port)] frame)
                                    send)))]
                            [(:id port)
                             rf]))))
                  input-ports)
          ports
          (into []
                (map (fn [port]
                       (let [input-format (:format port)]
                         (if (= :media-type/video
                                (:media-type input-format))
                           (let [output-format
                                 (if output-format
                                   (merge
                                    input-format
                                    (datafy-media/map->format output-format
                                                              :media-type/video))
                                   input-format)]
                             {:id [this (:id port)]
                              :format output-format})
                           ;; else
                           port))))
                input-ports)]
      {:ports ports
       :subscriptions subscriptions}))


  fm/IMediaSource
  (-media-inputs [this]
    [media])
  (-media [this]
    (mapv (fn [src]
            (if (fm/video? src)
              (let [input-format (fm/-format src)
                    output-format
                    (merge
                     input-format
                     (datafy-media/map->format output-format
                                               :media-type/video))]
                (fm/frame-source
                 (sequence
                  (video/swscale input-format output-format opts)
                  (fm/-frames src))
                 output-format
                 [src]
                 "swscale"))
              src))
          (fm/-media media))))

(defn swscale [opts output-format media]
  (->Swscale opts output-format media))

