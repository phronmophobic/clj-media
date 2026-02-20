(ns com.phronemophobic.clj-media.impl.filter.avfilter
  (:require [clojure.string :as str]
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
                          recycle-frame-chan
                          error-chan]
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
                   (async/>!! recycle-frame-chan input-frame)
                   
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
         (prn t)
         (async/put! error-chan t))
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
                 internal-eof-chan (async/chan 1)
                 internal-error-chan (async/chan 1)]
             (filter-proc-thread filter-name
                                 opts
                                 (:output-format state)
                                 internal-in-chans
                                 fresh-frame-chan
                                 internal-ready-for-frame-chan
                                 internal-output-frame-chan
                                 internal-eof-chan
                                 internal-recycle-chan
                                 internal-error-chan)
             (assoc state
                    :status :closed
                    :ready? true
                    :ready-ins #{}
                    :in->internal in->internal
                    ::flow/in-ports {:internal/ready-for-frame internal-ready-for-frame-chan
                                     :internal/output-frame internal-output-frame-chan
                                     :internal/recycle internal-recycle-chan
                                     :internal/eof internal-eof-chan
                                     :internal/error internal-error-chan}
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
        :internal/error (throw msg)
        :internal/output-frame [state {:out [msg]}]
        
        ;; else
        
        (if (:eof? state)
          [state (when-let [frame (:frame msg)]
                   {::impl.flow/recycle-frame [frame]})]
          [(assoc state :ready? false)
           {(get-in state [:in->internal in]) [msg]}]))))})


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



