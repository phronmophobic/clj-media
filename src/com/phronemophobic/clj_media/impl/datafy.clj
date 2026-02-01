(ns com.phronemophobic.clj-media.impl.datafy
  (:require [clojure.string :as str]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [tech.v3.datatype.struct :as dt-struct]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [com.phronemophobic.clj-media.impl.util
             :refer [normalize-str
                     str->kw]]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all])
  (:import
   java.nio.ByteOrder
   java.nio.ByteBuffer
   sun.misc.Unsafe
   tech.v3.datatype.ffi.Pointer
   tech.v3.datatype.struct.Struct
   ;; com.sun.jna.Memory
   ;; com.sun.jna.Structure
   ;; com.sun.jna.Pointer
   ;;com.sun.jna.ptr.PointerByReference
   ))

;; (raw/import-structs!)



(defn ch-layout->str [ch-layout]
  (let [buf (native-buffer/malloc 512)
        err (av_channel_layout_describe ch-layout buf (native-buffer/native-buffer-byte-len buf))
        _ (when (neg? err)
            (throw (ex-info "Could not encode ch-layout."
                            {:ch-layout ch-layout
                             :err err})))]
    (native-buffer/native-buffer->string buf 0 (max (dec err) 0))))

(defn str->ch-layout [s]
  (assert s "Invalid ch-layout.")
  (let [;; ch-layout (AVChannelLayoutByReference.)
        ch-layout (dt-struct/new-struct :AVChannelLayout
                                        {:container-type :native-heap})
        err (av_channel_layout_from_string ch-layout (dt-ffi/string->c s))]
    (when (neg? err)
      (throw (ex-info "Invalid channel layout."
                      {:channel-layout-str s})))
    ch-layout))

(comment
  


  (def my-cl (let [cl (dt-struct/new-struct :AVChannelLayout
                                            {:container-type :native-heap})
                   s (dt-ffi/string->c "stereo")]
               (av_channel_layout_from_string cl s)
               cl))
  (ch-layout->str my-cl)
  ,)

(defn pointer-seq [addr size terminal]
  (when (not (zero? addr))
    (loop [addr addr 
           results []]
      
      (when (> (count results) 100)
        (throw (ex-info "should have stopped by now" {})))
      (let [x (case size
                4 (.getInt (native-buffer/unsafe) addr)
                8 (.getLong (native-buffer/unsafe) addr))]
        (if (= x terminal)
          results
          (recur (+ addr size)
                 (conj results x)))))))

(def ^:private avrational-size (:datatype-size
                                (dt-struct/get-struct-def :AVRational)))
(defn avrational-seq [p]
  (when (not (zero? p))
    (loop [p p
           results []]
      (when (> (count results) 100)
        (throw (ex-info "should have stopped by now" {})))
      (let [ratio (dt-ffi/ptr->struct :AVRational p)
            ;;(Structure/newInstance AVRationalByReference p)
            ]
        (if (and (zero? (:num ratio))
                 (zero? (:den ratio)))
          results
          (recur ;;(.share p avrational-size)
                 (+ p avrational-size)
                 (conj results ratio)))))))



(def ^:private avchannellayout-size (:datatype-size
                                     (dt-struct/get-struct-def :AVChannelLayout)))
(defn avchannellayout-seq [p]
  (when (not (zero? p))
    (loop [p p
           results []]
      (when (> (count results) 100)
        (throw (ex-info "should have stopped by now" {})))

      (let [bs (native-buffer/wrap-address p avchannellayout-size)
            ;;(.getByteArray p 0 avchannellayout-size)
            ]
        (if (every? zero? bs)
          results
          (let [layout (dt-ffi/ptr->struct :AVChannelLayout p)]
            (recur ;; (.share p avchannellayout-size)
                   (+ p avchannellayout-size)
                   (conj results layout))))))))

(def avoption-type->kw
  (->> (:enums raw/av-api)
       (filter (fn [enum]
                 (= "AVOptionType" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "avoption-type"
                             (-> (subs (:name enum)
                                       (count "AV_OPT_TYPE_"))
                                 str/lower-case
                                 (str/replace #"_" "-"))))))
       (into {})))

(defmulti read-bytes (fn [type bs]
                       type))
(defmethod read-bytes :avoption-type/int64
  [_ bs]
  (native-buffer/read-long bs))

(defmethod read-bytes :avoption-type/int
  [_ bs]
  (native-buffer/read-int bs))

(defmethod read-bytes :avoption-type/bool
  [_ bs]
  (let [num (read-bytes :avoption-type/uint64 bs)]
    (not (zero? num))))

(defmethod read-bytes :avoption-type/uint64
  [_ bs]
  (let [bs (if (= (ByteOrder/nativeOrder)
                  ByteOrder/LITTLE_ENDIAN)
             (byte-array (reverse bs))
             (byte-array bs))
        val (BigInteger. 1 bs)]
    val))

(defmethod read-bytes :avoption-type/double
  [_ bs]
  (native-buffer/read-double bs))

(defmethod read-bytes :avoption-type/float
  [_ bs]
  (native-buffer/read-float bs))

(defmethod read-bytes :avoption-type/string
  [_ bs]
  (let [ptr-native (read-bytes :avoption-type/int64 bs )]
    (when (not (zero? ptr-native))
      (dt-ffi/c->string (dt-ffi/->pointer ptr-native)))))

(defmethod read-bytes :default
  [type bs]
  {:type type
   :bs bs
   :float (read-bytes :avoption-type/float bs)
   :double (read-bytes :avoption-type/double bs)
   :long (read-bytes :avoption-type/int64 bs)
   :int (read-bytes :avoption-type/int bs)})


(defmulti datafy-struct (fn [^Struct s]
                          (Struct/.datatype s)))

(extend-protocol p/Datafiable
  Struct
  (datafy [s]
    (datafy-struct s)))

#_(extend-protocol p/Datafiable
  AVClassByReference
  (datafy [cls]
    (when cls
      {:options
       (let [cls* (PointerByReference. (.getPointer cls))]
         (loop [prev nil
                opts []]
           (let [o (av_opt_next cls* prev)]
             (if o
               (recur o (conj opts (d/datafy o)))
               opts))))})))


(defmethod datafy-struct :AVClass [cls]
  {:options
   (let [cls* (dt-ffi/make-ptr :pointer (.address (dt-ffi/->pointer cls)))]
     (loop [prev nil
            opts []]
       (let [o (av_opt_next cls* prev)]
         (if o
           (recur o (conj opts (d/datafy o)))
           opts))))})

(defn filter-options [flt]
  (let [cls (:priv_class flt)]
    (when cls
      (let [cls* (dt-ffi/make-ptr :pointer cls)]
        (loop [prev nil
               opts []]
          (let [o (av_opt_next cls* prev)]
            (if o
              (recur o (conj opts (d/datafy o)))
              opts)))))))

#_(extend-protocol p/Datafiable
  AVFilterByReference
  (datafy [flt]
    (merge
     {:name 
      ;;(.getString (.getPointer (:name flt)) 0 "ascii")
      (dt-ffi/c->string (:name flt))
      :options (filter-options flt)}
     (when-let [description (dt-ffi/c->string (:description flt))]
       {:description description
        ;;(.getString description 0 "ascii")
        })
     (when-let [inputs (:inputs flt)]
       (let [size (avfilter_filter_pad_count flt 0)]
         {:inputs
          (into []
                (map (fn [i]
                       (let [name (avfilter_pad_get_name inputs i)
                             type (avfilter_pad_get_type inputs i)]
                         {:media-type
                          (condp = type
                            AVMEDIA_TYPE_AUDIO :media-type/audio
                            AVMEDIA_TYPE_VIDEO :media-type/video)
                          :name name})))
                (range size))}))
     (when-let [outputs (:outputs flt)]
       (let [size (avfilter_filter_pad_count flt 1)]
         {:outputs
          (into []
                (map (fn [i]
                       (let [name (avfilter_pad_get_name outputs i)
                             type (avfilter_pad_get_type outputs i)]
                         {:media-type
                          (condp = type
                            AVMEDIA_TYPE_AUDIO :media-type/audio
                            AVMEDIA_TYPE_VIDEO :media-type/video)
                          :name (dt-ffi/c->string name)})))
                (range size))})))))

(defmethod datafy-struct :AVFilter [flt]
  (merge
   {:name 
    ;;(.getString (.getPointer (:name flt)) 0 "ascii")
    (dt-ffi/c->string (:name flt))
    :options (filter-options flt)}
   (when-let [description (dt-ffi/c->string (:description flt))]
     {:description description
      ;;(.getString description 0 "ascii")
      })
   (when-let [inputs (:inputs flt)]
     (let [size (avfilter_filter_pad_count flt 0)]
       {:inputs
        (into []
              (map (fn [i]
                     (let [name (avfilter_pad_get_name inputs i)
                           type (avfilter_pad_get_type inputs i)]
                       {:media-type
                        (condp = type
                          AVMEDIA_TYPE_AUDIO :media-type/audio
                          AVMEDIA_TYPE_VIDEO :media-type/video)
                        :name (dt-ffi/c->string name)})))
              (range size))}))
   (when-let [outputs (:outputs flt)]
     (let [size (avfilter_filter_pad_count flt 1)]
       {:outputs
        (into []
              (map (fn [i]
                     (let [name (avfilter_pad_get_name outputs i)
                           type (avfilter_pad_get_type outputs i)]
                       {:media-type
                        (condp = type
                          AVMEDIA_TYPE_AUDIO :media-type/audio
                          AVMEDIA_TYPE_VIDEO :media-type/video)
                        :name (dt-ffi/c->string name)})))
              (range size))}))))

#_(extend-protocol p/Datafiable
  AVOptionByReference
  (datafy [opt]
    (let [option-type (avoption-type->kw (:type opt))]
      (merge
       {:name (.getString (.getPointer (:name opt)) 0 "ascii")
        :offset (:offset opt)
        :type option-type}
       (when-let [help (:help opt)]
         {:help (.getString (.getPointer help) 0 "ascii")})
       (when-let [default (:default_val opt)]
         
         (let [buf (native-buffer/wrap-address (+ (.address opt))
                    )]{:default-val (read-bytes option-type default)}))
       (when-let [min (:min opt)]
         {:min min})
       (when-let [max (:max opt)]
         {:max max})
       (when-let [unit (:unit opt)]
         {:unit (.getString (.getPointer unit) 0 "ascii")})))))

(def ^:private default-val-layout
  (-> (dt-struct/get-struct-def :AVOption)
      :layout-map
      :default_val
      ))
(defmethod datafy-struct :AVOption [opt]
  (let [option-type (avoption-type->kw (:type opt))]
    (merge
     {:name (dt-ffi/c->string (:name opt))
      :offset (:offset opt)
      :type option-type}
     (when-let [help (:help opt)]
       {:help (dt-ffi/c->string help)})
     (when-let [default (:default_val opt)]
       (let [buf (native-buffer/wrap-address (+ (-> opt dt-ffi/->pointer .address)
                                                (:offset default-val-layout))
                                             (:n-elems default-val-layout)
                                             opt)]
         {:default-val (read-bytes option-type buf)}))
     (when-let [min (:min opt)]
       {:min min})
     (when-let [max (:max opt)]
       {:max max})
     (when-let [unit (:unit opt)]
       {:unit (dt-ffi/c->string unit)}))))


(def media-type->kw
  {AVMEDIA_TYPE_ATTACHMENT :media-type/attachment
   AVMEDIA_TYPE_AUDIO      :media-type/audio
   AVMEDIA_TYPE_DATA       :media-type/data
   AVMEDIA_TYPE_NB         :media-type/nb
   AVMEDIA_TYPE_SUBTITLE   :media-type/subtitle
   AVMEDIA_TYPE_UNKNOWN    :media-type/unknown
   AVMEDIA_TYPE_VIDEO      :media-type/video})

(def pixel-format->kw
  (->> (:enums raw/av-api)
       (filter (fn [enum]
                 (= "AVPixelFormat" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "pixel-format"
                             (-> (subs (:name enum)
                                       (count "AV_PIX_FMT_"))
                                 normalize-str)))))
       (into {})))

(def kw->pixel-format
  (into
   {}
   (map (fn [[k v]]
          [v k]))
   pixel-format->kw))

(def sample-format->kw
  (->> (:enums raw/av-api)
       (filter (fn [enum]
                 (= "AVSampleFormat" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "sample-format"
                             (-> (subs (:name enum)
                                       (count "AV_SAMPLE_FMT_"))
                                 normalize-str)))))
       (into {})))

(def kw->sample-format
  (into
   {}
   (map (fn [[k v]]
          [v k]))
   sample-format->kw))


(def channel-order->kw
  (->> (:enums raw/av-api)
       (filter (fn [enum]
                 (= "AVChannelOrder" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "channel-order"
                             (-> (subs (:name enum)
                                       (count "AV_CHANNEL_ORDER_"))
                                 str/lower-case
                                 (str/replace #"_" "-"))))))
       (into {})))

(def channel->kw
  (->> (:enums raw/av-api)
       (filter (fn [enum]
                 (= "AVChannel" (:enum enum))))
       (map (juxt :value
                  (fn [enum]
                    (keyword "channel"
                             (-> (subs (:name enum)
                                       (count "AV_CHAN_"))
                                 str/lower-case
                                 (str/replace #"_" "-"))))))
       (into {})))

(defn avchannellayout->map [p]
  (let [order (channel-order->kw (:order p))]
    (merge
     {:order order
      :name (ch-layout->str p)
      :nb-channels (:nb_channels p)}
     (when (= :channel-order/native)
       (let [bs (:u p)
             bs (if (= (ByteOrder/nativeOrder)
                       ByteOrder/LITTLE_ENDIAN)
                  (byte-array (reverse bs))
                  (byte-array bs))
             mask (BigInteger. 1 bs)
             channels (into []
                            (comp (remove (fn [[num kw]]
                                            (= kw :channel/none)))
                                  (keep (fn [[num kw]]
                                          (when (not= (.and (BigInteger/valueOf num) mask)
                                                      BigInteger/ZERO)
                                            kw))))
                            (sort-by first
                                     channel->kw))]
         {:channels channels})))))

(defn codec->map [codec]
  (merge
   {:name (dt-ffi/c->string (:name codec))
    :long-name (dt-ffi/c->string (:long_name codec))
    :media-type (media-type->kw (:type codec))
    :id (:id codec)}
   (let [supported-framerates (:supported_framerates codec)]
     (when (not (zero? supported-framerates))
       {:supported-framerates
        (into []
              (map (fn [ratio]
                     (/ (:num ratio)
                        (:den ratio))))
              (avrational-seq supported-framerates))}))
   (let [pix-fmts (:pix_fmts codec)]
     (when (not (zero? pix-fmts))
       {:pixel-formats
        (into []
              (map pixel-format->kw)
              (pointer-seq pix-fmts
                           4 -1))}))
   (let [sample-rates (:supported_samplerates codec)]
     (when (not (zero? sample-rates))
       {:sample-rates
        (into []
              (pointer-seq sample-rates 4 0))}))
   (let [sample-fmts (:sample_fmts codec)]
     (when (not (zero? sample-fmts))
       {:sample-formats
        (into []
              (map sample-format->kw)
              (pointer-seq sample-fmts
                           4 -1))}))
   (let [channel-layouts (:ch_layouts codec)]
     (when (not (zero? channel-layouts))
       {:channel-layouts
        (into []
              (map avchannellayout->map)
              (avchannellayout-seq channel-layouts))}))))

#_(extend-protocol p/Datafiable
  AVRational
  (datafy [ratio]
    [(:num ratio)
     (:den ratio)])
  AVRationalByReference
  (datafy [ratio]
    [(:num ratio)
     (:den ratio)]))

(defmethod datafy-struct :AVRational [ratio]
  [(:num ratio)
   (:den ratio)])

#_(extend-protocol p/Datafiable
  AVCodecByReference
  (datafy [codec]
    (codec->map codec)))

(defmethod datafy-struct :AVCodec [codec]
  (codec->map codec))

#_(extend-protocol p/Datafiable
  AVChannelLayout
  (datafy [codec]
    (avchannellayout->map codec))

  AVChannelLayoutByReference
  (datafy [codec]
    (avchannellayout->map codec)))

(defmethod datafy-struct :AVChannelLayout [cl]
  (avchannellayout->map cl))

#_(extend-protocol p/Datafiable
  AVCodecParametersByReference
  (datafy [params]
    (let [media-type (media-type->kw (:codec_type params))]
      (case media-type
        (:media-type/audio
         :media-type/video)
        (let [codec (avcodec_find_decoder (:codec_id params))]
          (merge
           {:media-type media-type
            :codec (d/datafy codec)
            :bit-rate (:bit_rate params)}
           (case media-type
             :media-type/audio
             {:ch-layout (d/datafy (:ch_layout params))
              :sample-rate (:sample_rate params)
              :frame-size (:frame_size params)
              :bits-per-coded-sample (:bits_per_coded_sample params)
              :bits-per-raw-sample (:bits_per_raw_sample params)
              :sample-format (sample-format->kw (:format params))}
             :media-type/video
             {:width (:width params)
              :height (:height params)
              :video-delay (:video_delay params)
              :pixel-format (pixel-format->kw (:format params))})))

        ;; else
        {:media-type media-type}))))

(defmethod datafy-struct :AVCodecParameters [params]
  (let [media-type (media-type->kw (:codec_type params))]
    (case media-type
      (:media-type/audio
       :media-type/video)
      (let [codec (avcodec_find_decoder (:codec_id params))]
        (merge
         {:media-type media-type
          :codec (d/datafy codec)
          :bit-rate (:bit_rate params)}
         (case media-type
           :media-type/audio
           {:ch-layout (d/datafy (:ch_layout params))
            :sample-rate (:sample_rate params)
            :frame-size (:frame_size params)
            :bits-per-coded-sample (:bits_per_coded_sample params)
            :bits-per-raw-sample (:bits_per_raw_sample params)
            :sample-format (sample-format->kw (:format params))}
           :media-type/video
           {:width (:width params)
            :height (:height params)
            :video-delay (:video_delay params)
            :pixel-format (pixel-format->kw (:format params))})))
      
      ;; else
      {:media-type media-type})))


(defn ->avrational [num den]
  (dt-struct/map->struct :AVRational {:num num :den den} :gc)
  #_(doto (AVRational.)
    (.writeField "num" (int num))
    (.writeField "den" (int den))))

(defn clj->avrational [o]
  (cond
    (and (instance? Struct o)
         (= :AVRational (Struct/.datatype o)))
    o

    (ratio? o)
    (->avrational (numerator o)
                  (denominator o))
    (vector? o)
    (->avrational (first o)
                  (second o))

    ;; treat as fps
    (integer? o)
    (->avrational 1 o)

    :else
    (throw (ex-info "Invalid rational."
                    {:o o}))))

(defn map->format
  ([format media-type]
   (map->format (assoc format
                       :media-type media-type)))
  ([format]
   (merge
    (case (:media-type format)
      :media-type/audio
      (assoc format
             :ch-layout
             (doto (str->ch-layout
                    (:channel-layout format))
               .read)
             :sample-format (int (kw->sample-format
                                  (:sample-format format)))
             :sample-rate (int (:sample-rate format)))

      :media-type/video
      (assoc format
             :pixel-format (int (kw->pixel-format (:pixel-format format)))))
    (when-let [time-base (:time-base format)]
      {:time-base (clj->avrational time-base)}))))


(defmulti set-option (fn [o type k v]
                       type))



#_(defmethod set-option :avoption-type/flags
  [o _ k v])
(defmethod set-option :avoption-type/float
  [o _ k v]
  (av_opt_set_double o (dt-ffi/string->c k) v AV_OPT_SEARCH_CHILDREN))
(defmethod set-option :avoption-type/rational
  [o _ k v]
  (when-not (ratio? v)
    (throw (ex-info "Option type :avoption-type/rational must be set with Ratio."
                    {:o o
                     :k k
                     :v v})))
  (av_opt_set_q o (dt-ffi/string->c k)
                (->avrational (numerator v)
                              (denominator v))
                AV_OPT_SEARCH_CHILDREN))
(defmethod set-option :avoption-type/duration
  [o _ k v]
  (set-option o :avoption-type/int64 k v))
(defmethod set-option :avoption-type/int64
  [o _ k v]
  (av_opt_set_int o (dt-ffi/string->c k) v AV_OPT_SEARCH_CHILDREN))
(defmethod set-option :avoption-type/double
  [o _ k v]
  (av_opt_set_double o (dt-ffi/string->c k) v AV_OPT_SEARCH_CHILDREN))
(defmethod set-option :avoption-type/int
  [o _ k v]
  (av_opt_set_int o (dt-ffi/string->c k) v AV_OPT_SEARCH_CHILDREN))
#_(defmethod set-option :avoption-type/dict
  [o _ k v])
(defmethod set-option :avoption-type/image-size
  [o _ k v]
  (let [[w h] v]
   (av_opt_set_image_size o (dt-ffi/string->c k) w h AV_OPT_SEARCH_CHILDREN)))
(defmethod set-option :avoption-type/video-rate
  [o _ k v]
  (let [ratio
        (cond
          (ratio? v) (->avrational (numerator v) (denominator v))
          (integer? v) (->avrational 1 v)
          (and (seqable? v)
               (= 2 (count v))) (->avrational (first v) (second v))
          :else (throw
                 (ex-info "Could not set video rate"
                          {:o o
                           :k k
                           :v v})))]
   (av_opt_set_video_rate o k ratio AV_OPT_SEARCH_CHILDREN)))
(defmethod set-option :avoption-type/string
  [o _ k v]
  (av_opt_set o (dt-ffi/string->c k) (dt-ffi/string->c v) AV_OPT_SEARCH_CHILDREN))
#_(defmethod set-option :avoption-type/const
  [o _ k v])
(defmethod set-option :avoption-type/sample-fmt
  [o _ k v]
  (let [kw->sample-format
        (into {}
              (map (fn [[k v]]
                     [v k]))
              sample-format->kw)
        fmt (or
             (get kw->sample-format v)
             v)]
    (when-not (contains? sample-format->kw fmt)
      (throw (ex-info "Invalid sample format."
                      {:o o
                       :k k
                       :v v})))
    (av_opt_set_sample_fmt o (dt-ffi/string->c k) fmt AV_OPT_SEARCH_CHILDREN)))
(defmethod set-option :avoption-type/pixel-fmt
  [o _ k v]
  (let [kw->pixel-format
        (into {}
              (map (fn [[k v]]
                     [v k]))
              pixel-format->kw)
        pix-fmt (or
                 (get kw->pixel-format v)
                 v)]
    (when-not (contains? pixel-format->kw pix-fmt)
      (throw (ex-info "Invalid pixel format."
                      {:o o
                       :k k
                       :v v})))
    (av_opt_set_pixel_fmt o (dt-ffi/string->c k) pix-fmt AV_OPT_SEARCH_CHILDREN)))
#_(defmethod set-option :avoption-type/binary
  [o _ k v])
(defmethod set-option :avoption-type/color
  [o _ k v]
  (assert (string? v) "Colors must be string.")
  (av_opt_set o (dt-ffi/string->c k) (dt-ffi/string->c v) AV_OPT_SEARCH_CHILDREN))
(defmethod set-option :avoption-type/bool
  [o _ k v]
  (av_opt_set_int o (dt-ffi/string->c k)
                  (case v
                    (true 1) 1
                    ;; else
                    0)
                  AV_OPT_SEARCH_CHILDREN))

(defn list-filters []
  (let [iter-data (dt-ffi/make-ptr :pointer 0)
        #_(PointerByReference. Pointer/NULL)]
    (loop [flts []]
      (let [flt (av_filter_iterate iter-data)]
        (if flt
          (recur (conj flts (d/datafy flt)))
          flts)))))

(comment
  (list-filters)
  ,)

;; (defmulti set-option
;;   (fn [obj class-name k v]
;;     [class-name k]))

(defn set-filter-context-options [filter-context filter-name opts]
  (doseq [[k v] opts]
    (set-option filter-context [filter-name k] k v)))

(defn supported-filter-option? [option]
  (get-method set-option (:type option)))

(defn option-setter-fns [class-name options]
  (let [consts (->> options
                    (filter #(= (:type %)
                                :avoption-type/const))
                    (group-by :unit))]
    `(do
       ~@(eduction
          (filter supported-filter-option?)
          (map (fn [option]
                 (let [s (:name option)
                       unit (:unit option)
                       k (str->kw s)
                       v## (gensym "v")]
                   `(defmethod set-option [~class-name ~k]
                      [obj# _class-name# _k# ~v##]
                      (let [~v## ~(if-let [const-options (get consts unit)]
                                    ;; assumes int type
                                    ;; ignore :avoption-type/flags and :avoption-type/const
                                    (let [m (into {"none" 0}
                                                  (map (fn [opt]
                                                         [(-> opt
                                                              :name)
                                                          (-> opt
                                                              :default-val
                                                              :int)]))
                                                  const-options)]
                                      `(get ~m ~v## ~v##))
                                    v##)]
                        (set-option obj# ~(:type option) ~s ~v##))))))
          options)))
  )

(defn filter-setters [filter-info]
  (let [filter-name (:name filter-info)
        options (:options filter-info)]
    ;; maybe should namespace in the future?
    (option-setter-fns filter-name options)))

(defmacro make-filter-setters []
  `(do
     ~@(mapv filter-setters (list-filters))))

(make-filter-setters)

(defmacro make-swscale-setters []
  (option-setter-fns "swscale" (:options (d/datafy (sws_get_class)))))

(make-swscale-setters)
