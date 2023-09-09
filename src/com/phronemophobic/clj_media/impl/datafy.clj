(ns com.phronemophobic.clj-media.impl.datafy
  (:require [clojure.string :as str]
            [clojure.datafy :as d]
            [clojure.core.protocols :as p]
            [com.phronemophobic.clj-media.impl.util
             :refer [normalize-str]]
            [com.phronemophobic.clj-media.impl.raw :as raw
             :refer :all])
  (:import
   java.nio.ByteOrder
   java.nio.ByteBuffer
   com.sun.jna.Memory
   com.sun.jna.Structure
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference))

(raw/import-structs!)

(defn ch-layout->str [ch-layout]
  (let [buf (Memory. 512)
        err (av_channel_layout_describe (.getPointer ch-layout) buf (.size buf))]
    (when (neg? err)
      (throw (ex-info "Could not encode ch-layout."
                      {:ch-layout ch-layout
                       :err err})))
    (String. (.getByteArray buf 0 err) "ascii")))

(defn str->ch-layout [s]
  (assert s "Invalid ch-layout.")
  (let [ch-layout (AVChannelLayoutByReference.)
        err (av_channel_layout_from_string ch-layout s)]
    (when (neg? err)
      (throw (ex-info "Invalid channel layout."
                      {:channel-layout-str s})))
    ch-layout))

(defn pointer-seq [p size terminal]
  (loop [offset 0
         results []]
    (let [x (case size
              4 (.getInt p offset)
              8 (.getLong p offset))]
      (if (= x terminal)
        results
        (recur (+ offset size)
               (conj results x))))))

(def ^:private avrational-size (.size (AVRational.)))
(defn avrational-seq [p]
  (loop [p p
         results []]
    (let [ratio (Structure/newInstance AVRationalByReference p)]
      (if (and (zero? (:num ratio))
               (zero? (:den ratio)))
        results
        (recur (.share p avrational-size)
               (conj results ratio)))))
  )

(def ^:private avchannellayout-size (.size (AVChannelLayout.)))
(defn avchannellayout-seq [p]
  (loop [p p
         results []]
    (let [bs (.getByteArray p 0 avchannellayout-size)]
      (if (every? zero? bs)
        results
        (let [layout (Structure/newInstance AVChannelLayoutByReference p)]
          (recur (.share p avchannellayout-size)
                 (conj results layout)))))))

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
  (let [_ (assert (= 8 (alength bs)))
        bs (if (= (ByteOrder/nativeOrder)
                  ByteOrder/LITTLE_ENDIAN)
             (byte-array (reverse bs))
             bs)
        val (BigInteger. bs)]
    val))

(defmethod read-bytes :avoption-type/int
  [_ bs]
  (-> (ByteBuffer/wrap bs)
      (.order (ByteOrder/nativeOrder))
      (.getInt)))

(defmethod read-bytes :avoption-type/bool
  [_ bs]
  (let [num (read-bytes :avoption-type/uint64 bs)]
    (not (zero? num))))

(defmethod read-bytes :avoption-type/uint64
  [_ bs]
  (let [_ (assert (= 8 (alength bs)))
        bs (if (= (ByteOrder/nativeOrder)
                  ByteOrder/LITTLE_ENDIAN)
             (byte-array (reverse bs))
             bs)
        val (BigInteger. 1 bs)]
    val))

(defmethod read-bytes :avoption-type/double
  [_ bs]
  (-> (ByteBuffer/wrap bs)
      (.order (ByteOrder/nativeOrder))
      (.getDouble)))

(defmethod read-bytes :avoption-type/float
  [_ bs]
  (-> (ByteBuffer/wrap bs)
      (.order (ByteOrder/nativeOrder))
      (.getFloat)))

(defmethod read-bytes :avoption-type/string
  [_ bs]
  (let [ptr-native (read-bytes :avoption-type/int64 bs )]
    (when (not (zero? ptr-native))
      (let [p (Pointer.  ptr-native)]
        (.getString p 0 "ascii")))))

(defmethod read-bytes :default
  [type bs]
  {:type type
   :bs bs
   :float (read-bytes :avoption-type/float bs)
   :double (read-bytes :avoption-type/double bs)
   :long (read-bytes :avoption-type/int64 bs)
   :int (read-bytes :avoption-type/int bs)})


(defn filter-options [flt]
  (let [cls (:priv_class flt)]
    (when cls
      (let [cls* (PointerByReference. (.getPointer cls))]
        (loop [prev nil
               opts []]
          (let [o (av_opt_next cls* prev)]
            (if o
              (recur o (conj opts (d/datafy o)))
              opts)))))))

(extend-protocol p/Datafiable
  AVFilterByReference
  (datafy [flt]
    (merge
     {:name (.getString (.getPointer (:name flt)) 0 "ascii")
      :options (filter-options flt)}
     (when-let [description (.getPointer (:description flt))]
       {:description (.getString description 0 "ascii")})
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
                          :name name})))
                (range size))})))))

(extend-protocol p/Datafiable
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
         {:default-val (read-bytes option-type default)})
       (when-let [min (:min opt)]
         {:min min})
       (when-let [max (:max opt)]
         {:max max})
       (when-let [unit (:unit opt)]
         {:unit (.getString (.getPointer unit) 0 "ascii")})))))


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
             _ (assert (= 8 (alength bs)))
             bs (if (= (ByteOrder/nativeOrder)
                       ByteOrder/LITTLE_ENDIAN)
                  (byte-array (reverse bs))
                  bs)
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
   {:name (.getString (:name codec) 0 "ascii")
    :long-name (.getString (:long_name codec) 0 "ascii")
    :media-type (media-type->kw (:type codec))
    :id (:id codec)}
   (when-let [supported-framerates (:supported_framerates codec)]
     {:supported-framerates
      (into []
            (map (fn [ratio]
                   (/ (:num ratio)
                      (:den ratio))))
            (avrational-seq supported-framerates))})
   (when-let [pix-fmts (:pix_fmts codec)]
     {:pixel-formats
      (into []
            (map pixel-format->kw)
            (pointer-seq pix-fmts
                              4 -1))})
   (when-let [sample-rates (:supported_samplerates codec)]
     {:sample-rates
      (into []
            (pointer-seq sample-rates 4 0))})
   (when-let [sample-fmts (:sample_fmts codec)]
     {:sample-formats
      (into []
            (map sample-format->kw)
            (pointer-seq sample-fmts
                              4 -1))})
   (when-let [channel-layouts (:ch_layouts codec)]
     {:channel-layouts
      (into []
            (map avchannellayout->map)
            (avchannellayout-seq channel-layouts))})))

(extend-protocol p/Datafiable
  AVRational
  (datafy [ratio]
    [(:num ratio)
     (:den ratio)])
  AVRationalByReference
  (datafy [ratio]
    [(:num ratio)
     (:den ratio)]))

(extend-protocol p/Datafiable
  AVCodecByReference
  (datafy [codec]
    (codec->map codec)))

(extend-protocol p/Datafiable
  AVChannelLayout
  (datafy [codec]
    (avchannellayout->map codec))

  AVChannelLayoutByReference
  (datafy [codec]
    (avchannellayout->map codec)))

(extend-protocol p/Datafiable
  AVCodecParametersByReference
  (datafy [params]
    (let [media-type
          (condp = (:codec_type params)
            AVMEDIA_TYPE_VIDEO :media-type/video
            AVMEDIA_TYPE_AUDIO :media-type/audio)
          codec (avcodec_find_decoder (:codec_id params))]
      (merge
       {:media-type media-type
        :codec (d/datafy codec)
        :bit-rate (:bit_rate params)
        :bits-per-coded-sample (:bits_per_coded_sample params)
        :bits-per-raw-sample (:bits_per_raw_sample params)}
       (case media-type
         :media-type/audio
         {:ch-layout (d/datafy (:ch_layout params))
          :sample-rate (:sample_rate params)
          :frame-size (:frame_size params)
          :sample-format (sample-format->kw (:format params))}
         :media-type/video
         {:width (:width params)
          :height (:height params)
          :video-delay (:video_delay params)
          :pixel-format (pixel-format->kw (:format params))})))))


(defn map->format
  ([format media-type]
   (map->format (assoc format
                       :media-type media-type)))
  ([format]
   (case (:media-type format)
     :media-type/audio
     (assoc format
            :ch-layout
            (doto (str->ch-layout
              (:ch-layout format))
              .read)
            :sample-format (int (kw->sample-format
                                 (:sample-format format)))
            :sample-rate (int (:sample-rate format)))

     :media-type/video
     (assoc format
            :pixel-format (int (kw->pixel-format (:pixel-format format)))))))
