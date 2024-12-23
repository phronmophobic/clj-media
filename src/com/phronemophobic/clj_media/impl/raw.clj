(ns com.phronemophobic.clj-media.impl.raw
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :as edn]
            [com.phronemophobic.clong.gen.jna :as gen]
            [com.rpl.specter :as specter])
  (:import
   java.io.PushbackReader)
  (:gen-class))

(def AV_OPT_SEARCH_CHILDREN 1)
(def averror-eagains #{-11 -35})
(def AVERROR_EOF -541478725)
(def AVERROR_INVALIDDATA -1094995529)
(def AVIO_FLAG_READ  1)
(def AVIO_FLAG_WRITE 2)
(def AVIO_FLAG_READ_WRITE 3)
(def AVFMT_GLOBALHEADER  0x0040)
(def AV_CODEC_FLAG_GLOBAL_HEADER (bit-shift-left 1 22))
(def AV_CODEC_CAP_VARIABLE_FRAME_SIZE (int (bit-shift-left 1 16)) )

(def FF_COMPLIANCE_VERY_STRICT   2) ;; ///< Strictly conform to an older more strict version of the spec or reference software.
(def FF_COMPLIANCE_STRICT        1) ;; ///< Strictly conform to all the things in the spec no matter what consequences.
(def FF_COMPLIANCE_NORMAL        0) ;;
(def FF_COMPLIANCE_UNOFFICIAL   -1) ;; ///< Allow unofficial extensions
(def FF_COMPLIANCE_EXPERIMENTAL -2) ;; ///< Allow nonstandardized experimental things.


(def ^:private lib
  (com.sun.jna.NativeLibrary/getProcess))

(defmacro ^:private if-loaded [cls then else]
  (let [loaded?
        (try
          (Class/forName (name cls))
          true
          (catch ClassNotFoundException e
            false))]
    (if loaded?
      then
      else)))

(defn get-loaded-libraries []
  (if-loaded org.bytedeco.javacpp.Loader
    (org.bytedeco.javacpp.Loader/getLoadedLibraries)
    {}))

;; load all libraries
;; need to initialize class instances because native libraries
;;   are loaded in static initializers
(defn ^:private try-load [classname]
  (when-let [cls (try
                   (Class/forName (name classname))
                   (catch ClassNotFoundException e
                     nil))]
    (when-let [constructor (Class/.getConstructor cls
                                                  (into-array Class []))]
      (.newInstance constructor (to-array [])))))

(run! try-load
      '[org.bytedeco.ffmpeg.global.avcodec
        org.bytedeco.ffmpeg.global.avdevice
        org.bytedeco.ffmpeg.global.avfilter
        org.bytedeco.ffmpeg.global.avformat
        org.bytedeco.ffmpeg.global.avutil
        org.bytedeco.ffmpeg.global.swresample
        org.bytedeco.ffmpeg.global.swscale])

;; list loaded libraries
;; (keys (Loader/getLoadedLibraries))


(def ^:private lib-versions
  {"avutil" "avutil@.58"
   "avfilter" "avfilter@.9"
   "avformat" "avformat@.60"
   "avdevice" "avdevice@.60"
   "swresample" "swresample@.4"
   "swscale" "swscale@.7"
   "avcodec" "avcodec@.60"})

(def ^:private lib-names ["avutil"
                          "avfilter"
                          "avformat"
                          "avdevice"
                          "swresample"
                          "swscale"
                          "avcodec"])

(defonce ^:private libs (atom #{}))
(defn ^:private load-libs
  ([]
   (load-libs lib-versions))
  ([lib-versions]
   (let [new-libs
         (into []
               (map (fn [[lib-name lib-version]]
                      (if-let [lib-path (get (get-loaded-libraries) lib-version)]
                        (com.sun.jna.NativeLibrary/getInstance lib-path)
                        (com.sun.jna.NativeLibrary/getInstance lib-name))))
               lib-versions)]
     
     (swap! libs into new-libs))))
(load-libs)

(defn ^:private  parse-av-api []
  (let [default-arguments
        @(requiring-resolve 'com.phronemophobic.clong.clang/default-arguments)

        header-files (->> lib-names
                          (map #(->> (str "lib" % "/" % ".h")
                                     (io/file "../FFmpeg/" )
                                     .getCanonicalPath)))
        header-files (into header-files
                           (map #(->> %
                                      (io/file "../FFmpeg/")
                                      .getCanonicalPath))
                           ["libavfilter/buffersink.h"
                            "libavfilter/buffersrc.h"])

        clang-args (conj default-arguments
                         (->> (io/file "../FFmpeg")
                              (.getCanonicalPath)
                              (str "-I"))
                         (first header-files))
        clang-args (into clang-args
                         (mapcat (fn [h]
                              ["-include" h]))
                         (rest header-files))
        clang-args (into clang-args
                         (comp (map #(-> % io/file .getParent))
                               (distinct)
                               (map (fn [folder]
                                      (str "-I" folder))))
                         header-files)]
    ((requiring-resolve 'com.phronemophobic.clong.clang/easy-api) nil
       clang-args)))

(defn filter-structs [api]
  (update api :structs
          #(filterv (fn [{:keys [id] :as struct}]
                      (let [id-str (name id)]
                        (or (str/starts-with? id-str "AV")
                            (str/starts-with? id-str "Sw")
                            (= "RcOverride" id-str))))
                    %)))
(defn filter-fns [api]
  (update api :functions
          #(filterv (fn [{:keys [id] :as struct}]
                      (let [id-str (name id)]
                        (or (str/starts-with? id-str "av")
                            (str/starts-with? id-str "sw"))))
                    %)))

(defn ^:private dump-api []
  (let [outf (io/file
              "resources"
              "com"
              "phronemophobic"
              "clj-media"
              "api.edn")
        api (parse-av-api)]
    (.mkdirs (.getParentFile outf))
    (with-open [w (io/writer outf)]
      (let [api (-> api
                    filter-structs
                    filter-fns)]
        ((requiring-resolve 'com.phronemophobic.clong.clang/write-edn) w api)))))

(defn pointer? [datatype]
  (and (vector? datatype)
       (= :coffi.mem/pointer
          (first datatype))))

;; AVCodec

(def ^:private oformat-path
  [:structs
   specter/ALL
   #(#{:clong/AVFormatContext
       :clong/AVCodec
       :clong/AVCodecContext} (:id %))
   :fields
   specter/ALL
   :datatype
   pointer?
   ])




;; JNA has trouble `.writeField`ing
;; to StructureByReference fields
(defn ^:private with-hacks [api]
  (->> api
       (specter/setval oformat-path
                       :coffi.mem/pointer)
       (specter/setval [:functions
                        specter/ALL
                        #(or (str/starts-with? (:symbol %) "vk")
                             (str/starts-with? (:symbol %) "av_vk")
                             (str/includes? (:symbol %) "vdpau"))]
                       specter/NONE)))

(def av-api
  (with-hacks
    (with-open [rdr (io/reader
                     (io/resource
                      "com/phronemophobic/clj-media/api.edn"))
                pbr (PushbackReader. rdr)]
      (edn/read pbr))))

(defonce ^:private generated-api (gen/def-api lib av-api))
(defmacro import-structs! []
  `(gen/import-structs! av-api "com.phronemophobic.clj_media.impl.raw.structs"))
