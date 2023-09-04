(ns com.phronemophobic.clj-media.av.raw
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

(import 'org.bytedeco.javacpp.Loader)

;; load all libraries
;; need to initialize class instances because native libraries
;;   are loaded in static initializers
(do (import 'org.bytedeco.ffmpeg.global.avcodec) (avcodec.))
(do (import 'org.bytedeco.ffmpeg.global.avdevice) (avdevice.))
(do (import 'org.bytedeco.ffmpeg.global.avfilter) (avfilter.))
(do (import 'org.bytedeco.ffmpeg.global.avformat) (avformat.))
(do (import 'org.bytedeco.ffmpeg.global.avutil) (avutil.))
(do (import 'org.bytedeco.ffmpeg.global.swresample) (swresample.))
(do (import 'org.bytedeco.ffmpeg.global.swscale) (swscale.))

;; list loaded libraries
;; (keys (Loader/getLoadedLibraries))


(def ^:private lib-versions ["avutil@.58"
                             "avfilter@.9"
                             "avformat@.60"
                             "avdevice@.60"
                             "swresample@.4"
                             "swscale@.7"
                             "avcodec@.60"])
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
               (map (fn [lib-version]
                      (let [lib-path (get (Loader/getLoadedLibraries) lib-version)]
                        (com.sun.jna.NativeLibrary/getInstance lib-path))))
               lib-versions)]
     
     (swap! libs into new-libs))))
(load-libs)

(defn ^:private  parse-av-api []
  (let [default-arguments
        @(requiring-resolve 'com.phronemophobic.clong.clang/default-arguments)

        header-files (->> lib-names
                          (map #(str "lib" % "/" % ".h"))
                          (map #(io/file "../FFmpeg/" %))
                          (map #(.getCanonicalPath %)))
        header-files (into header-files
                           (comp (map #(io/file "../FFmpeg/" %))
                                 (map #(.getCanonicalPath %)))
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
                         (comp (map io/file)
                               (map #(.getParent %))
                               (distinct)
                               (map (fn [folder]
                                      (str "-I" folder))))
                         header-files)]
    ((requiring-resolve 'com.phronemophobic.clong.clang/easy-api) nil
       clang-args)))

(defn ^:private dump-av-api []
  (let [api (parse-av-api)]
    (with-open [w (io/writer (io/file
                              "resources"
                              "av-api.edn"))]
      ((requiring-resolve 'com.phronemophobic.clong.clang/write-edn) w api))))

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
    (with-open [rdr (io/reader (io/resource "av-api.edn"))
                pbr (PushbackReader. rdr)]
      (edn/read pbr))))

(defonce ^:private generated-api (gen/def-api lib av-api))
(defmacro import-structs! []
  `(gen/import-structs! av-api "com.phronemophobic.clj_media.av.raw.structs"))
