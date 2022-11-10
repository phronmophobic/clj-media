(ns com.phronemophobic.clj-media.av.raw
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [com.phronemophobic.clong.clang :as clang]
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

(def ^:private lib
  (com.sun.jna.NativeLibrary/getProcess))



(def ^:private lib-names ["avutil"
                          "avresample"
                          "avfilter"
                          "avformat"
                          "avdevice"
                          "swresample"
                          "swscale"
                          "avcodec"])
(def ^:private header-files
  (into []
        (comp (map #(io/file "/opt/local/include" (str "lib" %)))
              (mapcat #(.listFiles %))
              (map #(.getAbsolutePath %))
              (filter #(str/ends-with? % ".h")))
        lib-names))

(defonce ^:private libs (atom #{}))
(defn ^:private load-libs
  ([]
   (load-libs lib-names))
  ([lib-names]
   (let [new-libs
         (into []
               (map #(com.sun.jna.NativeLibrary/getInstance %))
               lib-names)]
     
     (swap! libs into new-libs))))
(load-libs)



(defn ^:private  parse-av-api []
  (clang/easy-api nil ;; (first header-files)
                  (into (conj clang/default-arguments
                              (first header-files))
                        (mapcat (fn [h]
                                  ["-include" h]))
                        (rest header-files))))

(defn ^:private dump-av-api []
  (let [api (parse-av-api)]
    (with-open [w (io/writer (io/file
                              "resources"
                              "av-api.edn"))]
      (clang/write-edn w api))))

(def ^:private oformat-path [:structs
                   specter/ALL
                   #(= :clong/AVFormatContext (:id %))
                   :fields
                   specter/ALL
                   #(#{"oformat" "pb"}
                       (:name %))
                   :datatype])

;; JNA has trouble `.writeField`ing
;; to StructureByReference fields
(defn ^:private with-hacks [api]
  (specter/setval oformat-path
                  :coffi.mem/pointer
                  api))

(def av-api
  (with-hacks
    (with-open [rdr (io/reader (io/file "resources"
                                        "av-api.edn"))
                pbr (PushbackReader. rdr)]
      (edn/read pbr))))

(defonce ^:private generated-api (gen/def-api lib av-api))
(defmacro import-structs! []
  `(gen/import-structs! av-api "com.phronemophobic.clj_media.av.raw.structs"))
