(ns com.phronemophobic.clj-media
  (:require
   [com.phronemophobic.clj-media.impl.filter.media
    :as fm]
   [com.phronemophobic.clj-media.avfilter
    :as avfilter]
   [clojure.string :as str]
   [clojure.java.io :as io]


   [com.phronemophobic.clj-media.impl.av :as av]
   ;; [com.phronemophobic.clj-media.audio :as audio]
   ;; [com.phronemophobic.clj-media.video :as video]
   
))



(defn write! [media dest]
  (fm/write! media dest))

(defn filter-video [media]
  (fm/filter-video media))

(defn remove-video [media]
  (fm/filter-audio media))

(defn filter-audio [media]
  (fm/filter-audio media))

(defn remove-audio [media]
  (fm/filter-video media))

(defn has-video? [media])

(defn has-audio? [media])

(defn silent-audio
  "Returns silent audio media in format."
  [format])

(defn java-graphics-media
  
  [f ])

(defn byte-buffer-media
  "Returns media with format and frames."
  [format frames]
  )

(defn union [& medias]
  (apply fm/union medias))

(defn file
  "Returns media with the contents of the file `f`.

  `f` should be a value that can be coerced to a file via
      clojure.java.io/file."
  [f]
  (fm/->MediaFile
   (.getCanonicalPath (io/as-file f))))



(defn probe
  "Probes the file `f` and returns metadata about the media's format.

  `f` Any value that can be coerced to a file via `clojure.java.io/as-file`."
  [f]
  (av/probe f))


(comment
  (av/probe "my-video.mp4")
  (probe "my-video.mp4")
  (av/probe3  "my-video.mp4")

  
  ,)
