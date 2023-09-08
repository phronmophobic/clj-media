(ns com.phronemophobic.clj-media
  (:require
   [com.phronemophobic.clj-media.impl.filter.media
    :as fm]
   [com.phronemophobic.clj-media.impl.datafy
    :as datafy-media]
   [com.phronemophobic.clj-media.avfilter
    :as avfilter]
   [com.phronemophobic.clj-media.impl.video :as video]
   [com.phronemophobic.clj-media.impl.audio :as audio]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [com.phronemophobic.clj-media.impl.raw :as raw]
   [com.phronemophobic.clj-media.impl.av :as av]))


(def sample-formats
  (disj
   (into #{}
         (keys datafy-media/kw->sample-format))
   :sample-format/none))

(def pixel-formats
  (disj
   (into #{}
         (keys datafy-media/kw->pixel-format))
   :pixel-format/none))

(defn audio-format [{:keys [ch-layout
                            sample-format
                            sample-rate]
                     :as format}]
  (when (not sample-format)
    (throw (ex-info "Cannot create audio format without specifying sample format."
                    {})))
  (when (not (contains? sample-formats sample-format))
    (throw (ex-info "Invalid sample format."
                    {:format format})))

  (merge
   {:ch-layout "stereo"
    :sample-format sample-format
    :sample-rate sample-rate}))

(defn video-format [{:keys [pix-fmt]
                     :as format}]
  (when (not pix-fmt)
    (throw (ex-info "Cannot create video format without specifying pix format."
                    {})))
  (when (not (contains? pixel-formats pix-fmt))
    (throw (ex-info "Invalid pixel format."
                    {:format format})))

  {:pix-fmt pix-fmt})



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


(defn frames
  "Returns a reducible of frames of media.

  Stream can either be:
  - an index
  - `:audio`, which selects the first audio stream.
  - `:video`, which seelcts the first video stream."
  ([media]
   (fm/frames-reducible media 0 nil))
  ([media stream]
   (fm/frames-reducible media stream nil))
  ([media stream opts]
   (fm/frames-reducible media stream opts)))

(defn play-audio [media]
  (transduce
   (audio/frame->buf raw/AV_SAMPLE_FMT_S16)
   (audio/play-sound)
   0
   (frames media :audio {:audio-format (audio-format
                                        {:ch-layout "stereo"
                                         :sample-rate 44100
                                         :sample-format :sample-format/s16})})))

(defn frame->img [frame]
  (video/frame->img frame))

(comment
  (av/probe "my-video.mp4")
  (probe "my-video.mp4")
  (av/probe3  "my-video.mp4")

  
  ,)
