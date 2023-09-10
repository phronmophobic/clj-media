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
  "All possible sample formats."
  (disj
   (into #{}
         (keys datafy-media/kw->sample-format))
   :sample-format/none))

(def pixel-formats
  "All possibel pixel formats."
  (disj
   (into #{}
         (keys datafy-media/kw->pixel-format))
   :pixel-format/none))

(defn list-codecs
  "Returns a collection of available codecs."
  []
  (av/list-codecs))

(defn audio-format
  "Convenience function for returning an audio format."
  [{:keys [channel-layout
           sample-format
           sample-rate]
    :as format}]
  (when (not sample-format)
    (throw (ex-info "Cannot create audio format without specifying sample format."
                    {})))
  (when (not (contains? sample-formats sample-format))
    (throw (ex-info "Invalid sample format."
                    {:format format})))

  {:channel-layout channel-layout
   :sample-format sample-format
   :sample-rate sample-rate})

(defn video-format
  "Convenience function for returning an audio format."
  [{:keys [pixel-format]
    :as format}]
  (when (not pixel-format)
    (throw (ex-info "Cannot create video format without specifying pix format."
                    {})))
  (when (not (contains? pixel-formats pixel-format))
    (throw (ex-info "Invalid pixel format."
                    {:format format})))

  {:pixel-format pixel-format})

(defn write!
  "Write media to the path given by `dest`.

  opts can have the following options:

  `:audio-format`
  A map with the following keys:
  `:sample-rate`: See `:sample-rates` found in `list-codecs`.
  `:sample-format`: See `:sample-formats` found in `list-codecs`.
  `:channel-layout`: See `:name` of `:channel-layouts` found in `list-codecs`.
                \"stereo\" and \"mono\" are typical values.
  `:codec` {:id codec-id}

  `:video-format`
  A map with the following keys.
  `:pixel-format`: See `:pixel-formats` found in `list-codecs`.
  `:gop-size`: [optional] the number of pictures in a group of pictures, or 0 for intra_only.
  `:codec`: A codec in the form, `{:id codec-id}`. See `list-codecs`.
  "
  ([media dest]
   (write! media dest nil))
  ([media dest opts]
   (fm/write! media dest opts)))

(defn filter-video
  "Returns media with only video streams from `media`."
  [media]
  (fm/filter-video media))

(defn remove-video
  "Returns media without video streams from `media`."
  [media]
  (fm/filter-audio media))

(defn filter-audio
  "Returns media with only audio streams from `media`."
  [media]
  (fm/filter-audio media))

(defn remove-audio
  "Returns media without audio streams from `media`."
  [media]
  (fm/filter-video media))

(defn union
  "Returns media that combines all the streams from `medias`."
  [& medias]
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

(comment
  (av/probe "my-video.mp4")
  (probe "my-video.mp4")
  (av/probe3  "my-video.mp4")

  
  ,)
