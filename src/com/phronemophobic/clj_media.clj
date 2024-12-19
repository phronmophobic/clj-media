(ns com.phronemophobic.clj-media
  (:require
   [com.phronemophobic.clj-media.impl.filter.media
    :as fm]
   [com.phronemophobic.clj-media.model :as mm]
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

(set! *warn-on-reflection* true)

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
  (merge
   format
   {:channel-layout channel-layout
    :sample-format sample-format
    :sample-rate sample-rate
    :media-type :media-type/audio}))

(defn video-format
  "Convenience function for returning an video format."
  [{:keys [pixel-format]
    :as format}]
  (when (not pixel-format)
    (throw (ex-info "Cannot create video format without specifying pix format."
                    {})))
  (when (not (contains? pixel-formats pixel-format))
    (throw (ex-info "Invalid pixel format."
                    {:format format})))

  (merge
   format
   {:pixel-format pixel-format
    :media-type :media-type/video}))

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

(defn filter-media-types
  "Returns media with only types in `media-types`."
  [media-types media]
  (fm/filter-media-types media-types media))

(defn remove-media-types
  "Returns media without any types in `media-types`."
  [media-types media]
  (fm/remove-media-types media-types media))

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
  "Reads the file `f` and returns metadata about the media's format.

  `f` Any value that can be coerced to a file via `clojure.java.io/as-file`."
  [f]
  (av/probe f))


(defn frames
  "Returns a reducible of frames of media.

  Stream can either be:
  - an index
  - `:audio`, which selects the first audio stream.
  - `:video`, which selects the first video stream.

  `opts` is an optional map with the following keys:
  `:format` A media format description as returned by
            `audio-format` or `video-format`. The
            frames will be transcoded to the provided
            format if not already in that format.
  "
  ([media]
   (fm/frames-reducible media 0 nil))
  ([media stream]
   (fm/frames-reducible media stream nil))
  ([media stream opts]
   (fm/frames-reducible media stream opts)))

(defn play-audio
  "Plays the first audio stream in `media`."
  [media]
  (transduce
   (comp
    (map mm/byte-buffer)
    (map (fn [^java.nio.ByteBuffer buf]
           (let [bytes (byte-array (.capacity buf))]
             (.get buf bytes)
             bytes))))
   (audio/play-sound)
   0
   (frames media :audio {:format (audio-format
                                  {:channel-layout "stereo"
                                   :sample-rate 44100
                                   :sample-format :sample-format/s16})}))
  nil)


(defn make-frame
  "Returns an opaque, raw frame for creating media with `make-media`.

  The following keys are required:
  `:format`: map describing the format returned by `audio-format` or `video-format`.
  `:bytes`: a byte array with the raw data adhering to `:format`.
  `:time-base`: a ratio of that `pts` will be specified in. `:time-base` typically
                matches the frame rate for video or sample rate for audio.

                `time-base` can specified using 3 different methods:
                 - as a ratio: (eg. 1/60)
                 - as a 2 element vector of [numerator, denominator]: (eg. [1 60])
                 - as an integer. It will be treated as 1/time-base: (eg. 60).
  `:pts`: the presentation timestamp in `:time-base` units.
          (time in seconds) = pts * time-base.

  The following keys are optional:
  `:key-frame?`: specifies whether the current frame should be a key frame.

"
  [{:keys [bytes
           format
           time-base
           key-frame?
           pts]
    :as m}]
  (av/make-frame m))

(defn make-media
  "Returns media using raw data found in `frames`. `frames`
  must be a sequence of frames created with `make-frame`."
  [format frames]
  (fm/media-frames format frames))

(comment
  (av/probe "my-video.mp4")
  (probe "my-video.mp4")
  (av/probe3  "my-video.mp4")

  
  ,)
