(ns com.phronemophobic.clj-media.avfilter
  (:refer-clojure :exclude [format
                            identity
                            interleave
                            loop
                            reverse
                            concat])
    (:require
     [com.phronemophobic.clj-media.impl.filter.media
             :as fm]
     [com.phronemophobic.clj-media.impl.filter.avfilter
      :as avfilter]))

(avfilter/make-fns)


(defn hstack
"hstack: Stack video inputs horizontally.

Supported options:

:shortest - force termination when the shortest input terminates
  type: bool
  default: false"
  ([opts-or-input & inputs]

   (let [[opts inputs]
         (if (fm/media-source? opts-or-input)
           [nil (cons opts-or-input inputs)]
           [opts-or-input inputs])

         opts (assoc opts :inputs (count inputs))]
     (avfilter/->AVMultiFilterMedia "hstack" opts :media-type/video inputs))))

(defn vstack
  "vstack: Stack video inputs vertically.

Supported options:

:shortest - force termination when the shortest input terminates
  type: bool
  default: false"
  ([opts-or-input & inputs]

   (let [[opts inputs]
         (if (fm/media-source? opts-or-input)
           [nil (cons opts-or-input inputs)]
           [opts-or-input inputs])

         opts (assoc opts :inputs (count inputs))]
     (avfilter/->AVMultiFilterMedia "vstack" opts :media-type/video inputs))))

(defn concat
  "concat: Concatenate audio and video streams. Medias must have the same
           number of streams with matching media types. All video streams
           must be the same width and height, but media formats may differ.

Inputs: 

Supported options:

:unsafe - enable unsafe mode
	type: bool
	default: false
	values: true, false
"
  ([opts-or-media & medias]
   (let [[opts medias]
         (if (fm/media-source? opts-or-media)
           [nil (cons opts-or-media medias)]
           [opts-or-media medias])]
     (avfilter/->AVConcatFilterMedia opts medias))))


(defn trim+
"trim: Pick one continuous section from the input, drop the rest.

Supported options:

:duration - Maximum duration of the output
	type: duration in microseconds
	min: 0.0
	max: 9.223372036854776E18

:durationi - Maximum duration of the output
	type: duration in microseconds
	min: 0.0
	max: 9.223372036854776E18

:end - Timestamp of the first frame that should be dropped again
	type: duration in microseconds
	min: -9.223372036854776E18
	max: 9.223372036854776E18

:end-pts - Timestamp of the first frame that should be dropped again
	type: int64
	default: -9223372036854775808
	min: -9.223372036854776E18
	max: 9.223372036854776E18

:end-sample - Number of the first audio sample that should be dropped again
	type: int64
	default: 9223372036854775807
	min: 0.0
	max: 9.223372036854776E18

:endi - Timestamp of the first frame that should be dropped again
	type: duration in microseconds
	min: -9.223372036854776E18
	max: 9.223372036854776E18

:start - Timestamp of the first frame that should be passed
	type: duration in microseconds
	min: -9.223372036854776E18
	max: 9.223372036854776E18

:start-pts - Timestamp of the first frame that should be  passed
	type: int64
	default: -9223372036854775808
	min: -9.223372036854776E18
	max: 9.223372036854776E18

:start-sample - Number of the first audio sample that should be passed to the output
	type: int64
	default: -1
	min: -1.0
	max: 9.223372036854776E18

:starti - Timestamp of the first frame that should be passed
	type: duration in microseconds
	min: -9.223372036854776E18
	max: 9.223372036854776E18
"
  [opts media]
  (->> media
       (trim opts)
       (atrim opts)
       (setpts {:expr "PTS-STARTPTS"})
       (asetpts {:expr "PTS-STARTPTS"})))
