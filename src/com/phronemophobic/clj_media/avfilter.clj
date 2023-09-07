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

(defn concat
  ([opts-or-input & inputs]
   (let [[opts inputs]
         (if (fm/media-source? opts-or-input)
           [nil (cons opts-or-input inputs)]
           [opts-or-input inputs])]
     (avfilter/->AVConcatFilterMedia opts inputs))))


(defn trim+ [opts media]
  (->> media
       (trim opts)
       (atrim opts)
       (setpts {:expr "PTS-STARTPTS"})
       (asetpts {:expr "PTS-STARTPTS"})))
