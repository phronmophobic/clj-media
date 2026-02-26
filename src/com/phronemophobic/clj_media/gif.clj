(ns com.phronemophobic.clj-media.gif
  (:require [com.phronemophobic.clj-media :as clj-media]
            [com.phronemophobic.clj-media.avfilter :as avfilter]))

(defn save-gif!
  "Write a gif to `fname` with contents of `media`.

  `media` can be any valid media.

  Available options:
  `:dither` A dithering algorithm. One of  \"bayer\", \"heckbert\", \"floyd_steinberg\", \"sierra2\", \"sierra2_4a\", \"sierra3\", \"burkes\", \"atkinson\", \"none\". See avfilter/paletteuse for more info. default \"sierra2_4a\".
  `:transparent?` Allow transparency. default: true.
  `:max-colors` Set the maximum number of colors to use in the palette. default 256.
  `:alpha-threshold` Alpha cutoff for transparency. default 128."
  ([media fname]
   (save-gif! media fname {}))
  ([media fname {:keys [dither
                        transparent?
                        max-colors
                        alpha-threshold] :as opts}]
   (let [;; loop? (get opts :loop? true)
         alpha-threshold (or alpha-threshold
                             128)
         transparent? (get opts :transparent? true)
         
         palette (avfilter/palettegen
                  (merge
                   {:reserve-transparent transparent?}
                   (when max-colors
                     {:max-colors max-colors}))
                  media)

         media (avfilter/paletteuse
                (merge
                 (when alpha-threshold
                   {:alpha-threshold alpha-threshold})
                 (when dither
                   {:dither dither}))
                media palette)]
     (clj-media/write!
      media
      fname
      {:video-format
       {:codec {:name "gif",
                :id 97,}
        :pixel-format :pixel-format/pal8}}))))



