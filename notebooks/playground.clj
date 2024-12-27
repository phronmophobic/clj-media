(ns playground
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.java.io :as io]
            [com.phronemophobic.clj-media :as clj-media]
            [com.phronemophobic.clj-media.model :as mm]

            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.raw :as raw]
            [com.phronemophobic.clj-media.impl.filter.media :as fm]
            [com.phronemophobic.clj-media.avfilter :as avfilter]))


(comment
  (import 'java.awt.image.BufferedImage
          'java.awt.Graphics2D
          'java.awt.Color
          'java.awt.RenderingHints
          'javax.imageio.ImageIO)

  (require '[membrane.java2d :as java2d]
           '[membrane.ui :as ui])
  ,)




