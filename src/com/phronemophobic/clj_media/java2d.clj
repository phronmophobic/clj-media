(ns com.phronemophobic.clj-media.java2d
  (:require [com.phronemophobic.clj-media :as clj-media])
  (:import java.awt.image.BufferedImage
           java.awt.Graphics2D
           java.awt.Color
           java.awt.RenderingHints
           javax.imageio.ImageIO))

(defn ^:private new-img
  "Returns a new BufferedImage in a format suitable for creating a gif"
  [width height]
  (BufferedImage. width height BufferedImage/TYPE_4BYTE_ABGR))

(defn graphics->media
  "Creates a `media` by calling `drawf` with a Graphics2D and each element of `coll`. `drawf` should draw the current frame. The return value is ignored.

  The following options are available:
  `:fps` frames per second. default: 24.
  `:width` width of the media. default: 100.
  `:height` height of the media. default 100.

  Example:

  (graphics->media
   (fn [^Graphics2D g frameno]
     (.setColor g Color/white)
     (.fillRect g 0 0 100 100)
     (.setColor g Color/black)
     (.drawString g \"Hello World\" 5 50))
  (range 24))"
  ([drawf coll]
   (graphics->media drawf {} coll))
  ([drawf
    {:keys [fps
            width
            height]
     :as opts}
    coll]
   (let [width (or width 100)
         height (or height 100)
         fps (or fps 24)
         img ^BufferedImage (new-img width height)
         frame-format
         (clj-media/video-format
          {:pixel-format :pixel-format/abgr
           :time-base fps
           :line-size (* width
                         ;; abgr has 4 bytes per pixel when packed
                         4)
           :width width
           :height height})

         g (.createGraphics img)
         frames
         (sequence
          (map-indexed
           (fn [pts x]
             (.setBackground g (Color. (int 255)
                                       (int 255)
                                       (int 255)
                                       (int 0)))
             (.clearRect g 0 0
                         width height)
             (.setColor ^Graphics2D g (Color/BLACK))
             (drawf g x)
             (clj-media/make-frame
              {:bytes (-> img
                          (.getData)
                          ^java.awt.image.DataBufferByte
                          (.getDataBuffer)
                          (.getData))
               :format frame-format
               :time-base (:time-base frame-format)
               :pts pts})))
          coll)]
     (clj-media/make-media frame-format
                           frames))))



