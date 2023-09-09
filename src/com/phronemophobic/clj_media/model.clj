(ns com.phronemophobic.clj-media.model)

(defprotocol IFrameData
  (byte-buffer [this] [this plane-index]
    "Returns a java.nio.ByteBuffer mapped to the raw data of this frame.
     Only valid for video and non planar audio"))

(defprotocol IFrame
  (media-type [this]
    "Returns the media type. One of `:media-type/audio`, `:media-type/video`.")
  (media-format [this]
    "Returns the media format as a keyword.")
  (presentation-time [this]
    "Returns the presentation time of the frame in seconds.")
  (pts [this]
    "Returns the presentation timestamp in `time-base` units.")
  (time-base [this]
    "Returns the time base as a two element collection of [numerator, denominator]."))

(defprotocol IVideoFrame
  (image [this]
    "Returns a java.awt.image.BufferedImage for the contents of this frame."))

