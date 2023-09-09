(ns com.phronemophobic.clj-media.impl.model
  (:require [com.phronemophobic.clj-media.model
             :as model]
            [com.phronemophobic.clj-media.impl.datafy
             :as datafy-media]
            [com.phronemophobic.clj-media.impl.video
             :as video]
            [com.phronemophobic.clj-media.impl.audio
             :as audio]
            [clojure.datafy :as d]))

(defn ^:private presentation-time* [frame]
  (let [{:keys [num den]} (:time_base frame)
        pts (:pts frame)]
    (/ (* pts num)
       den)))

(defprotocol IRawFrame
  (raw-frame [frame]))

(deftype VideoFrame [frame]
  model/IFrame
  (media-type [this]
    :media-type/video)
  (media-format [this]
    (datafy-media/pixel-format->kw (:format frame)))
  (presentation-time [this]
    (presentation-time* frame))
  (pts [this]
    (:pts frame))
  (time-base [this]
    (d/datafy (:time_base frame)))

  model/IFrameData
  (byte-buffer [this]
    (video/frame->buf frame))

  model/IVideoFrame
  (image [this]
    (video/frame->img frame))

  IRawFrame
  (raw-frame [this]
    frame))

(deftype AudioFrame [frame]
  model/IFrame
  (media-type [this]
    :media-type/audio)
  (media-format [this]
    (datafy-media/sample-format->kw (:format frame)))
  (presentation-time [this]
    (presentation-time* frame))
  (pts [this]
    (:pts frame))
  (time-base [this]
    (d/datafy (:time_base frame)))

  model/IFrameData
  (byte-buffer [this]
    (audio/frame->buf frame))

  IRawFrame
  (raw-frame [this]
    frame))
