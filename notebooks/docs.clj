^{:nextjournal.clerk/visibility {:code :hide :result :hide}
  :nextjournal.clerk/toc true}
(ns docs
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.java.io :as io]
            [com.phronemophobic.clj-media :as clj-media]
            [com.phronemophobic.clj-media.avfilter :as avfilter]
            [com.phronemophobic.clj-media.model :as mm]
            [clojure.string :as str])
  (:import
   java.awt.image.BufferedImage
   java.awt.Graphics2D
   java.awt.Color
   java.awt.RenderingHints)
  )

{:nextjournal.clerk/visibility {:code :hide :result :hide}}

(def mermaid-viewer
  {:transform-fn clerk/mark-presented
   :render-fn '(fn [value]
                 (when value
                   [nextjournal.clerk.render/with-d3-require {:package ["mermaid@8.14/dist/mermaid.js"]}
                    (fn [mermaid]
                      [:div {:ref (fn [el] (when el
                                             (.render mermaid (str (gensym)) value #(set! (.-innerHTML el) %))))}])]))})

(def media-prefix
  (if-let [media-prefix (System/getProperty "docs.media.prefix")]
    media-prefix
    "http://localhost:8000/media/"))

(defmacro write! [& args]
  `(let [path# ~(let [fname (second args)
                      suffix (re-find #"\.[a-z0-9]+$" fname)]
                  (str (hash args)
                       suffix))
         f# (io/file "docs" "media" path#)
         f2# (io/file ~(second args))]
     (.mkdirs (-> f#
                  (.getParentFile)))
     (when (not (.exists f#))
       (clj-media/write! ~(first args) (.getCanonicalPath f#) ~@(nthrest args 2) )
       (clj-media/write! ~(first args) (.getCanonicalPath f2#) ~@(nthrest args 2) ))
     (clerk/col
      {::clerk/width :wide}
      (clerk/md
       (str "```clojure\n"
            ~(with-out-str
               (clojure.pprint/pprint
                (apply list 'clj-media/write args)))
            "\n```"))
      (cond
        
        (or (str/ends-with? path# ".mp4")
            (str/ends-with? path# ".mov"))
        (clerk/html
         [:video {:controls "1"
                  :preload "none"
                  :src (str media-prefix path#)}])
        
        (str/ends-with? path# ".mp3")
        (clerk/html
         [:audio {:controls "1"
                  :preload "none"
                  :src (str media-prefix path#)}])
        
        (str/ends-with? path# ".gif")
        (clerk/html
         [:img {:src (str media-prefix path#)}])
        
        :else
        path#))))


{:nextjournal.clerk/visibility {:code :hide :result :show}}
;; # clj-media

;; [clj-media](https://github.com/phronmophobic/clj-media) is a library for reading, writing, and transforming audio and video.

;; # Introduction

;; While clj-media tries to use intuitive names, it can be helpful to build an intuition for the underlying mechanics.

;; ## Definitions

;; The primary unit of data in clj-media is "media".

;; **Media** is a collection of audio or video streams.

;; A **stream** is a collection of packets.

;; A **packet** is an encoded (ie. compressed) collection of frames.

;; A **frame** is a collection of decoded (ie. uncompressed) samples.

;; **Audio samples** are typically the amplitudes of a sound's waveform "sampled" over time.

;; **Video samples** are typically pixels (eg. red, green, blue intensities).

{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def bs
  (into
   []
   (comp (take 20)
         (map (fn [frame]
                (let [buf (.asFloatBuffer
                           (mm/byte-buffer frame))
                      bs (float-array (.capacity buf))]
                  (.get buf bs)
                  bs)))
         cat)
   (clj-media/frames (clj-media/file "media/birds.mp4")
                     :audio
                     {:format (clj-media/audio-format
                               {:channel-layout "mono"
                                :sample-format :sample-format/flt
                                :sample-rate 44100})})))


(def plot-data
  {:data [{:x (map #(/ % 44100)
                   (range (count bs)))
           :y bs
           :type "scatter"}]
   :layout {:title "Sound samples"}})
{:nextjournal.clerk/visibility {:code :hide :result :show}}

(clerk/plotly
 plot-data
 )


;; There are a variety of formats for representing samples and packaging them
;; into media. clj-media provides a variety of functions for unpacking
;; and repacking media. Further, it's common to want to clip, trip,
;; crop, blur, denoise, or otherwise modify media.

;; In clj-media, **filters** are transformations of frames.

;; _Note: even though filters operate on frames. Most filter functions in clj-media accept
;; and return media for convenience._

;; Filters operate on decoded, uncompressed frames. 
;; However, most media is stored in an encoded, compressed format.
;; Therefore, in order to filter media, the media must first
;; be decoded, then filtered, then reencoded again.

;; A typical pipeline looks like:

(clerk/with-viewer mermaid-viewer
  "flowchart LR

    media[input.mp4] --> packets
packets -- decoder --> decoded[frames]
decoded -- filters --> frames
frames -- encoder --> encoded[packets]
encoded --> output.mp4

")



;; # Usage

;; ## Dependency

;; ```clojure
;; com.phronemophobic/clj-media {:mvn/version "2.1"}
;; ```

;; ### Linux dependencies

;; The `libxcb-shape0` dependency must be installed to use clj-media on linux.
;; It can usually be installed via a package manager like `apt`. For example:

;; ```bash
;; sudo apt-get install libxcb-shape0
;; ```

;; ## Requires

;; ```clojure
;; (require '[com.phronemophobic.clj-media.model :as mm])
;; (require '[com.phronemophobic.clj-media :as clj-media])
;; (require '[com.phronemophobic.clj-media.avfilter :as avfilter])
;; ```

;; Typically, media is obtained from a file
;; that was recorded using other software.

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(def my-media (clj-media/file "media/birds.mp4"))

;; The main operations on media are `write!` and `frames`.

;; ## clj-media/write!

;; The basic use of `write!` is to pass media and a destination
;; to write the output to. 

{:nextjournal.clerk/visibility {:code :hide :result :show}}
(write! my-media
        "my-copy.mov")

;; The input format does not need to match the output format and
;; clj-media will try to automatically convert formats as
;; necessary. However, `write!` will fail when trying to write
;; audio streams to video only outputs (eg. gif) or video
;; streams to audio only outputs (eg. mp3).

;; To avoid errors from mismatched media types,
;; clj-media provides simple filters that select
;; streams based on media type.

(write! (clj-media/filter-video my-media)
        "my-copy.gif")

(write! (clj-media/filter-audio my-media)
        "my-copy.mp3")

;; `write!` also accepts an optional map. The main options are
;; `:audio-format` and `:video-format` in cases where
;; choosing the exact format is desired.

;; Codecs can be found via `list-codecs`.

^{:nextjournal.clerk/visibility {:code :show :result :show}}
(clj-media/list-codecs)

;; The `list-codecs` function will return the available codecs
;; as well as the supported formats.


;; ## clj-media/frames

;; The `frames` function will return a reducible. The reason that
;; `frames` does not return a collection or lazy sequence is that
;; `frames` can consume lots of resources. Returning a reducible
;; allows clj-media to handle all of the resource management for you.

;; _Note: there are is still ongoing work on resource management
;; and memory usage. However, improvements can be made without
;; changing or breaking the higher level API._

;; ### Obtaining the first frame

^{:nextjournal.clerk/visibility {:code :show :result :show}}
(reduce (fn [_ frame] frame) nil (clj-media/frames my-media :audio))

;; The functions for examining frames can be found in the [com.phronemophobic.clj-media.model](https://phronmophobic.github.io/clj-media/reference/com.phronemophobic.clj-media.model.html) namespace.

;; ## clj-media/probe

;; The `probe` function will open a file and gather metadata.

;; _Note: `probe` takes a filename rather than `media` like `write!` and `frames`. That's
;; because `write!` and `frames` can work on filtered media, but `probe` cannot._

^{:nextjournal.clerk/visibility {:code :show :result :show}}
(clj-media/probe "media/birds.mp4")


;; ## Generating Media

;; Media can be created from raw sample frames via `clj-media/make-media`.
;; `clj-media/make-media` accepts a format and sequence of frames created
;; using `clj-media/make-frame`.

;; ### Audio generation example

{:nextjournal.clerk/visibility {:code :show :result :show}}
;; Import some helper classes.
(import '(java.nio FloatBuffer
                   ByteOrder
                   ByteBuffer))

(defn tone
  "Returns the amplitude of a `hz` sine wave at time `t` in seconds."
  [hz t]
  (* 0.8
     (Math/sin (* t (* 2 Math/PI)
                  hz))))

(defn tone-media
  "Returns media of a `hz` sine tone that lasts `seconds` seconds."
  [hz seconds]
  (let [sample-rate 44100
        num-samples (* sample-rate
                       seconds)
        format (clj-media/audio-format
                {:channel-layout "mono"
                 :sample-rate sample-rate
                 :sample-format :sample-format/flt})
        time-base sample-rate
        frame-size 1024]
    (clj-media/make-media
     format
     (sequence
      (comp
       (partition-all frame-size)
       (map (fn [ptss]
              (let [buf (doto (ByteBuffer/allocate (* frame-size
                                                      4))
                          (.order (ByteOrder/nativeOrder)))
                    fbuf (.asFloatBuffer buf)]
                (doseq [[i pts] (map-indexed vector ptss)]
                  (.put fbuf i (float (tone hz (/ pts
                                                  sample-rate)))))
                (clj-media/make-frame
                 {:bytes (.array buf)
                  :format format
                  :time-base time-base
                  :pts (first ptss)})))))
      (range num-samples)))))

(def my-tone-media (tone-media 440 3))

{:nextjournal.clerk/visibility {:code :hide :result :show}}
(write! my-tone-media
        "tone-440hz.mp3"
        {:audio-format
         (merge
          (:format my-tone-media)
          {:codec {:name "mp3",
                   :long-name "MP3 (MPEG audio layer 3)",
                   :id 86017}})})


;; ### Video Generation example

;; Import some helper classes.

{:nextjournal.clerk/visibility {:code :show :result :hide}}
(import 'java.awt.image.BufferedImage
        'java.awt.Graphics2D
        'java.awt.Color
        'java.awt.RenderingHints)

(def frame-img
  (BufferedImage. 100 100 BufferedImage/TYPE_3BYTE_BGR))

(def image-graphics
  (let [g (.createGraphics frame-img)]
    (.setRenderingHint ^Graphics2D g
                       RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint ^Graphics2D g
                       RenderingHints/KEY_TEXT_ANTIALIASING,
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    g))

(def frame-format
  (clj-media/video-format {:pixel-format :pixel-format/rgb24
                           :time-base 24
                           :line-size (* (.getWidth frame-img)
                                         ;; rgb24 has 3 bytes per pixel when packed
                                         3)
                           :width (.getWidth frame-img)
                           :height (.getHeight frame-img)}))

(defn img->frame [img pts time-base]
  (clj-media/make-frame
   {:bytes (-> img
               (.getData)
               (.getDataBuffer)
               (.getData))
    :format frame-format
    :time-base time-base
    :pts pts}))


(def generated-frames
  (let [frame-rate 24
        seconds 3
        num-frames (* seconds frame-rate)]
   (into []
         (map (fn [pts]
                (.setColor ^Graphics2D image-graphics (Color/WHITE))
                (.fillRect image-graphics 0 0
                           (.getWidth frame-img)
                           (.getHeight frame-img))
                (.setColor ^Graphics2D image-graphics (Color/BLACK))
                (.drawString image-graphics (str "pts: " pts)
                             5 50)
                (img->frame frame-img pts frame-rate)))
         (range num-frames))))


{:nextjournal.clerk/visibility {:code :hide :result :show}}
(write! (clj-media/make-media frame-format
                              generated-frames)
        "generated-movie.mp4"
        {:video-format
         {:codec {:name "h264",
                  :long-name "H.264 / AVC / MPEG-4 AVC / MPEG-4 part 10",
                  :media-type :media-type/video,
                  :id 27}
          :pixel-format :pixel-format/yuv420p}})


;; ## Filtering

;; In clj-media, filters are a transformation of frame to frames. However,
;; as a convenience, clj-media allows filters to be applied to media. In most
;; cases, it makes sense to treat related streams as a group for trimming,
;; cropping, etc.

;; Most filters are simple wrappers over ffmpeg's avfilter library and
;; can be found in the namespace `com.phronemophobic.clj-media.avfilter`.

;; As convention, the first argument to filters is always an optional
;; options map followed by any media inputs which allows filters to be chained
;; via thread last, `->>`.

;; Extra documentation about the filters in `com.phronemophobic.clj-media.avfilter` can be found at https://ffmpeg.org/ffmpeg-filters.html.

;; ### Filtering tips

;;- Almost all filters work on either just video or just audio. If you get unexpected results, check to see if you should also be applying a filter to the video or vice versa.
;;- Many audio filters use the "a" prefix (but some do not).
;;- As a convenience, some wrappers that package audio and video filters are provided (eg. `trim+`) and are given the `+` suffix.
;;- Filters get applied in order. If you only need to modify a small portion of media, consider trimming _before_ applying other filters to speed things up.

;; # Examples

;; ## Converting Video Files To Audio Files
(write! (->> (clj-media/file "media/birds.mp4")
             (clj-media/filter-audio))
        "my-video.mp3")

;; ## Change The Volume Of Audio Files
(write! (->> (clj-media/file "media/birds.mp4")
             (avfilter/volume {:volume "0.1"}))
        "quieter.mp4")

;; ## Removing Audio Stream From A Video File
(write! (->> (clj-media/file "media/birds.mp4")
             (clj-media/remove-audio))
        "my-soundless-video.mp4")

;; ## Removing Video Stream From A Media File
(write! (->> (clj-media/file "media/birds.mp4")
             (clj-media/remove-video))
        "my-videoless-video.mp4")

;; ## Extracting Images From The Video

^{:nextjournal.clerk/visibility {:code :show :result :show}}
(def img
  (first
   (into
    []
    (comp (take 1)
          (map mm/image))
    (clj-media/frames
     (clj-media/file "media/birds.mp4")
     :video
     {:format (clj-media/video-format
               {:pixel-format :pixel-format/rgb565le})}))))

;; ## Cropping Videos
(write! (->> (clj-media/file "media/birds.mp4")
             (avfilter/crop {:w "iw*0.5"
                             :keep-aspect true}))
        "my-cropped-video.mp4")
;; ## Convert A Specific Portion Of A Video
(write! (let [media (clj-media/file "media/birds.mp4")
              portion (->> media
                           (clj-media/filter-video )
                           (avfilter/crop {:w "iw*0.5"})
                           (avfilter/edgedetect))]
          (avfilter/overlay {:x "main_w/2 - overlay_w/2"}
                            media
                            portion))
        "my-cropped-edited-video.mp4")

;; ## Trim A Media File Using Start And Stop Times
(write! (->> (clj-media/file "media/birds.mp4")
             (avfilter/trim+ {:start 3e6
                              :duration 2e6}))
        "my-trimmed-video.mp4")
;; ## Merge Audio And Video Files
(write! (clj-media/union
         (clj-media/file "my-soundless-video.mp4")
         (clj-media/file "my-videoless-video.mp4"))
        "whole.mp4")

;; ## Increase/Decrease Video Playback Speed
(write! (->> (clj-media/file "media/birds.mp4")
             (avfilter/setpts {:expr "PTS/10"})
             (avfilter/atempo {:tempo 10}))
        "speed10x.mp4")

;; ## Create Animated GIF
(write! (->> (clj-media/file "media/birds.mp4")
             (clj-media/filter-video))
        "my-gif.gif")

;; ## Rotate Videos
(write! (->> (clj-media/file "media/birds.mp4")
             (avfilter/rotate {:a "t"}))
        "my-rotation.mp4")


 ;; ## Fade In and Out
(write! (let [vid (clj-media/file "media/birds.mp4")]
          (avfilter/concat
           ;; fade in
           (->> vid
                (avfilter/trim+ {:duration 1e6})
                (avfilter/fade {:duration 1e6}))
           ;; normal
           (->> vid
                (avfilter/trim+ {:duration 1e6
                                 :start 1e6}))
           ;; fade out
           (->> vid
                (avfilter/trim+ {:duration 1e6
                                 :start 2e6})
                (avfilter/fade {:duration 1e6
                                :type "out"}))))
        "my-fade-in-out.mp4")



^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/serve! {:watch-paths ["notebooks/docs.clj"]})

  ;; cd docs && python3 -m http.server
  ;; 
  (clerk/show! "notebooks/docs.clj")


  (clerk/build! {:paths ["notebooks/docs.clj"]
                 :out-path "docs/"
                 :bundle true})

  
  ,) 

