(ns com.phronemophobic.clj-media.video
  (:require [clojure.java.io :as io]
            [com.phronemophobic.clj-media.av :as av]
            [com.phronemophobic.clj-media.av.raw :as raw
             :refer :all]
            [clojure.pprint :refer [pprint]])
  (:import
   com.sun.jna.Memory
   com.sun.jna.Pointer
   com.sun.jna.ptr.PointerByReference
   com.sun.jna.ptr.IntByReference
   com.sun.jna.ptr.ByteByReference
   com.sun.jna.Structure)
  (:gen-class))

(raw/import-structs!)

(defn pf [& args]
  (apply prn args)
  (flush))

(defn transcode-frame [decoder-context pix-fmt]
  (fn [rf]
    (let [frame (av_frame_alloc)

          filter-graph (avfilter_graph_alloc)

          buffer (avfilter_get_by_name "buffer")
          _ (when (nil? buffer)
              (throw (Exception.)))
          buffer-context (avfilter_graph_alloc_filter filter-graph buffer "src")
          time-base (.readField decoder-context "time_base")
          args (format "video_size=%dx%d:pix_fmt=%d:time_base=%d/%d"
                       (.readField decoder-context "width")
                       (.readField decoder-context "height")
                       (.readField decoder-context "pix_fmt")
                       (.readField time-base "num")
                       (.readField time-base "den"))
          
          _ (prn args)
          err (avfilter_init_str buffer-context args)
          _ (when (not (zero? err))
              (throw (Exception.)))

          buffersink (avfilter_get_by_name "buffersink")
          _ (when (nil? buffersink)
              (throw (Exception.)))
          buffersink-context* (PointerByReference.)
          _ (avfilter_graph_create_filter buffersink-context*
                                          buffersink
                                          "sink"
                                          nil
                                          nil
                                          filter-graph)

          buffersink-context (.getValue buffersink-context*)

          pix-fmts (doto (IntByReference.)
                        (.setValue pix-fmt))
          _ (av_opt_set_bin buffersink-context "pix_fmts"
                            pix-fmts
                            (* 1 4)
                            AV_OPT_SEARCH_CHILDREN)

          err (avfilter_link buffer-context 0
                             buffersink-context 0)
          _ (when (not (zero? err))
              (throw (Exception.)))
          err (avfilter_graph_config filter-graph nil)]
      (when (not (>= err 0))
        (prn err)
        (throw (Exception.)))
      (fn
        ([]
         (rf))
        ([result]
         (av_frame_free (doto (PointerByReference.)
                          (.setValue (.getPointer frame))))
         (avfilter_graph_free
          (doto (PointerByReference.)
            (.setValue (.getPointer filter-graph))))
         (rf result))
        ([result input-frame]
         (av_buffersrc_add_frame buffer-context
                                 input-frame)
         (loop [result result]
           (let [err (av_buffersink_get_frame_flags buffersink-context
                                                    frame
                                                    0)]
             (cond
               (zero? err)
               (let [result (rf result frame) ]
                 (if (reduced? result)
                   result
                   (recur result)))

               (av/eagain? err)
               result

               (av/eof? err)
               (reduced result)

               :else
               (reduced {:error-code err
                         :error-msg (av/error->str err)
                         :type :transcode-error})))))))))

(defmacro with-tile [[tile-bind img] & body]
  `(let [img# ~img
         ~tile-bind (.getWritableTile img# 0 0)]
     (try
       ~@body
       (finally
         (.releaseWritableTile img# 0 0)))))

(defn render-frame
  "Writes an AVFrame into a BufferedImage. Assumes :byte-bgr image format."
  [img frame]
  (let [
        width (.readField frame "width")
        height (.readField frame "height")
        linesize (-> frame
                     (.readField "linesize")
                     (nth 0))
        buf-ptr (-> frame
                    (.readField "data")
                    (nth 0)
                    (.getPointer))]

    (with-tile [wraster img]
      (doseq [y (range height)]
        (.setDataElements wraster 0 y width 1
                          (.getByteArray buf-ptr (* linesize y) linesize))))))
