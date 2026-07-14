(ns com.phronemophobic.clj-media.impl.video-player.skia
  (:require [com.phronemophobic.clj-media.impl.flow :as impl.flow]
            [com.phronemophobic.clj-media.impl.datafy :as media.datafy]
            [com.phronemophobic.clj-media.impl.filter.avfilter :as avfilter]
            [com.phronemophobic.clj-media.impl.av :as av]
            [com.phronemophobic.clj-media.impl.model :as impl.model]
            [com.phronemophobic.clj-media.model :as model]
            [com.phronemophobic.clj-media.impl.raw :as raw]
            [com.phronemophobic.clj-media.impl.audio :as impl.audio]
            com.phronemophobic.clj-media.avfilter
            [com.phronemophobic.membrandt.icon.ui :as icon.ui]
            [com.phronemophobic.membrandt :as ant]
            [membrane.component.present :as present]
            [clojure.zip :as zip]
            [tech.v3.datatype.ffi :as dt-ffi]
            [tech.v3.datatype :as dt]
            [tech.v3.datatype.native-buffer :as native-buffer]
            [tech.v3.tensor :as dtt]
            [clojure.core.async.flow :as flow]
            [clojure.core.async :as async]
            [membrane.skia :as skia]
            [membrane.component :refer [defui defeffect]]
            [membrane.ui :as ui]
            [clojure.java.io :as io]
            [com.phronemophobic.easel :as-alias easel])
  (:import com.sun.jna.Pointer
           (javax.sound.sampled AudioFormat
                        AudioFormat$Encoding
                        AudioInputStream
                        AudioSystem
                        DataLine
                        DataLine$Info
                        Port$Info
                        Line
                        LineUnavailableException
                        SourceDataLine
                        TargetDataLine
                        UnsupportedAudioFileException
                        Mixer)))

;; There are plenty of improvements to be made
;; - a new flow is created for each seek or play. we can reuse flows which will better keep track of state. it will
;;   probably also be more efficient for seeks. we may need some method for either flushing channel buffers
;;   or just use smaller buffers.
;; - we special case `:file` type media parts. we probably want a better model for what media is seekable.
;;   the special case code should also be more generic.
;; - we do not yet support a volume knob
;; - adding some generic avfilter stuff would be neat.
;; - we do not yet support variable playback speeds.

(defprotocol IGetBuf
  (getbuf [_]))

(defn ;;^:private
  frame->pixmap [frame buf pixmap-id]
  (let [linesize (first (:linesize frame))
        {:keys [width height]} frame
        buf-size (* linesize height)
        buf (if (or (not buf)
                    (< (count buf) buf-size))
              (native-buffer/malloc buf-size)
              buf)
        _ (dt/copy! (native-buffer/wrap-address (first (:data frame))
                                                buf-size)
                    buf)
        
        pixmap (membrane.skia/pixmap pixmap-id
                                     (proxy [Pointer com.phronemophobic.clj_media.impl.video_player.skia.IGetBuf]
                                       [(.address (dt-ffi/->pointer buf))]
                                       (getbuf []
                                         ;; Important, must hold a reference to buf
                                         ;; to keep it from being garbage collected.
                                         buf)
                                       #_(toString []
                                         ;; just need to hole a reference to buf
                                         (str "wrapping" buf)))
                                     width
                                     height
                                     membrane.skia/kRGB_888x_SkColorType
                                     membrane.skia/kOpaque_SkAlphaType
                                     linesize)]
    pixmap))

(defn video-player-flow
  "Returns a flow ready to be presented. 
  Video streams will be decoded as rgb0.
  Audio streams will be s16, stereo, at 44100hz.
  
  Ports will be in :format->coord, which is a vector of [format coord] pairs"
  [{:keys [media
           repaint!]}]
  (let [g (impl.flow/->file-flow media )
        
        g
        (reduce
         (fn [g [format coord :as format-coord]]
           (if (= :frame (:container-type format))
             (update g :format->coord conj format-coord)
             ;; else
             (let [
                   decoder-pid (impl.flow/gen-pid "decoder")
                   decoder-flow {:procs {decoder-pid {:proc (-> (impl.flow/packet->frames)
                                                                flow/map->step
                                                                flow/process)}}
                                 :conns [[coord [decoder-pid :packet]]]}
                   g (impl.flow/merge-flows decoder-flow
                                            g)
                   output-format {:media-type (:media-type format)
                                  :container-type :frame}
                   g (update g :format->coord conj [output-format [decoder-pid :frame]])]
               g)))
         (assoc g :format->coord [])
         (:format->coord g))
        
        ;; transcode if necessary
        g
        (reduce
         (fn [g [format coord]]
           (let [transcode-pid (impl.flow/gen-pid "transcode")
                 media-type (:media-type format)
                 output-format (case media-type
                                 :media-type/video  {:pixel-format :pixel-format/rgb0
                                                     :media-type :media-type/video}
                                 :media-type/audio {:channel-layout "stereo"
                                                    :sample-rate 44100
                                                    :sample-format :sample-format/s16
                                                    :media-type :media-type/audio})
                 transcode-flow
                 {:procs {transcode-pid
                          {:proc (-> (avfilter/filter-proc [[:in ""]])
                                     flow/map->step
                                     flow/process)
                           :args {:filter-name (case media-type
                                                 :media-type/audio "anull"
                                                 :media-type/video "null")
                                  :output-format 
                                  (media.datafy/map->format output-format)}}}
                  :conns [[coord [transcode-pid :in]]]}
                 
                 g (impl.flow/merge-flows g transcode-flow)
                 g (update g :format->coord conj [output-format [transcode-pid :out]])]
             g))
         (assoc g :format->coord [])
         (:format->coord g))
        
        recycle-frame-chan (async/chan 10)

        
        audio-format (impl.audio/default-stereo-format)
        info (DataLine$Info. SourceDataLine
                             audio-format)
        source-data-line (^SourceDataLine AudioSystem/getLine info)
        source-data-line (doto ^SourceDataLine source-data-line
                           (.open audio-format)
                           (.start))
        
        base-t (promise)
        g (impl.flow/merge-flows
           g
           {:procs {:video-out {:proc (flow/process
                                       (flow/map->step
                                        {:describe (fn [] {:ins {:in "  "}
                                                           :outs {::impl.flow/recycle-frame "frame to recycle"}})
                                         :init (fn [m] (assoc m :pixmap-id 1))
                                         :transition
                                         (fn [m state]
                                           (case state
                                             ::flow/stop (do
                                                           (.drain source-data-line)
                                                           (.close source-data-line))
                                             nil)
                                           m)
                                         :transform (fn [state _ msg]
                                                      (case (:type msg)
                                                        :stream-opened 
                                                        (let [time-base-struct (-> msg :format :time-base)
                                                              time-base (/ (:num time-base-struct)
                                                                           (:den time-base-struct))]
                                                          (tap> {:stream-opened-video msg})
                                                          [(assoc state :time-base time-base)])

                                                        :stream-closed [state]
                                                        :new-frame
                                                        (let [buf (:buf state)
                                                              
                                                              frame (:frame msg)
                                                              pixmap-id (:pixmap-id state)
                                                              pixmap (frame->pixmap frame buf pixmap-id)
                                                              buf (getbuf (:buf pixmap))

                                                              ;; linesize (first (:linesize frame))
                                                              ;; {:keys [width height]} frame
                                                              ;; buf-size (* linesize height)
                                                              ;; buf (if (or (not buf)
                                                              ;;             (< (count buf) buf-size))
                                                              ;;       (native-buffer/malloc buf-size)
                                                              ;;       buf)
                                                              ;; _ (dt/copy! (native-buffer/wrap-address (first (:data frame))
                                                              ;;                                         buf-size)
                                                              ;;             buf)
                                                              
                                                              ;; pixmap-id (:pixmap-id state)
                                                              ;; pixmap (membrane.skia/pixmap pixmap-id
                                                              ;;                              (proxy [Pointer]
                                                              ;;                                [(.address (dt-ffi/->pointer buf))]
                                                              ;;                                (toString []
                                                              ;;                                  ;; just need to hole a reference to buf
                                                              ;;                                  (str "wrapping" buf)))
                                                              ;;                              width
                                                              ;;                              height
                                                              ;;                              membrane.skia/kRGB_888x_SkColorType
                                                              ;;                              membrane.skia/kOpaque_SkAlphaType
                                                              ;;                              linesize)
                                                              
                                                              state (assoc state
                                                                           :pixmap-id (inc pixmap-id)
                                                                           :buf buf)
                                                              
                                                              audio-us (SourceDataLine/.getMicrosecondPosition source-data-line)
                                                              t (/ audio-us
                                                                   1000000)
                                                              pts (:pts frame)
                                                              absolute-frame-t (* (:time-base state)
                                                                                  pts)
                                                              frame-t (- absolute-frame-t
                                                                         @base-t)]
                                                          (when (> frame-t t)
                                                            (let [delta-t (- frame-t t)
                                                                  delta-ms (Number/.longValue (* 1000 delta-t))]
                                                              (Thread/sleep delta-ms)))

                                                          (repaint! {:pixmap pixmap
                                                                     :timestamp absolute-frame-t})
                                                          [state
                                                           {::impl.flow/recycle-frame [frame]}])))}))}
                    :audio-out {:proc (flow/process
                                       (flow/map->step
                                        {:describe (fn [] {:ins {:in "  "}
                                                           :outs {::impl.flow/recycle-frame "frame to recycle"}})
                                         :init (fn [m] m)
                                         :transition (fn [m state] m)
                                         :transform (fn [state _ msg]
                                                      (case (:type msg)
                                                        :stream-opened 
                                                        (do
                                                          (tap> {:stream-opened-audio msg})
                                                          
                                                          (let [time-base-struct (-> msg :format :time-base)
                                                                time-base (/ (:num time-base-struct)
                                                                             (:den time-base-struct))]
                                                            [(assoc state :time-base time-base)]))

                                                        :stream-closed [state]
                                                        :new-frame
                                                        (let [frame (:frame msg)
                                                              ;; linesize (-> frame :linesize first)
                                                              sample-format (:format frame)
                                                              bytes-per-sample (raw/av_get_bytes_per_sample sample-format)
                                                              buf-size (* bytes-per-sample
                                                                          (:nb_samples frame )
                                                                          (-> frame :ch_layout :nb_channels))

                                                              
                                                              ^bytes
                                                              ba (:byte-array state)
                                                              ba (if (or (not ba)
                                                                         (< (alength ba) buf-size))
                                                                   (byte-array buf-size)
                                                                   ba)
                                                              _ (dt/copy! (native-buffer/wrap-address (first (:data frame))
                                                                                                      buf-size)
                                                                          (dtt/select ba (range buf-size)))
                                                              state (assoc state :byte-array ba)
                                                              state (if (:base-t state)
                                                                      state
                                                                      (let [t (* (:pts frame)
                                                                                 (:time-base state))]
                                                                        (deliver base-t t)
                                                                        (assoc state :base-t t)))]
                                                          
                                                          (.write source-data-line ba 0 buf-size)
                                                          
                                                          [state
                                                           {::impl.flow/recycle-frame [frame]}
                                                           ])
                                                        ))}))}}
            :conns (into []
                         ;; assume just audio and video streams
                         (comp (map (fn [[format coord]]
                                      (let [out-pid (case (:media-type format)
                                                      :media-type/audio :audio-out
                                                      :media-type/video :video-out)]
                                        [coord [out-pid :in]]))))
                         (:format->coord g))})
        
        g (-> g
              (impl.flow/add-frame-recycler)
              (impl.flow/add-packet-recycler)
              (assoc-in [:procs ::impl.flow/frame-recycler :args :recycle-frame] recycle-frame-chan))]
    g)) 



(def fname "/Users/adrian/workspace/clj-media/symbolics.mp4")


;; zip stuff

(defmulti media-zip-branch? :type)
(defmulti media-zip-children :type)
(defmulti media-zip-make-node :type)


(defn media-zip-branch?-media [media] true)
(defn media-zip-children-media [media]
  (when-let [child (:media media)]
    (list child)))
(defn media-zip-make-node-media [media children]
  (assert (<= (count children) 1))
  (assoc media :media (first children)))

(defmacro add-media-zip-methods []
  `(do
     ~@(eduction
        (mapcat
         (fn [kw]
           [`(defmethod media-zip-branch? ~kw [media#]
               (media-zip-branch?-media media#))
            `(defmethod media-zip-children ~kw [media#]
               (media-zip-children-media media#))
            `(defmethod media-zip-make-node ~kw [media# children#]
               (media-zip-make-node-media media# children#))]))
        [:filter-video
         :remove-video
         :filter-audio
         :remove-audio])))

(add-media-zip-methods)

(defn media-zip-branch?-inputs [media] true)
(defn media-zip-children-inputs [media]
  (:inputs media))
(defn media-zip-make-node-inputs [media children]
  (assoc media :inputs children))

(defmacro add-inputs-zip-methods []
  `(do
     ~@(eduction
        (mapcat
         (fn [kw]
           [`(defmethod media-zip-branch? ~kw [media#]
               (media-zip-branch?-inputs media#))
            `(defmethod media-zip-children ~kw [media#]
               (media-zip-children-inputs media#))
            `(defmethod media-zip-make-node ~kw [media# children#]
               (media-zip-make-node-inputs media# children#))]))
        [:avfilter
         :union
         :concat])))

(add-inputs-zip-methods)

(defmacro add-empty-zip-methods []
  `(do
     ~@(eduction
        (mapcat
         (fn [kw]
           [`(defmethod media-zip-branch? ~kw [media#]
               false)
            `(defmethod media-zip-children ~kw [media#]
               nil)
            `(defmethod media-zip-make-node ~kw [media# children#]
               media#)]))
        [:frames
         :file])))

(add-empty-zip-methods)

(defn media-zip [media]
  (zip/zipper media-zip-branch?
              media-zip-children
              media-zip-make-node
              media))

(defn ^:private set-start-percent [media percent]
  (loop [zip (media-zip media)]
    (if (zip/end? zip)
      (zip/root zip)
      (let [zip (zip/edit zip
                          (fn [media]
                            (case (:type media)
                              :file (assoc media :start-percent percent)
                              ;; else
                              media)))]
        (recur (zip/next zip))))))

;; TODO: this doesn't seem to work for .mkv files
;; even though the duration is available in meta data
(defn ^:private extract-duration [media]
  (when-let [root-file (loop [zip (media-zip media)]
                         (if (zip/end? zip)
                           (zip/root zip)
                           (let [media (zip/node zip)]
                             (case (:type media)
                               :file (:file media)
                               ;; else
                               (recur (zip/next zip))))))]
    (let [f (io/file root-file)
          path (java.io.File/.getCanonicalPath f)
          {:keys [streams]} (av/probe path)
          max-duration (transduce
                        (map (fn [{:keys [time-base estimated-duration]}]
                               (let [[num den] time-base]
                                 (/ (* num estimated-duration)
                                    den))))
                        (completing max)
                        0
                        streams)]
      max-duration)))

(defeffect ::toggle-play [{:keys [$player $pm seek $timestamp $percent media]}]
  (assert (and $player $pm $timestamp $percent media))
  (dispatch! ::easel/enqueue
             {:f
              (fn []
                (if-let [{:keys [flow stop*]} (dispatch! :get $player)]
                  (let [stopped (vswap! stop* not)] 
                    (if stopped
                      (flow/pause flow)
                      (flow/resume flow)))
                  ;; else start
                  (let [duration (extract-duration media)
                        stop* (volatile! false)
                        g (video-player-flow 
                           {:media media
                            :repaint! (fn [{:keys [pixmap timestamp]}]
                                        (when (not @stop*)
                                          (dispatch! :set $timestamp timestamp)
                                          (let [percent (/ timestamp duration)]
                                            (dispatch! :set $percent percent))
                                          (dispatch! :set $pm pixmap)
                                          (dispatch! :repaint!)))})]
                    (let [flow (flow/create-flow g)]
                      (dispatch! :set $player {:g g
                                               :stop* stop*
                                               :flow flow})
                      (tap> g)
                      (impl.flow/track-flow flow)
                      
                      (-> (flow/start flow)
                          (impl.flow/monitoring))
                      (flow/resume flow)))))}))

(defeffect ::player-seek [{:keys [$player $pm seek $timestamp $percent media]}]
  (assert (and $player $pm $timestamp $percent seek media))
  (dispatch! ::easel/enqueue
             {:f
              (fn []
                (when-let [player (dispatch! :get $player)]
                  (let [{:keys [flow stop*]} player]
                    (vreset! stop* true)
                    (flow/stop flow)
                    
                    (dispatch! :set $player nil)))
                (let [duration (extract-duration media)
                      stop* (volatile! false)
                      media (set-start-percent media seek)
                      g (video-player-flow 
                         {:media media
                          :repaint! (fn [{:keys [pixmap timestamp]}]
                                      (when (not @stop*)
                                        (dispatch! :set $timestamp timestamp)
                                        (let [percent (/ timestamp duration)]
                                          (dispatch! :set $percent percent))
                                        (dispatch! :set $pm pixmap)
                                        (dispatch! :repaint!)))})
                      flow (flow/create-flow g)]
                  
                  (dispatch! :set $player {:g g
                                           :stop* stop*
                                           :flow flow})
                  (tap> g)
                  (impl.flow/track-flow flow)
                  
                  (-> (flow/start flow)
                      (impl.flow/monitoring))
                  (flow/resume flow)))}))

(defui media-controls [{:keys [width index player]}]
  (ui/flex-layout
   [(ui/on
     :mouse-down
     (fn [_]
       [[::toggle-play {}]])
     
     (icon.ui/icon {:name "play-circle"}))
    (ui/wrap-on
     ;; only seek on mouse up
     :mouse-move
     (fn [handler mpos]
       (let [intents (handler mpos)]
         (into []
                 (remove (fn [[type & _]]
                           (= type ::player-seek)))
                 intents)))
     :mouse-event
     (fn [handler pos button mouse-down? mods]
       (let [intents (handler pos button mouse-down? mods)]
         (if (not mouse-down?)
           intents
           (into []
                 (remove (fn [[type & _]]
                           (= type ::player-seek)))
                 intents))))
     (ui/on
      ::ant/update-number-slider
      (fn [{:keys [slider mpos] :as m}]
        (let [new-index (ant/calculate-slider-val slider mpos)]
          [[::player-seek {:$player $player
                           :new-index new-index}]]))
      (ant/number-slider {:width width
                          :integer? true
                          :min 0
                          :max width
                          :value index})))]
   {:direction :row
    :gap 21
    :align :center}))

(defui fframe [{:keys [frame* ready?]}]
  (when ready?
    @frame*))

(defeffect ::load-first-frame [{:keys [media $frame]}]
  (let [frame* (delay 
                 (let [frames (impl.flow/frames-reducible 
                               media :video
                               {:format {:pixel-format :pixel-format/rgb0
                                         :media-type :media-type/video}})
                       pixmap (transduce (take 1)
                                         (completing
                                          (fn [_ frame]
                                            (frame->pixmap (impl.model/raw-frame frame)
                                                           nil 0)))
                                         nil
                                         frames)]
                   (dispatch! :update $frame assoc :ready? true)
                   pixmap))]
    (dispatch! :update $frame (fn [f]
                                (or f
                                    (fframe {:ready? false
                                             :frame* frame*})))))
  (future
    (-> (dispatch! :get $frame)
        :frame*
        deref)))

(defui wrap-first-frame [{:keys [media]}]
  (let [frame (get extra ::frame)]
    (if frame
      frame
      (present/on-present
       (fn []
         [[::load-first-frame {:$frame $frame
                               :media media}]])
       nil))))

(comment
  
  (dev/add-component-as-applet
   #'wrap-first-frame
   {:media {:type :file
            :file "/Users/adrian/workspace/clj-media/symbolics.mp4"}})
  ,)

(defui video-player [{:keys [media size]}]
  (let [[cw ch] (or size
                    (:membrane.stretch/container-size context))
        controls-width (long (* 0.75 cw))

        pm (::pm extra)
        player (::player extra)
        timestamp (get extra ::timestamp 0)
        percent (get extra ::percent 0)
        index (long (* percent controls-width))

        controls (ui/on 
                  ::player-seek
                  (fn [{:keys [new-index]}]
                    (let [new-percent (/ new-index controls-width)]
                      [[::player-seek {:$player $player
                                       :$timestamp $timestamp
                                       :$percent $percent
                                       :media media
                                       :seek new-percent
                                       :$pm $pm} ]]))

                  ::toggle-play
                  (fn [_]
                    [[::toggle-play {:$player $player
                                     :$timestamp $timestamp
                                     :$percent $percent
                                     :media media
                                     :seek percent
                                     :$pm $pm}]])
                  (media-controls {:width controls-width
                                   :index index
                                   :player player} ))
        [controls-width controls-height] (ui/bounds controls)
        
        pad 20
        [screen-width screen-height] [(- cw pad)
                                      (- ch controls-height pad)]

        frame-view (or pm
                       (wrap-first-frame {:media media}))
        [pw ph] (ui/bounds frame-view)
        
        frame-view (if (and (pos? pw)
                            (pos? ph))
                     (let [scale (min (/ screen-width pw)
                                      (/ screen-height ph))]
                       (ui/scale scale scale frame-view))
                     frame-view)]
    (ui/vertical-layout
     (ui/on :mouse-down
            (fn [_]
              [[::toggle-play {:$player $player
                               :$timestamp $timestamp
                               :$percent $percent
                               :seek percent
                               :media media
                               :$pm $pm}]])
            (ui/fixed-bounds 
             [screen-width screen-height]
             frame-view))
     controls)))



(comment
  (dev/add-component-as-applet #'video-player {})
  
  (impl.flow/stop-all-flows!)
  ,)


