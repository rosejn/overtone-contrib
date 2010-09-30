(ns
    #^{:doc "A library with which to fetch events from a 3DConnexion SpaceNavigator 3D mouse"
       :author "Sam Aaron"}
  devices.space-nav
  (:import
   (net.java.games.input AbstractController AbstractComponent ControllerEnvironment))
  (:use overtone.live))

(defn find-controller
  "Find a HID controller that matches the supplied regexp name-matcher. If there is more than one match, the first is selected"
  [name-matcher]
  (let [default-env (ControllerEnvironment/getDefaultEnvironment)
        matcher     #(re-find name-matcher (.toString %))]
    (first (filter matcher (.getControllers default-env)))))

(defn list-controllers []
  (let [env (ControllerEnvironment/getDefaultEnvironment)]
    (seq (.getControllers env))))

(defn components
  "Fetch a list of components associated with a given controller"
  [controller]
  (let [component-list (.getComponents controller)]
    (reduce #(assoc %1 (.getName %2) %2) {} component-list)))

(defn find-component
  "Find a controller's component based on the component's name"
  [controller component-name]
  (let [components (components controller)]
    (components component-name)))

(defn device
  "Return a map for a given device which has keys for its name, controler and components"
  [name-matcher]
  (let [controller (find-controller name-matcher)
        components (components controller)
        name       (.getName controller)]
    {:name       name
     :controller controller
     :components components}))

(defn poll-and-read-component
  "Poll the controller to refresh its current values and then read the value of a given component"
  [#^AbstractController controller #^AbstractComponent component]
  (let [_   (.poll controller )
        val (.getPollData component)]
    val))

(defn read-component
  "Read the value of a given controller's component without polling the controller to refresh the values"
  [#^AbstractComponent component]
  (.getPollData component))

(defn read-all-vals
  "Read all the component values for a given device. Returns a map of component name to current value."
  [device]
  (let [components                      (device :components)
        #^AbstractController controller (device :controller)
        _                               (.poll controller)
        combine-name-val                (fn [[name comp]] [name (read-component comp)])
        vals                            (into {} (map combine-name-val components))]
    vals))

(defn space-action
  [new-vals]
  (let [x    (new-vals "x")
  ;      ry   (new-vals "ry")
        z    (new-vals "z")
        y    (new-vals "y")
        freq (Math/abs (+ 50 (* 1000 (/ (+ 1000 (new-vals "x")) 2000))))
        amp  (Math/abs (/ (+ 1000 (new-vals "x")) 2000))]
    ;(println new-vals)
    (println "freq: " freq "\namp: " amp)
    (snd "/c_set" 10 freq)
    (snd "/c_set" 11 amp)))

(defn poll
  "Agent function to poll a given device every at a specified frequency (in ms). Updates the agent with the new
   device values. Currently prints out the values of the device if they have changed since the last poll."
  ([x device old-vals freq]
     (let [new-vals (read-all-vals device)]
       (send-off *agent* #'poll device new-vals freq)
       (if-not (= old-vals new-vals) (space-action new-vals))
       (java.lang.Thread/sleep freq)
       new-vals)))

(comment )
(def space (device #"Space"))
(def poller (agent nil))

(refer-ugens)

(println "booting")
(boot)

(defsynth pad [freq 440 t 4 amt 0.3 lfo 2]
  (let [f-env      (env-gen (perc t t))
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (* 0.9 (env-gen (perc 0.1 0.8) (impulse lfo)) echo)))

(defsynth sound [freq 440 amp 0.2]
 (* amp 
    (env-gen (perc 0.1 0.3) (impulse amp))
    (rlpf (saw [freq (+ freq 7)]) (* 0.8 freq) 0.3)))

(def s (pad))
(node-map-controls s "freq" 10)
(node-map-controls s "lfo" 11)

;d.slots[3].at( 48 ).createBus( s );
;;b = Buffer.read(s, "/Users/sam/Documents/music/samples/plagasul/mellow_plagasul_guitars/1281_plagasul_arpeggio6lop.wav")
;;SynthDef( \tom, { |out=0, rate=0, inter=2,b| Out.ar( out, BufRd.ar(1, b, Phasor.ar(0, BufRateScale.kr(0) * (rate - 0.5) * 12, 0, BufFrames.kr(0))), 1, inter)}).load(s);
;;Synth.new( \tom ).map( \rate, d.slots[3].at( 48 ).bus );
(sound)

(java.lang.Thread/sleep 2000)

(send-off poller poll space {} 40)

  )
