(in-package :rdf)

;; animation stuff

(defclass animation ()
  ((anim-start-time
     :initarg :anim-start-time
     :reader anim-start-time
     :type real)
   (render-layer
     :initarg :render-layer
     :initform nil
     :reader render-layer
     :type '(or string nil))))

(defgeneric animate (animation state))
(defgeneric draw (animation state)) ;draw a visual representation of the current animation state
(defgeneric pre-animation (animation state)) ;called when the animation is activated
(defgeneric post-animation (animation state)) ;called when the animation is done

(defmethod animate ((animation animation) (state game-state)))
(defmethod draw ((animation animation) (state game-state)))
(defmethod pre-animation ((animation animation) (state game-state)))
(defmethod post-animation ((animation animation) (state game-state)))

;convenience function that gives the time since the start of the animation
(defgeneric anim-time (animation state))
(defmethod anim-time ((anim animation) (state game-state))
  (- (time-val state)
     (anim-start-time anim)))

(defun schedule-animation (anim state)
  (pushnew anim (scheduled-animations state)))
(defun stop-animation (anim state)
  (pushnew anim (finished-animations state)))

(defclass walking-animation (animation)
  ((pos
     :initarg :pos
     :accessor pos)
   (path ;piecewise rectilinear path represented as a list of successive points
     :initarg :path
     :accessor path)
   (facing
     :initarg :facing
     :accessor facing
     :type *facing-type*)))

(defclass alarm-clock-animation (animation) ()
  (:default-initargs
    :render-layer "items"))

(defmethod pre-animation ((anim alarm-clock-animation) (state game-state))
  (setf (alarm-sounds-p state) t)
  (gamekit:play :alarm-sound))

(defmethod post-animation ((anim alarm-clock-animation) (state game-state))
  (setf (alarm-sounds-p state) nil))

(defmethod animate ((anim alarm-clock-animation) (state game-state))
  (when (>= (in-game-time state) (+ (* 8 60) 15))
      (stop-animation anim state)))

(defmethod draw ((anim alarm-clock-animation) (state game-state))
  (when (<= (mod (anim-time anim state) 0.5)
          0.25)
      (gamekit:print-text "*BEEP*" 300 420)))

(defclass situation-animation (animation) ())

;transition to new situation of player character
(defun transition-to (state situation)
  (setf (situation (player-character state)) :in-transition)
  (schedule-animation (make-instance 'situation-transition-animation
                       :target-situation situation)
                      state))

(defmethod animate ((anim situation-animation) (state game-state))
  ;player state transitions
  (case (situation (player-character state))
    (:sleeping
      (when (alarm-sounds-p state)
        (stop-animation anim state)
        (transition-to state :in-bed)))
    (otherwise t)))

(defclass situation-transition-animation (animation)
  ((target-situation
     :initarg :target-situation
     :accessor target-situation
     :type *situation-type*)))

(defmethod post-animation ((anim situation-transition-animation) (state game-state))
  (setf (situation (player-character state))
        (target-situation anim))
  (push (make-instance 'situation-animation)
        (active-animations state)))
