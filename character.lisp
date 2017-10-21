(in-package :rdf)

(defclass emotional-state ()
  ((awareness
     :initarg :awareness
     :initform 0
     :accessor awareness)
   ;positive values indicate arousal,
   ;negative values indicate calmness
   (arousal
     :initarg :arousal
     :initform 0
     :accessor arousal)
   ;positive values indicate content,
   ;negative values indicate discontent
   (content
     :initarg :content
     :initform 0
     :accessor content)
   (energy
     :initarg :energy
     :initform 0
     :accessor energy)
   ;is a list that can contain any of the following
   ; :hunger :sleepiness
   (wants
     :initarg :wants
     :initform '()
     :accessor wants)))

(defvar *situation-type* '(or (eql :sleeping)
                              (eql :in-bed)
                              (eql :eating)
                              (eql :walking)))

(defclass animation () ())
(defgeneric animate (animation state))

(defclass game-character ()
   ((situation
      :initarg :situation
      :initform :in-bed
      :type *situation-type*)
    (active-animation
      :initarg :active-animation
      :initform nil
      :accessor active-animation
      :type '(or (eql nil)
                 animation))
    (inner-state
     :initarg :inner-state
     :initform (make-instance 'emotional-state)
     :accessor inner-state)))

(defclass animated-character ()
  ((pos
     :initarg :pos
     :initform (gamekit:vec2 0 0)
     :accessor pos)
   (facing
     :initarg :facing
     :initform :up
     :accessor facing
     :type '(or (eql :up)
                (eql :down)
                (eql :left)
                (eql :right)))
   (walking-animation-phase
     :initarg :walking-animation-phase
     :initform 0
     :accessor walking-animation-phase
     :type real)
   (walking-p
     :initarg :walking-p
     :initform nil
     :accessor walking-p)))

(defclass animated-game-character (game-character animated-character)
  ())
