(in-package :rdf)

(defclass game-character ()
  ((pos
     :initarg :pos
     :accessor pos)
   (facing
     :initarg :facing
     :accessor facing
     :type '(or (eql :up)
                (eql :down)
                (eql :left)
                (eql :right)))
   (animation-phase
     :initarg :animation-phase
     :initform 0
     :accessor animation-phase
     :type real)
   (walking-p
     :initarg :walking-p
     :initform nil
     :accessor walking-p)))
