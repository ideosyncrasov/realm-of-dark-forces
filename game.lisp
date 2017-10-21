(in-package :rdf)

;; game logic

(defclass game-state ()
  ((in-game-time
     :initarg :in-game-time
     :initform (* 6 60) ;6 am
     :accessor in-game-time
     :type integer)
   (time-val
     :initarg :time-val
     :accessor time-val
     :type real)
   (player-character
     :initarg :player-character
     :initform (make-instance 'animated-game-character)
     :reader player-character
     :type 'animated-game-character)))

(defun update-world (state)
  (let* ((cur-time (cur-time-in-secs))
         (dt (- cur-time
                (time-val state)))
         (player-char (player-character state)))
    (incf (in-game-time state)
          (truncate (* 12 24 dt))) ;5 min real time = 24 hours game time
    (setf (time-val state) cur-time)
    (when (walking-p player-char)
    (setf (walking-animation-phase player-char)
          (mod (+ (walking-animation-phase player-char)
                  (* 4 dt))
               4))
    (case (facing player-char)
      (:up (incf (gamekit:y (pos player-char))
                 (* 64 dt)))
      (:down (decf (gamekit:y (pos player-char))
                   (* 64 dt)))
      (:right (incf (gamekit:x (pos player-char))
                    (* 64 dt)))
      (:left (decf (gamekit:x (pos player-char))
                   (* 64 dt))))
    (when (active-animation player-char)
      (animate (active-animation player-char))))))
