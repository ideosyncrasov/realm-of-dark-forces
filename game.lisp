(in-package :rdf)

;; game logic

(defclass game-state ()
  ((in-game-time
     :initarg :in-game-time
     :initform (* 6 60) ;6 am
     :accessor in-game-time
     :type real)
   (alarm-sounds-p
     :initarg :alarm-sounds-p
     :initform nil
     :accessor alarm-sounds-p
     :type boolean)
   (time-val
     :initarg :time-val
     :accessor time-val
     :type real)
   (player-character
     :initarg :player-character
     :initform (make-instance 'animated-game-character)
     :reader player-character
     :type 'animated-game-character)
   (active-animations
     :initform '()
     :accessor active-animations
     :type list)
   (scheduled-animations
     :initform '()
     :accessor scheduled-animations
     :type list)
   (finished-animations
     :initform '()
     :accessor finished-animations
     :type list)
   (sun-light
     :initform (gamekit:vec4 1 0 0 1)
     :accessor sun-light)))

(defun rectangles-intersect-p (llx1 lly1 urx1 ury1 llx2 lly2 urx2 ury2)
  (and (<= 0 (- (min urx2 urx1) (max llx2 llx1)))
       (<= 0 (- (min ury2 ury1) (max lly2 lly1)))))

(defun can-walk-p (dx dy)
  (let ((layer (layer-by-name *apartment-tile-map* "collision"))
        (x (gamekit:x (pos (player-character *game-state*))))
        (y (gamekit:y (pos (player-character *game-state*))))
        (ox (- (tile-width *apartment-tile-map*)))
        (oy (* (1- (num-rows *apartment-tile-map*)) (tile-height *apartment-tile-map*))))
    (when (< dx 0)
      (setf dx (- dx))
      (decf x dx))
    (when (< dy 0)
      (setf dy (- dy))
      (decf y dy))
    (block loopy
           (dotimes (row (num-rows layer) t)
             (dotimes (col (num-cols layer))
               (unless (= (aref (data layer) row col)
                          0)
                 (when (rectangles-intersect-p x y (+ x dx) (+ y dy)
                                               (+ ox (* col (tile-width *apartment-tile-map*)))
                                               (+ oy (* -1 (1+ row) (tile-height *apartment-tile-map*)))
                                               (+ ox (* (1+ col) (tile-width *apartment-tile-map*)))
                                               (+ oy (* -1 row (tile-height *apartment-tile-map*))))
                   (return-from loopy))))))))

(defun update-world (state dt)
  (let ((player-char (player-character state))
        (old-in-game-time (in-game-time state)))
    ;in game time
    (setf (in-game-time state)
          (mod (+ old-in-game-time
                  (* 4.8 dt)) ;5 min real time = 24 hours in game time
               (* 24 60))) ;in-game-time is in minutes
    ;time
    (incf (time-val state) dt)
    ;sunlight
    (let* ((sun-phase (* (- (in-game-time *game-state*)
                            (+ (* 6 60) 30)) ;sunrise at 6:30 am
                         (/ (* 2 pi)
                            (* 24 60))))
           (y (max 0 (sin sun-phase)))
           (x (expt y (- 0.5 (* 0.25 y y))))
           (r x)
           (g (max 0 (- x 0.2)))
           (b (min 0.2 (- 0.2 x)))
           (alpha (min 0.6 (- 1 x))))
      (setf (sun-light state)
            (gamekit:vec4 r g b alpha)))
    ;alarm clock
    (and (not (alarm-sounds-p state))
         (<= old-in-game-time (* 8 60))
         (>= (in-game-time state) (* 8 60))
         (schedule-animation
           (make-instance 'alarm-clock-animation
                          :anim-start-time (time-val state))
           state))
    ;walking animation
    (when (walking-p player-char)
      (setf (walking-animation-phase player-char)
            (mod (+ (walking-animation-phase player-char)
                    (* 4 dt))
                 4))
      (case (facing player-char)
        (:up (let ((dy (* 64 dt)))
               (when (can-walk-p 0 dy)
                 (incf (gamekit:y (pos player-char))
                       dy))))
        (:down (let ((dy (* -64 dt)))
                 (when (can-walk-p 0 dy)
                   (incf (gamekit:y (pos player-char))
                         dy))))
        (:right (let ((dx (* 64 dt)))
                  (when (can-walk-p dx 0)
                    (incf (gamekit:x (pos player-char))
                          dx))))
        (:left (let ((dx (* -64 dt)))
                 (when (can-walk-p dx 0)
                   (incf (gamekit:x (pos player-char))
                         dx))))))
    ;animations
    (dolist (anim (active-animations state))
      (animate anim state))
    (dolist (anim (scheduled-animations state))
      (pre-animation anim state))
    (dolist (anim (finished-animations state))
      (post-animation anim state))
    (setf (active-animations state)
          (nset-difference (nunion (scheduled-animations state)
                                   (active-animations state))
                           (finished-animations state)))
    (setf (scheduled-animations state) nil)
    (setf (finished-animations state) nil)))
