(in-package :rdf)

(defvar *canvas-width* 640)
(defvar *canvas-height* 480)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *box-pos* (gamekit:vec2 0 0))
(defvar *curve* (make-array 4
                            :initial-contents
                            (list (gamekit:vec2 300 300)
                                  (gamekit:vec2 375 300)
                                  (gamekit:vec2 425 300)
                                  (gamekit:vec2 500 300))))
(defvar *origin* (gamekit:vec2 0 0))
(defvar *cursor-pos* (gamekit:vec2 0 0))
(defvar *head-grabbed-p* nil)

(defclass main (gamekit:gamekit-system) ()
  (:default-initargs
    :resource-path "common-lisp/realm of dark forces/"
    :viewport-width *canvas-width*
    :viewport-height *canvas-height*
    :viewport-title "Realms of Dark Forces"))

(defmethod gamekit:initialize-resources ((app main))
  (gamekit:import-image :snake-head "snake-head.png")
  (gamekit:import-sound :snake-grab "snake-grab.ogg"))

(defmethod gamekit:post-initialize ((app main))
  (gamekit:bind-cursor (lambda (x y)
                         "Move snake head to current cursor position when grabbed"
                         (when *head-grabbed-p*
                           (let ((snake-head (aref *curve* 3)))
                             (setf (gamekit:x snake-head) x)
                             (setf (gamekit:y snake-head) y)))))
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         "Grab snake head"
                         (gamekit:play :snake-grab)
                         (setf *head-grabbed-p* t)))
  (gamekit:bind-button :mouse-left :released
                       (lambda ()
                         "Release snake head"
                         (setf *head-grabbed-p* nil)))
  (gamekit:bind-button :up :repeating
                       (lambda ()
                         (let ((snake-tail (aref *curve* 0)))
                           (incf (gamekit:y snake-tail)
                                 2))))
  (gamekit:bind-button :down :repeating 
                       (lambda ()
                         (let ((snake-tail (aref *curve* 0)))
                           (decf (gamekit:y snake-tail)
                                 2))))
  (gamekit:bind-button :right :repeating
                       (lambda ()
                         (let ((snake-tail (aref *curve* 0)))
                           (incf (gamekit:x snake-tail)
                                 2))))
  (gamekit:bind-button :left :repeating 
                       (lambda ()
                         (let ((snake-tail (aref *curve* 0)))
                           (decf (gamekit:x snake-tail)
                                 2)))))

(defun cur-time-in-secs ()
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (pos abs-time)
  (let* ((frac-of-sec (multiple-value-bind (q r) (truncate abs-time) r))
         (angle (* 2 pi frac-of-sec)))
    (setf (gamekit:y pos) (+ 300 (* 100 (sin angle))))))

(defmethod gamekit:draw ((app main))
  (let ((cur-time (cur-time-in-secs)))
    (gamekit:print-text "A snake it is!" 300 400)
    (update-position (aref *curve* 1) cur-time)
    (update-position (aref *curve* 2) (+ 0.3 cur-time))
    (gamekit:draw-curve (aref *curve* 0)
                        (aref *curve* 3)
                        (aref *curve* 1)
                        (aref *curve* 2)
                        *black*
                        :thickness 5.0)
    (let ((head-img-pos (gamekit:subt (aref *curve* 3)
                                      (gamekit:vec2 32 32))))
      (gamekit:draw-image head-img-pos :snake-head))))

(gamekit:start 'main)
