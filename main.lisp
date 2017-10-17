(in-package :rdf)

(defvar *pkg-dir* (asdf:system-source-directory :realm-of-dark-forces))
(defvar *canvas-width* 640)
(defvar *canvas-height* 480)
(defvar *last-draw* nil)
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
(defvar *character*)
(defvar *head-grabbed-p* nil)
(defvar *tile-map-file* (merge-pathnames "tileset_16x16_interior/map_deco0.csv" *pkg-dir*))
(defvar *character-tile-set-file* (merge-pathnames "character.png" *pkg-dir*))
(defvar *default-tile-set*)
(defvar *default-tile-map*)
(defvar *character-tile-set*)

(defclass main (gamekit:gamekit-system) ()
  (:default-initargs
    :resource-path "common-lisp/realm of dark forces/"
    :viewport-width *canvas-width*
    :viewport-height *canvas-height*
    :viewport-title "In the Realm of Dark Forces"))

(defmethod gamekit:initialize-resources ((app main))
  (gamekit:import-image :snake-head "snake-head.png")
  (gamekit:import-image :tile-set "tileset_16x16_interior.png")
  (gamekit:import-image :character-tile-set *character-tile-set-file*)
  (gamekit:import-sound :snake-grab "snake-grab.ogg"))

(defun setup-things ()
  (setf *last-draw* nil)
  (setf *default-tile-set* (make-tile-set :img-id :tile-set
                                          :tile-width 16
                                          :tile-height 16))
  (setf *default-tile-map* (csv->tile-map :csv-file *tile-map-file*
                                          :tile-set *default-tile-set*))
  (setf *character-tile-set* (make-tile-set :img-id :character-tile-set
                                            :tile-width 16
                                            :tile-height 32))
  (setf *character* (make-instance 'game-character
                                  :pos (gamekit:vec2 55 55)
                                  :facing :up
                                  :animation-phase 0)))

(defun key-up-pressed ()
  (setf (facing *character*) :up)
  (setf (walking-p *character*) t))

(defun key-up-released ()
  (setf (walking-p *character*) nil))

(defun key-down-pressed ()
  (setf (facing *character*) :down)
  (setf (walking-p *character*) t))

(defun key-down-released ()
  (setf (walking-p *character*) nil))

(defun key-right-pressed ()
  (setf (facing *character*) :right)
  (setf (walking-p *character*) t))

(defun key-right-released ()
  (setf (walking-p *character*) nil))

(defun key-left-pressed ()
  (setf (facing *character*) :left)
  (setf (walking-p *character*) t))

(defun key-left-released ()
  (setf (walking-p *character*) nil))

(defmethod gamekit:post-initialize ((app main))
  (setup-things)
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
  (gamekit:bind-button :up :pressed
                       #'key-up-pressed)
  (gamekit:bind-button :up :released
                       #'key-up-released)
  (gamekit:bind-button :down :pressed
                       #'key-down-pressed)
  (gamekit:bind-button :down :released
                       #'key-down-released)
  (gamekit:bind-button :right :pressed
                       #'key-right-pressed)
  (gamekit:bind-button :right :released
                       #'key-right-released)
  (gamekit:bind-button :left :pressed
                       #'key-left-pressed)
  (gamekit:bind-button :left :released
                       #'key-left-released))

(defun cur-time-in-secs ()
  (* (get-internal-real-time) (/ 1.0 internal-time-units-per-second)))

(defun update-position (pos abs-time)
  (let* ((frac-of-sec (multiple-value-bind (q r) (truncate abs-time) r))
         (angle (* 2 pi frac-of-sec)))
    (setf (gamekit:y pos) (+ 300 (* 100 (sin angle))))))

(defun draw-image-part (origin image-id &key (llx 0) (lly 0) width height)
  (gamekit::when-let ((image (gamekit::resource-by-id image-id)))
    (gamekit::push-canvas)
    (unless width
      (setf width (gamekit::width-of image)))
    (unless height
      (setf height (gamekit::height-of image)))
    (unwind-protect
      (progn
        (gamekit::translate-canvas (- (gamekit:x origin) llx) (- (gamekit:y origin) lly))
        (gamekit:draw-rect (gamekit:vec2 llx lly) width height :fill-paint image))
      (gamekit::pop-canvas))))

(defun update-world (cur-time dt)
  (when (walking-p *character*)
    (setf (animation-phase *character*)
          (mod (+ (animation-phase *character*)
                  (* 4 dt))
               4))
    (case (facing *character*)
      (:up (incf (gamekit:y (pos *character*))
                 (* 64 dt)))
      (:down (decf (gamekit:y (pos *character*))
                   (* 64 dt)))
      (:right (incf (gamekit:x (pos *character*))
                    (* 64 dt)))
      (:left (decf (gamekit:x (pos *character*))
                   (* 64 dt))))))

(defmethod gamekit:draw ((app main))
  (let* ((cur-time (cur-time-in-secs))
         (dt (progn (when (eq *last-draw* nil)
                      (setf *last-draw* cur-time))
                    (- cur-time *last-draw*))))
    (setf *last-draw* cur-time)
    (update-world cur-time dt)
    (gamekit:print-text "A snake it is!" 300 400)
    (draw (gamekit:vec2 -128 800)
          *default-tile-map*)
    (update-position (aref *curve* 1) cur-time)
    (update-position (aref *curve* 2) (+ 0.3 cur-time))
    (draw-image-part (gamekit:vec2 (+ 100 (* 100 (cos cur-time)))
                                   (+ 100 (* 100 (sin cur-time))))
                     :snake-head
                     :llx 10 :lly 10
                     :width 50 :height 50)
    (draw (pos *character*)
          (aref (tiles *character-tile-set*)
                (+ (mod (floor (animation-phase *character*))
                        4)
                   (case (facing *character*)
                     (:up (* 2 17))
                     (:down (* 0 17))
                     (:left (* 3 17))
                     (:right (* 1 17))))))
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
