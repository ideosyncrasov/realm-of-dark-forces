(in-package :rdf)

(defun tile-map-from-file (file-path)
  (let* ((csv-list (cl-csv:read-csv file-path))
         (num-rows (length csv-list))
         (num-cols (length (first csv-list)))
         (arr (make-array (list num-rows num-cols)
                          :initial-contents csv-list)))
    (dotimes (row num-rows arr)
      (dotimes (col num-cols)
        (setf (aref arr row col)
              (read-from-string (aref arr row col)))))))

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
(defvar *tile-width* 16)
(defvar *tile-height* 16)
(defvar *tile-map-width* 16)
(defvar *tile-map-height* 16)
(defvar *tile-map-file* #P"~/common-lisp/realm of dark forces/tileset_16x16_interior/map_deco0.csv")
(defvar *tile-map* (tile-map-from-file *tile-map-file*))

(defclass main (gamekit:gamekit-system) ()
  (:default-initargs
    :resource-path "common-lisp/realm of dark forces/"
    :viewport-width *canvas-width*
    :viewport-height *canvas-height*
    :viewport-title "In the Realm of Dark Forces"))

(defmethod gamekit:initialize-resources ((app main))
  (gamekit:import-image :snake-head "snake-head.png")
  (gamekit:import-image :tile-set "tileset_16x16_interior.png")
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

(defun draw-tile (origin tileset-id tile-num &key (tile-width *tile-width*) (tile-height *tile-height*))
  (when (< tile-num 0)
    return)
  (let* ((tileset-img (gamekit::resource-by-id tileset-id))
         (tileset-height (gamekit::height-of tileset-img))
         (num-rows (/ tileset-height
                      tile-height))
         (tiles-per-row (/ (gamekit::width-of tileset-img)
                           tile-width)))
    (declare (type integer tiles-per-row))
    (multiple-value-bind (row col) (truncate tile-num tiles-per-row)
      (setf row (- num-rows 1 row))
      (draw-image-part origin
                       tileset-id
                       :llx (* col tile-width)
                       :lly (* row tile-height)
                       :width tile-width
                       :height tile-height))))

(defun draw-tile-map (origin tile-map &key tile-set num-rows num-cols (tile-width *tile-width*) (tile-height *tile-height*))
  (dotimes (row num-rows)
    (dotimes (col num-cols)
      (let ((tile-num (aref tile-map row col)))
        (unless (< tile-num 0)
          ;tiles are ordered starting from the upper left corner of the image,
          ;but the y-axis in trivial-gamekit points upwards, therefore we have
          ;to transform (TODO: FIXME)
          (draw-tile (gamekit:add (gamekit:vec2 (* col tile-width)
                                                (* -1 (1+ row) tile-height))
                                  origin)
                     tile-set
                     tile-num
                     :tile-width tile-width
                     :tile-height tile-height))))))

(defmethod gamekit:draw ((app main))
  (let ((cur-time (cur-time-in-secs))
        (img (gamekit::resource-by-id :snake-head)))
    (gamekit:print-text "A snake it is!" 300 400)
    (draw-tile-map (gamekit:vec2 -128 800)
                   *tile-map*
                   :tile-set :tile-set
                   :num-rows 80
                   :num-cols 80)
    (update-position (aref *curve* 1) cur-time)
    (update-position (aref *curve* 2) (+ 0.3 cur-time))
    (draw-image-part (gamekit:vec2 (+ 100 (* 100 (cos cur-time)))
                                   (+ 100 (* 100 (sin cur-time))))
                     :snake-head
                     :llx 10 :lly 10
                     :width 50 :height 50)
    (draw-tile (gamekit:vec2 0 0)
               :tile-set
               33)
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
