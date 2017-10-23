(in-package :rdf)

;; common util functions

(defun cur-time-in-secs ()
  (* (get-internal-real-time) (/ 1.0 internal-time-units-per-second)))

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

;Perez sky color model
; ref: https://nicoschertler.wordpress.com/2013/04/03/simulating-a-days-sky/
(defun sky-color (sun-elevation)
  ;TODO
  )
