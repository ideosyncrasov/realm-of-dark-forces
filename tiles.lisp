(in-package :rdf)

(defclass tile-set ()
  ((resource-id
     :initarg :id
     :reader id)
  (tile-width
    :initarg :tile-width
    :reader tile-width
    :type integer)
  (tile-height
    :initarg :tile-height
    :reader tile-height
    :type integer)
  (num-rows
    :initarg :num-rows
    :reader num-rows
    :type integer)
  (num-cols
    :initarg :num-cols
    :reader num-cols
    :type integer)
  (tiles
    :initarg :tiles
    :reader tiles)))

(defclass tile ()
  ((tile-set
     :initarg :tile-set
     :reader tile-set
     :type tile-set)
   (tile-num
     :initarg :tile-num
     :reader tile-num
     :type integer)
   (llx
     :initarg :llx
     :reader llx)
   (lly
     :initarg :lly
     :reader lly)))

(defclass tile-map ()
  ((tile-set
     :initarg :tile-set
     :reader tile-set
     :type tile-set)
   (num-rows
     :initarg :num-rows
     :reader num-rows)
   (num-cols
     :initarg :num-cols
     :reader num-cols)
   (map-data
     :initarg :map-data
     :reader map-data)))

(defun make-tile-set (&key img-id tile-width tile-height)
  (let* ((img (gamekit::resource-by-id img-id))
         (num-rows (/ (gamekit::height-of img)
                      tile-height))
         (num-cols (/ (gamekit::width-of img)
                      tile-width))
         (tiles (make-array (* num-rows num-cols)))
         (tile-set (make-instance 'tile-set
                                  :id img-id
                                  :tile-width tile-width
                                  :tile-height tile-height
                                  :num-rows num-rows
                                  :num-cols num-cols
                                  :tiles tiles)))
    (dotimes (row num-rows tile-set)
      (dotimes (col num-cols)
        (let ((tile-num (+ col (* num-cols row))))
          (setf (aref tiles tile-num)
                (make-instance 'tile
                               :tile-set tile-set
                               :tile-num tile-num
                               :llx (* col tile-width)
                               :lly (* (- num-rows 1 row) tile-height))))))))

(defmethod draw (origin (tile tile))
  (let ((tile-set (tile-set tile)))
    (draw-image-part origin
                     (id tile-set)
                     :llx (llx tile)
                     :lly (lly tile)
                     :width (tile-width tile-set)
                     :height (tile-height tile-set))))

(defmethod draw (origin (tile-map tile-map))
  (let* ((tile-set (tile-set tile-map))
         (tiles (tiles tile-set))
         (tile-width (tile-width tile-set))
         (tile-height (tile-height tile-set)))
    (dotimes (row (num-rows tile-map))
      (dotimes (col (num-cols tile-map))
        (let ((tile-num (aref (map-data tile-map) row col)))
          (unless (< tile-num 0)
            (draw (gamekit:add origin
                               (gamekit:vec2 (* col tile-width)
                                             (* -1 (1+ row) tile-height)))
                  (aref tiles tile-num))))))))
