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

(defclass tile-map-layer ()
  ((name
     :initarg :name
     :reader name
     :type string)
   (num-rows
     :initarg :num-rows
     :reader num-rows)
   (num-cols
     :initarg :num-cols
     :reader num-cols)
   (data ;two-dimensional arrays of tilenums
     :initarg :data
     :reader data)))

(defclass tile-map ()
  ((tiles ;array mapping tilenums to tiles (of possibly multiple) tilesets
     :initarg :tiles
     :reader tiles)
   (tile-width
     :initarg :tile-width
     :reader tile-width)
   (tile-height
     :initarg :tile-height
     :reader tile-height)
   (num-rows
     :initarg :num-rows
     :reader num-rows)
   (num-cols
     :initarg :num-cols
     :reader num-cols)
   (layers ;array of layers, ordered from front-to-back, i.e. the first element is in front
     :initarg :layers
     :reader layers)))

(defmethod draw (origin (tile tile))
  (let ((tile-set (tile-set tile)))
    (draw-image-part origin
                     (id tile-set)
                     :llx (llx tile)
                     :lly (lly tile)
                     :width (tile-width tile-set)
                     :height (tile-height tile-set))))

(defmethod draw (origin (tile-map tile-map))
  (let ((tiles (tiles tile-map))
        (tile-width (tile-width tile-map))
        (tile-height (tile-height tile-map)))
    (loop :for layer :across (layers tile-map) :do
          (dotimes (row (num-rows layer))
            (dotimes (col (num-cols layer))
              (let* ((tile (aref tiles
                                 (aref (data layer)
                                       row col))))
                (when tile ;tile is nil if there is no tile in the tile-layer in (row,col)
                  (draw (gamekit:add origin
                                     (gamekit:vec2 (* col tile-width)
                                                   (* -1 (1+ row) tile-height)))
                        tile))))))))
