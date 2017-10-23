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

(defun layer-by-name (tile-map layer-name)
  (find layer-name (layers tile-map) :test #'string= :key #'name))

(defun draw-tile (origin tile)
  (let ((tile-set (tile-set tile)))
    (draw-image-part origin
                     (id tile-set)
                     :llx (llx tile)
                     :lly (lly tile)
                     :width (tile-width tile-set)
                     :height (tile-height tile-set))))

(defvar *rendered-sun-light*)

(defun draw-tile-map-layer (origin &key layer tile-map)
  (let ((tiles (tiles tile-map))
        (tile-width (tile-width tile-map))
        (tile-height (tile-height tile-map)))
    (dotimes (row (num-rows layer))
      (dotimes (col (num-cols layer))
        (let* ((tile (aref tiles
                           (aref (data layer)
                                 row col))))
          (when tile ;tile is nil if there is no tile in the tile-layer in (row,col)
            (draw-tile (gamekit:add origin
                                    (gamekit:vec2 (* col tile-width)
                                                  (* -1 (1+ row) tile-height)))
                       tile)
            (when (or (string= (name layer)
                               "windows")
                      (and (not (aref *rendered-sun-light* row col))
                           (or
                             (string= (name layer)
                                      "wall deco")
                             (string= (name layer)
                                      "inner walls")
                             (string= (name layer)
                                      "windows")
                             (string= (name layer)
                                      "ground & walls")
                             (string= (name layer)
                                      "carpet"))))
              (gamekit:draw-rect (gamekit:add origin
                                              (gamekit:vec2 (* col tile-width)
                                                            (* -1 (1+ row) tile-height)))
                                 (tile-width (tile-set tile))
                                 (tile-height (tile-set tile))
                                 :fill-paint (sun-light *game-state*)
                                 )
              (setf (aref *rendered-sun-light* row col) t))))))))

(defun draw-tile-map (origin tile-map)
  (loop :for layer :across (layers tile-map) :do
        (draw-tile-map-layer origin
                             :layer layer
                             :tile-map tile-map)))
