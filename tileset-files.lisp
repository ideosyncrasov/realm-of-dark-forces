(in-package :rdf)

;; Stuff for loading tileset files

(defun img->tile-set (&key img-id tile-width tile-height img-width img-height)
  (let* ((img (gamekit::resource-by-id img-id))
         (num-rows (/ (if img-height img-height (gamekit::height-of img))
                      tile-height))
         (num-cols (/ (if img-width img-width (gamekit::width-of img))
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

(defun tsx->tile-set (&key tsx-file file-to-id-dict)
  "Constructs a new tile-set object, given a pathname to a tsx-file and a dictionary mapping truepathnames to resource-ids."
  (let* ((document (cxml:parse-file tsx-file
                                    (cxml-dom:make-dom-builder)))
         (tileset-node (aref (dom:get-elements-by-tag-name document
                                                           "tileset")
                             0))
         (image-node (aref (dom:get-elements-by-tag-name tileset-node
                                                         "image")
                           0))
         (image-file (merge-pathnames (dom:get-attribute image-node "source")
                                      (directory-namestring tsx-file))); relative filenames in .tsx file are relative wrt the position of the .tsx file itself
         (resource-id (gethash (namestring (truename image-file))
                               file-to-id-dict))
         (tilewidth (read-from-string
                      (dom:get-attribute tileset-node "tilewidth")))
         (tileheight (read-from-string
                       (dom:get-attribute tileset-node "tileheight")))
         (num-cols (read-from-string
                     (dom:get-attribute tileset-node "columns")))
         (tilecount (read-from-string
                      (dom:get-attribute tileset-node "tilecount")))
         (num-rows (/ tilecount
                      num-cols))
         (tiles (make-array tilecount :element-type 'tile))
         (tileset (make-instance 'tile-set
                                 :id resource-id
                                 :tile-width tilewidth
                                 :tile-height tileheight
                                 :num-rows num-rows
                                 :num-cols num-cols
                                 :tiles tiles)))
    (dotimes (row num-rows tileset)
      (dotimes (col num-cols)
        (let ((tilenum (+ col (* row num-cols))))
          (setf (aref tiles tilenum)
                (make-instance 'tile
                               :tile-set tileset
                               :tile-num tilenum
                               :llx (* col tilewidth)
                               :lly (* (- num-rows 1 row) tileheight))))))))
