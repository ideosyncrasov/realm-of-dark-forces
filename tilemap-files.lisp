(in-package :rdf)

;; Stuff for loading tilemap files

(defun csv->tile-map (&key csv-file tile-set)
  (let* ((data (cl-csv:read-csv csv-file
                                :map-fn #'(lambda (row)
                                            (mapcar #'read-from-string
                                                    row))))
         (num-rows (length data))
         (num-cols (length (first data)))
         (tiles (make-array (1+ (length (tiles tile-set)))
                            :initial-element nil))
         (data (make-array
                 (list num-rows num-cols)
                 :initial-contents data
                 :element-type 'integer)))
    ;missing tiles are indicated by a tilenum of -1 in the csv.
    ;we therefore remap the tiles of tile-set to the indices
    ; 1, .., (length (tiles (tile-set)))
    ;and index 0 maps to nil.
    (dotimes (i (length (tiles tile-set)))
      (setf (aref tiles (1+ i))
            (aref (tiles tile-set) i)))
    (dotimes (row num-rows)
      (dotimes (col num-cols)
        (incf (aref data row col))))
    (make-instance 'tile-map
                   :tiles tiles
                   :tile-width (tile-width tile-set)
                   :tile-height (tile-height tile-set)
                   :num-rows num-rows
                   :num-cols num-cols
                   :layers
                   (make-array 1
                               :initial-element
                               (make-instance 'tile-map-layer
                                              :name ""
                                              :num-rows num-rows
                                              :num-cols num-cols
                                              :data data)))))

(defun tmx->tile-map (&key tmx-file loaded-tile-sets loaded-images)
  "Given the filename of a .tmx file and a hash-table mapping
   namestrings of truenames of pathnames of .tsx files to tile-set objects,
   and a hash table mapping namestrings of truenames of pathnames of image file
   to resource-ids, returns a new tile-map object representing the given .tmx file.

   If the .tmx file contains references to .tsx files or image file that are not present
   as keys in the file-to-tile-set-dict, it will try to load these .tsx files respectively
   create new tile-set objects out of the resource id corresponding to the image file name,
   and will add them to the file-to-tile-set-dict. In particular, it may alter file-to-tile-set-dict, and it will throw an error if the .tmx file references an image file that hasn't been loaded yet."
  (labels ((first-element-with-tag-name (element tag-name)
             (aref (dom:get-elements-by-tag-name element tag-name)
                   0))
           (value-attribute (element attr-name)
             (read-from-string (dom:get-attribute element attr-name)))
           (string-attribute (element attr-name)
             (dom:get-attribute element attr-name))
           ;returns the tile-set corresponding to tileset-node, and
           ;loads it if necessary (and possible)
           (ensure-tile-set (tileset-node)
             (if (dom:has-attribute tileset-node "source")
                 ;this tileset-node references a .tsx file
                 (let ((tsx-file (namestring
                                   (truename
                                     (merge-pathnames (string-attribute tileset-node "source")
                                                      (directory-namestring tmx-file))))))
                   (multiple-value-bind (value value-exists) (gethash tsx-file loaded-tile-sets)
                     (if value-exists
                         value
                         (setf (gethash tsx-file loaded-tile-sets)
                               (tsx->tile-set
                                 :tsx-file tsx-file
                                 :file-to-id-dict loaded-images)))))
                 ;this tileset-node references an image file
                 (let* ((img-node (first-element-with-tag-name tileset-node "image"))
                        (img-file (namestring
                                    (truename
                                      (merge-pathnames (string-attribute img-node "source")
                                                       (directory-namestring tmx-file))))))

                   (multiple-value-bind (value value-exists) (gethash img-file loaded-tilesets)
                     (if value-exists
                         value
                         (multiple-value-bind (resource-id id-exists) (gethash img-file loaded-images)
                           (assert id-exists (resource-id) "There is no resource-id associated to the file ~A." img-file)
                           (setf (gethash img-file loaded-tile-sets)
                                 (img->tile-set :img-id resource-id
                                                :tile-width (value-attribute img-node "tilewidth")
                                                :tile-height (value-attribute img-node "tileheight")))))))))
           (layer-node->map-layer (node)
             (let ((num-rows (value-attribute node "height"))
                   (num-cols (value-attribute node "width"))
                   (data-node (first-element-with-tag-name node "data")))
               (assert (string= (dom:get-attribute data-node "encoding")
                                "csv"))
               (make-instance 'tile-map-layer
                              :name (string-attribute node "name")
                              :num-rows num-rows
                              :num-cols num-cols 
                              :data (make-array (list num-rows num-cols)
                                                :initial-contents
                                                (cl-csv:read-csv
                                                  (dom:data
                                                    (dom:first-child data-node))
                                                  :map-fn #'(lambda (row)
                                                              (mapcar #'read-from-string row))))))))
    (let* ((document (cxml:parse-file tmx-file
                                      (cxml-dom:make-dom-builder)))
           (map-node (first-element-with-tag-name document "map"))
           (tileset-nodes (dom:get-elements-by-tag-name map-node "tileset"))
           (tilesets (map #'ensure-tile-set
                          tileset-nodes))
           (tilenum-offsets (map #'(lambda (node)
                                     (value-attribute node "firstgid"))
                                 tileset-nodes))
           (num-tilesets (length tilesets))
           (num-tiles
             (loop :for i :below num-tilesets
                   :maximize (+ (aref tilenum-offsets i)
                                (length (aref tilesets i)))))
           (map-tiles (make-array num-tiles
                                  ;missing tiles are indicated by nil
                                  :initial-element nil)))
      ;set up the tiles array of the map
      (dotimes (i num-tilesets)
        (let ((tileset-tiles (tiles (aref tilesets i)))
              (offset (aref tilenum-offsets i)))
          (dotimes (tilenum (length tileset-tiles))
            (setf (aref map-tiles
                        (+ offset tilenum))
                  (aref tileset-tiles
                        tilenum)))))
      ;set up the layers of the map
      (make-instance 'tile-map
                     :tiles map-tiles
                     :tile-width (value-attribute map-node "tilewidth")
                     :tile-height (value-attribute map-node "tileheight")
                     :num-cols (value-attribute map-node "width")
                     :num-rows (value-attribute map-node "height")
        :layers (map #'layer-node->map-layer
                     (dom:get-elements-by-tag-name map-node "layer"))))))
