(in-package :rdf)

;; Stuff for loading tilemap files

(defun csv->tile-map (&key csv-file tile-set)
  (let* ((data (cl-csv:read-csv csv-file
                                :map-fn #'(lambda (row)
                                            (mapcar #'read-from-string
                                                    row))))
         (num-rows (length data))
         (num-cols (length (first data))))
    (make-instance 'tile-map
                   :tile-set tile-set
                   :num-rows num-rows
                   :num-cols num-cols
                   :map-data (make-array (list num-rows num-cols)
                                         :initial-contents data
                                         :element-type 'integer))))

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
                 ;this tileset references a .tsx file
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
                 ;this tileset references an image file
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
                                 (make-tile-set :img-id resource-id
                                                :tile-width (value-attribute img-node "tilewidth")
                                                :tile-height (value-attribute img-node "tileheight"))))))))))
    (let* ((document (cxml:parse-file tmx-file
                                      (cxml-dom:make-dom-builder)))
           (tilesets-and-offsets
             (map #'(lambda (node)
                      (cons (value-attribute node "firstgid")
                            (ensure-tile-set node)))
                  (dom:get-elements-by-tag-name document "tileset")))
           (num-tilesets (length tilesets-and-offsets))
           (num-tiles
             (loop :for i :below num-tilesets
                   :sum (length (tiles (cdr (aref tilesets-and-offsets i))))))
           (tiles (make-array num-tiles)))
      (sort tilesets-and-offsets #'< :key #'car)
      (dotimes (i num-tilesets)
        (let ((tileset (cdr (aref tilesets-and-offsets i)))
              (firstgid (car (aref tilesets-and-offsets i))))
          (dotimes (tilenum (length (tiles (tileset))))
            (setf (aref tiles
                        (+ firstgid tilenum))
                  (aref (tiles tileset)
                        tilenum)))))
      (make-instance 'tile-map
        :tiles (
                ;TODO

                    )
        )
      )

    )
  )
