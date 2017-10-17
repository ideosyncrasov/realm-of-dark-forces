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
