(in-package :rdf)

;; visualization of game state and animations

(defvar *rendered-sun-light*)

(defun draw-game ()
  (let* ((player-character (player-character *game-state*))
         (rounded-player-pos 
           (gamekit:vec2 (truncate (gamekit:x (pos player-character)))
                         (truncate (gamekit:y (pos player-character)))))
         (offset (gamekit:subt *center* rounded-player-pos)))
    ;draw background
    (gamekit:draw-rect (gamekit:vec2 0 0)
                       *canvas-width*
                       *canvas-width*
                       :fill-paint
                       (gamekit:vec4 (/ 17 256) (/ 17 256) (/ 17 256) 1))
    ;draw tile map
    (let ((tile-map *apartment-tile-map*))
      (setf *rendered-sun-light*
            (make-array (list (num-rows tile-map)
                              (num-cols tile-map))
                        :initial-element nil))
      (loop :for layer :across (layers tile-map) :do
            (dolist (anim (active-animations *game-state*))
              (and (render-layer anim)
                   (string= (name layer)
                            (render-layer anim))
                   (draw anim *game-state*)))
            (when (string= (name layer)
                           "character")
              (draw-tile *center*
                         (aref (tiles *character-tile-set*)
                               (+ (mod (floor (walking-animation-phase player-character))
                                       4)
                                  (case (facing player-character)
                                    (:up (* 2 17))
                                    (:down (* 0 17))
                                    (:left (* 3 17))
                                    (:right (* 1 17))
                                    (otherwise 0)))))) ;this case never happens
            (unless (string= (name layer)
                             "collision")
              (draw-tile-map-layer (gamekit:add (gamekit:vec2 0 (* (num-rows tile-map)
                                                                   (tile-height tile-map)))
                                                offset)
                                   :layer layer
                                   :tile-map tile-map))))
    ;print UI
    (gamekit:print-text (multiple-value-bind (h m) (truncate (truncate (in-game-time *game-state*))
                                                             60)
                          (format nil "~2,'0D:~2,'0D" h m))
                        300 438 (gamekit:vec4 1 0 0 1))))
