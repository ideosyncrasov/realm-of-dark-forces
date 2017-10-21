(in-package :rdf)

;; visualization of game state and animations

(defun draw-game ()
  (let* ((player-character (player-character *game-state*))
         (rounded-player-pos 
           (gamekit:vec2 (truncate (gamekit:x (pos player-character)))
                         (truncate (gamekit:y (pos player-character))))))
    (gamekit:print-text "A snake it is!" 300 400)
    (let ((tile-map *apartment-tile-map*))
      (loop :for layer :across (layers tile-map) :do
            (when (string= (name layer)
                           "character")
              (draw-tile *origin*
                         (aref (tiles *character-tile-set*)
                               (+ (mod (floor (walking-animation-phase player-character))
                                       4)
                                  (case (facing player-character)
                                    (:up (* 2 17))
                                    (:down (* 0 17))
                                    (:left (* 3 17))
                                    (:right (* 1 17)))))))
            (draw-tile-map-layer (gamekit:subt *origin* rounded-player-pos)
                                 :layer layer
                                 :tile-map tile-map)))
    (draw-image-part (gamekit:vec2 (+ 100 (* 100 (cos (time-val *game-state*))))
                                   (+ 100 (* 100 (sin (time-val *game-state*)))))
                     :snake-head
                     :llx 10 :lly 10
                     :width 50 :height 50)))
