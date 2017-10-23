(in-package :rdf)

(defvar *asset-dir* (merge-pathnames "assets/" (asdf:system-source-directory :realm-of-dark-forces)))
(defvar *canvas-width* 640)
(defvar *canvas-height* 480)
(defvar *origin* (gamekit:vec2 0 0))
(defvar *center* (gamekit:vec2 (* 0.5 *canvas-width*)
                               (* 0.5 *canvas-height*)))
(defvar *game-state*)
; *truenames-to-resource-ids* maps file names of resources their id, i.e.
;  s |-> x
; if x is the symbol a resource loaded from filename p and s = (namestring (truename p))
(defvar *truenames-to-resource-ids* (make-hash-table :test 'equal))
(defvar *truenames-to-tile-sets* (make-hash-table :test 'equal)); maps names of .tsx files to corresponding tile-set objects


(defvar *character-tile-set-file* (merge-pathnames "character.png" *asset-dir*))
(defvar *character-tile-set*)
(defvar *apartment-tile-map-file* (merge-pathnames "apartment.tmx" *asset-dir*))
(defvar *apartment-tile-map*)
; *truenames-to-resource-ids* maps file names of resources their id, i.e.
;  s |-> x
; if x is the symbol a resource loaded from filename p and s = (namestring (truename p))
(defvar *truenames-to-resource-ids* (make-hash-table :test 'equal))
(defvar *truenames-to-tile-sets* (make-hash-table :test 'equal)); maps names of .tsx files to corresponding tile-set objects

(defclass main (gamekit:gamekit-system) ()
  (:default-initargs
    :resource-path *asset-dir*
    :viewport-width *canvas-width*
    :viewport-height *canvas-height*
    :viewport-title "In the Realm of Dark Forces"))

(defmethod gamekit:initialize-resources ((app main))
  (flet ((import-image (id-symbol filename)
           (gamekit:import-image id-symbol filename)
           (setf (gethash (namestring (truename (merge-pathnames filename *asset-dir*)))
                          *truenames-to-resource-ids*) id-symbol))
         (import-sound (id-symbol filename)
           (gamekit:import-sound id-symbol filename)
           (setf (gethash (namestring (truename (merge-pathnames filename *asset-dir*)))
                          *truenames-to-resource-ids*) id-symbol)))
    (import-image :snake-head "snake-head.png")
    (import-image :interior-tile-set "tileset_16x16_interior.png")
    (import-image :character-tile-set *character-tile-set-file*)
    (import-image :tile-set-inner-zelda "Inner.png")
    (import-sound :alarm-sound "alarm.ogg")))

(defun setup-things ()
  (setf *game-state* (make-instance 'game-state
                                    :time-val (cur-time-in-secs)
                                    :player-character
                                    (make-instance 'animated-game-character
                                                   :pos (gamekit:vec2 368 248)
                                                   :facing :up)))
  (setf *character-tile-set* (img->tile-set :img-id :character-tile-set
                                            :tile-width 16
                                            :tile-height 32))
  (setf *apartment-tile-map*
        (tmx->tile-map
          :tmx-file *apartment-tile-map-file*
          :loaded-tile-sets *truenames-to-tile-sets*
          :loaded-images *truenames-to-resource-ids*)))

(defun key-up-pressed ()
  (setf (facing (player-character *game-state*)) :up)
  (setf (walking-p (player-character *game-state*)) t))

(defun key-up-released ()
  (setf (walking-p (player-character *game-state*)) nil))

(defun key-down-pressed ()
  (setf (facing (player-character *game-state*)) :down)
  (setf (walking-p (player-character *game-state*)) t))

(defun key-down-released ()
  (setf (walking-p (player-character *game-state*)) nil))

(defun key-right-pressed ()
  (setf (facing (player-character *game-state*)) :right)
  (setf (walking-p (player-character *game-state*)) t))

(defun key-right-released ()
  (setf (walking-p (player-character *game-state*)) nil))

(defun key-left-pressed ()
  (setf (facing (player-character *game-state*)) :left)
  (setf (walking-p (player-character *game-state*)) t))

(defun key-left-released ()
  (setf (walking-p (player-character *game-state*)) nil))

(defmethod gamekit:post-initialize ((app main))
  (setup-things)
  (gamekit:bind-button :up :pressed
                       #'key-up-pressed)
  (gamekit:bind-button :up :released
                       #'key-up-released)
  (gamekit:bind-button :down :pressed
                       #'key-down-pressed)
  (gamekit:bind-button :down :released
                       #'key-down-released)
  (gamekit:bind-button :right :pressed
                       #'key-right-pressed)
  (gamekit:bind-button :right :released
                       #'key-right-released)
  (gamekit:bind-button :left :pressed
                       #'key-left-pressed)
  (gamekit:bind-button :left :released
                       #'key-left-released))

(defmethod gamekit:draw ((app main))
  (let* ((cur-time (cur-time-in-secs))
        (dt (- cur-time
               (time-val *game-state*))))
    (update-world *game-state* dt)
    (draw-game)))

(defun main ()
  (gamekit:start 'main))
