;;;; Evolution game for LD48
; vim: ts=2 sts=2 sw=2 et ai

(ql:quickload "lispbuilder-sdl")
(ql:quickload "lispbuilder-sdl-gfx")

(defpackage :primordial-soup
  (:use :cl :sdl)
  (:export main))

(in-package :primordial-soup)

(defparameter *map-width* 60)
(defparameter *map-height* 60)

(defparameter *ticks* 0)

(defun get-ticks ()
  (let ((ticks *ticks*))
    (setf *ticks* (sdl-get-ticks))
    (- *ticks* ticks)))

(defun random-element (l)
  (if (endp l)
    nil
    (nth (random (length l)) l)))

(defclass hsv ()
  ((hue :initarg :h :initform 0 :accessor hue)
   (saturation :initarg :s :initform 0 :accessor saturation)
   (value :initarg :v :initform 0 :accessor value)))

(defmethod hsv->rgb ((hsv hsv))
  (with-slots ((h hue) (s saturation) (v value)) hsv
    (let* ((i (floor (* h 6)))
           (f (- (* h 6) i))
           (p (round (* v (- 1 s) 255)))
           (q (round (* v (- 1 (* f s)) 255)))
           (u (round (* v (- 1 (* (- 1 f) s)) 255)))
           (w (round (* v 255))))
    (case (mod i 6)
      (0 (color :r w :g u :b p))
      (1 (color :r q :g w :b p))
      (2 (color :r p :g w :b u))
      (3 (color :r p :g q :b w))
      (4 (color :r u :g p :b w))
      (5 (color :r w :g p :b q))))))

(defclass player ()
  ((energy-cap :initform (+ 1 (random 2)) :accessor energy-cap)
   (colour :initarg :colour :initform nil :accessor colour)
   (mobility :initform 0 :accessor mobility)
   (spawn-rate :initform (+ 0.1 (random 0.5)) :accessor spawn-rate)
   (synthesis :initform 0 :accessor synthesis)
   (predation :initform (random 0.5) :accessor predation)
   (organisms :initform 0 :accessor organisms)))

(defclass organism ()
  ((player :initarg :player :accessor player)
   (energy :accessor energy)
   (coords :initform (point :x 0 :y 0) :accessor coords)
   (time-since-last-spawn :initform 0 :accessor time-since-last-spawn)))

(defclass world ()
  ((cells :initform (make-array `(,*map-width* ,*map-height*)
                                :element-type 'list
                                :initial-element nil)
          :accessor cells)
   (players :initform 0 :accessor players)
   (score :initform 0 :accessor score)
   (level :initform 0 :accessor level)))

(defmethod initialize-instance :after ((player player) &key)
  (when (null (colour player))
    (setf (colour player)
          (make-instance 'hsv :h (random 1.0)
                              :s 1
                              :v 1))))

(defmethod initialize-instance :after ((org organism) &key)
  (setf (energy org) (energy-cap (player org))))

(defun translate-to-screen (coords)
  (point :x (round (* (x coords) 8))
         :y (round (* (y coords) 8))))

(defun text-* (str x y &key (color *white*))
  (sdl-gfx:draw-string-solid-* str x y :color color))

(defun text (str p &key (color *white*))
  (text-* str (x p) (y p) :color color))

(defmethod render ((org organism))
  (let* ((translated (translate-to-screen (coords org)))
         (col (colour (player org))))
    (setf (value col)
          (min 1 (max 0 (/ (energy org) (energy-cap (player org))))))
    (draw-filled-circle translated 4 :color (hsv->rgb col))))

(defmethod move ((org organism) time-delta (world world))
  (declare (ignore world))
  (with-slots ((coords coords)) org
    (when (> (mobility (player org)) 0)
      (let ((vel (* (mobility (player org)) time-delta)))
        (incf (x coords) (* (- (random 2) 1) vel))
        (incf (y coords) (* (- (random 2) 1) vel))))))

(defmethod synth ((org organism) time-delta)
  (with-slots ((energy energy)) org
    (when (and (> (synthesis (player org)) 0)
               (< energy (energy-cap (player org))))
      (incf energy (* (synthesis (player org)) time-delta)))))

(defmethod occupant ((world world) coords)
  (car (aref (cells world) (x coords) (y coords))))

(defmethod adjacent ((world world) coords)
  (let ((x (x coords))
        (y (y coords)))
    (loop for y2 from (- y 1) to (+ y 1)
     append (loop for x2 from (- x 1) to (+ x 1)
             when (and (< 0 x2 *map-width*)
                       (< 0 y2 *map-height*)
                       (not (and (= x x2) (= y y2))))
             collect (point :x x2 :y y2)))))

(defmethod can-occupy ((org organism) coords-list (world world))
  (remove-if-not (lambda (coords)
                   (let ((other (occupant world coords)))
                     (or (null other)
                         (and (not (eql (player org) (player other))) ; no cannibalism
                              (< (random 1.0) (predation (player org)))
                              (> (* (energy org) (predation (player org)))
                                 (* (energy other) (predation (player other))))))))
                 coords-list))

(defmethod clone ((org organism))
  (let ((clone (make-instance 'organism :player (player org))))
    (setf (energy org) (* 0.4 (energy org)))
    clone))

(defmethod add-to-world ((world world) (org organism) coords)
  (let ((x (x coords))
        (y (y coords)))
    (setf (aref (cells world) x y) (list org))
    (setf (x (coords org)) x)
    (setf (y (coords org)) y)
    (incf (organisms (player org)))))

(defmethod spawn ((org organism) time-delta (world world))
  (with-slots ((coords coords)
               (energy energy)
               (waited time-since-last-spawn)) org
    (let ((available-cells (can-occupy org (adjacent world coords) world)))
      (when (and available-cells
                 (> (incf waited time-delta) (spawn-rate (player org)))
                 (> energy (/ (energy-cap (player org)) 3)))
        (let* ((clone (clone org))
               (dest (random-element available-cells))
               (victim (occupant world dest)))
          (when victim
            (incf (energy clone) (* 0.5 (energy victim)))
            (remove-from-world world victim))
          (add-to-world world clone dest))))))

(defmethod burn-rate ((org organism))
  (let ((mobility (mobility (player org)))
        (predation (predation (player org))))
    (+ 0.3 ; BMR
       (* mobility 0.5)
       (* predation 0.8))))

(defmethod update ((org organism) time-delta (world world))
  (move org time-delta world)
  (synth org time-delta)
  (spawn org time-delta world)
  (decf (energy org)
        (* (burn-rate org) time-delta)))

(defmethod remove-from-world ((world world) (org organism))
  (with-slots ((coords coords)) org
    (setf (aref (cells world) (x coords) (y coords)) nil))
    (decf (organisms (player org))))

(defmethod map-world (fn (world world))
  (dotimes (y *map-height*)
    (dotimes (x *map-width*)
      (funcall fn (occupant world (point :x x :y y))))))

(defmethod win-level ((world world) (player player))
  (setf (level world) 0))

(defmethod lose-level ((world world) (player player))
  (setf (level world) 0))

(defmethod update-world ((world world) (player player) time-delta)
  (incf (score world) (organisms player))
  (map-world (lambda (org)
               (when org
                 (if (> (energy org) 0)
                   (update org time-delta world)
                   (remove-from-world world org))))
             world)
  (if (= (organisms player) 0)
    (lose-level world player)
    (let ((survivors (remove-if (lambda (p)
                                  (< (organisms p) 1))
                                (players world))))
      (when (= (length survivors) 1)
          (win-level world player)))))

(defmethod start-next-level ((world world) (player player))
  (with-accessors ((level level) (cells cells) (players players)) world
    (setf cells (make-array `(,*map-width* ,*map-height*)
                            :element-type 'list
                            :initial-element nil))
    (incf level)
    (add-to-world world
                  (make-instance 'organism :player player)
                  (point :x (random *map-width*)
                         :y (random *map-height*)))
    (setf players
          (cons player
                (loop repeat 5
                 collect (let ((enemy (make-instance 'player)))
                           (add-to-world world
                                         (make-instance 'organism :player enemy)
                                         (point :x (random *map-width*)
                                                :y (random *map-height*)))
                           enemy))))
    (setf *ticks* (sdl-get-ticks))))

(defun centre-text (str x y w h &key (color *white*))
  (let ((cx (+ x (- (round (/ w 2)) (round (/ (* (length str) 5) 2)))))
        (cy (+ y (- (round (/ h 2)) 4))))
    (text-* str cx cy :color color)))

(defun bar-chart (&key x y w h (value 0) lo hi)
  (let* ((mn (min lo hi))
         (mx (max lo hi))
         (vl (/ value (- mx mn)))
         (w2 (max 0 (min (round (* w vl)) w))))
    (draw-rectangle-* x y w h :color *white*)
    (draw-box-* x y w2 (- h 1) :color *white*)))

(defmethod render-controls ((player player))
  (centre-text "ENERGY STORE" 480 120 160 8)
  (centre-text "-" 500 136 20 8)
  (centre-text "+" 600 136 20 8)
  (bar-chart :value (energy-cap player)
             :lo 0 :hi 10
             :x 520 :y 136 :w 80 :h 8)
  (centre-text "SPAWN-RATE" 480 152 160 8)
  (centre-text "-" 500 168 20 8)
  (centre-text "+" 600 168 20 8)
  (bar-chart :value (spawn-rate player)
             :lo 0 :hi 1
             :x 520 :y 168 :w 80 :h 8)
  (centre-text "AGGRESSION" 480 184 160 8)
  (centre-text "-" 500 200 20 8)
  (centre-text "+" 600 200 20 8)
  (bar-chart :value (predation player)
             :lo 0 :hi 1
             :x 520 :y 200 :w 80 :h 8))

(defmethod render-world ((world world) (player player) paused)
  (clear-display *black*)
  (centre-text "PRIMORDIAL SOUP" 480 16 160 8)
  (centre-text (format nil "SCORE ~7<~a~>" (score world)) 480 48 160 8)
  (render-controls player)
  (if (= (level world) 0)
    (progn
      (centre-text "Guide the evolution of your organism" 0 120 480 8)
      (centre-text "[P]lay / [P]ause" 0 240 480 240)
      (centre-text "[Q]uit game" 0 256 480 240))
    (progn
      (map-world (lambda (org)
                   (when org
                     (render org)))
                 world)
      (when paused
        (draw-box-* 0 0 480 480 :color (color :r 0 :g 0 :b 0 :a 128))
        (draw-box-* 205 228 71 24 :color *black*)
        (text-* "P A U S E D" 213 236)))))

(defun main ()
  (with-init ()
    (window 640 480
            :title-caption "Primordial Soup"
            :icon-caption "Primordial Soup")
    (sdl-gfx:initialise-default-font sdl-gfx:*font-5x8*)
    (setf (frame-rate) 60)
    (clear-display *black*)
    (let* ((player (make-instance 'player
                                  :colour (make-instance 'hsv :h 0 :s 0 :v 1)))
           (world (make-instance 'world))
           (paused nil))
      (with-events ()
        (:quit-event () t)
        (:mouse-button-down-event (:x x :y y)
          (when (and (< 500 x 520)
                     (< 132 y 158)
                     (> (energy-cap player) 1))
            (decf (energy-cap player)))
          (when (and (< 600 x 620)
                     (< 132 y 158)
                     (< (energy-cap player) 10))
            (incf (energy-cap player)))
          (when (and (< 500 x 520)
                     (< 164 y 180)
                     (> (spawn-rate player) 0.1))
            (decf (spawn-rate player) 0.1))
          (when (and (< 600 x 620)
                     (< 164 y 180)
                     (< (spawn-rate player) 0.9))
            (incf (spawn-rate player) 0.1))
          (when (and (< 500 x 520)
                     (< 196 y 212)
                     (> (predation player) 0.1))
            (decf (predation player) 0.1))
          (when (and (< 600 x 620)
                     (< 196 y 212)
                     (< (predation player) 0.9))
            (incf (predation player) 0.1)))
        (:key-up-event (:key key)
          (case key
            (:sdl-key-escape (push-quit-event))
            (:sdl-key-q (setf (level world) 0))
            (:sdl-key-p (if (= (level world) 0)
                          (progn
                            (setf (score world) 0)
                            (setf *ticks* (sdl-get-ticks))
                            (start-next-level world player))
                          (setf paused (not paused))))))
        (:idle ()
          (when (and (> (level world) 0)
                     (not paused))
            (update-world world player (* (get-ticks) 0.001)))
          (render-world world player paused)
          (update-display))))))
