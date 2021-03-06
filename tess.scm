(format #t "Entering ~a.~%" (current-filename))

;;; modules
(use-modules (ice-9 receive))

;;; actual code

(define x-res 1)
(define y-res 1)

(receive (x y w h) (get-viewport)
  (format #t "~a x ~a~%" w h)
  (set! x-res w)
  (set! y-res h))

;(let ((cam (make-perspective-camera "cam" (list 0 0 5) (list 0 0 -1) (list 0 1 0) 35 (/ x-res y-res) 1 1000)))
;  (use-camera cam))

;(load "default.shader")
(load "tess.shader")

(let ((home (getenv "HOME")))
  (append-image-path (string-append home "/render-data/images"))
  (append-image-path (string-append home "/render-data/images/wikimedia"))
  (append-image-path (string-append home "/render-data/images/sponza")))

(defmacro cmdline (x)
  `(query-cmdline ',x))

(define the-scene (make-scene "default"))

(define (custom-uniform-handler de uniform location)
  (cond ((string=? uniform "light_dir") (gl:uniform3f location 0 -1 -0.2))
        ((string=? uniform "light_col") (gl:uniform3f location 1 .9 .9))
        ((string=? uniform "tl_inner") (gl:uniform1f location 1))
        ((string=? uniform "tl_outer") (gl:uniform1f location 1))
		((string=? uniform "hemi_dir") 
		   (let ((h (cmdline hemi-dir))) 
		 	 (gl:uniform3f location (car h) (cadr h) (caddr h))))
		(else #f))
	)

(define drawelements '())

(define (testcall name mesh material bbmin bbmax)
  (let* ((shader (if (cmdline hemi)
				     (if (material-has-textures? material)
				         (find-shader "diffuse-hemi+tex")
				         (find-shader "diffuse-hemi"))
					 (if (material-has-textures? material)
					     (find-shader "diffuse-dl+tex")
						 (find-shader "diffuse-dl"))))
		 (de (make-drawelement name mesh shader material)))
    (add-drawelement-to-scene the-scene de)
	(prepend-uniform-handler de 'default-matrix-uniform-handler)
	(prepend-uniform-handler de 'default-material-uniform-handler)
	(prepend-uniform-handler de custom-uniform-handler)
    (set! drawelements (cons de drawelements))
	))

(let ((fallback (make-material "fallback" (list 1 0 0 1) (list 1 0 0 1) (list 0 0 0 1))))
  (receive (min max) (load-objfile-and-create-objects-with-separate-vbos (cmdline model) (cmdline model) testcall fallback)
    (let* ((near 1)
		   (far 1000)
		   (diam (vec-sub max min))
		   (diam/2 (vec-div-by-scalar diam 2))
		   (center (vec-add min diam/2))
		   (distance (vec-length diam))
		   (pos (vec-add center (make-vec 0 0 distance))))
	  (while (> near (/ distance 100))
	    (set! near (/ near 10)))
	  (while (< far (* distance 2))
	    (set! far (* far 2)))
	  (let ((cam (make-perspective-camera "cam" pos (list 0 0 -1) (list 0 1 0) 35 (/ x-res y-res) near far)))
        (use-camera cam))
      (set-move-factor! (/ distance 20)))))

(define r .2)
(define g .4)
(define b .7)

(define gl!line #x1b01)
(define gl!fill #x1b02)
(define gl!patches #x000e)
(define gl!patch-vertices #x8e72)

(define command-queue '())
(defmacro enqueue (cmd)
  `(begin
      (format #t "cmd: .~a.~%" ',cmd)
      (set! command-queue (cons ',cmd command-queue))))
(define (apply-commands)
  (for-each primitive-eval
            (reverse command-queue))
  (set! command-queue '()))

(let* ((qma (make-material "quad" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (qme (make-quad "quad"))
       ;(qsh (find-shader "quad"))
       (qsh (find-shader "quad-tess"))
       (de (make-drawelement "quad" qme qsh qma)))
  (set-mesh-primitive-type! qme gl!patches)
  (gl:patch-parameteri gl!patch-vertices 3)
  (prepend-uniform-handler de 'default-material-uniform-handler)
  (prepend-uniform-handler de custom-uniform-handler)
  (prepend-uniform-handler de 'default-matrix-uniform-handler))

(define (display)
  (gl:clear-color r g b 1)
  (apply-commands)
  (gl:disable gl#cull-face)
  (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
  (for-each render-drawelement
            drawelements)
  (gl:polygon-mode gl#front-and-back gl!line)
  (render-drawelement (find-drawelement "quad"))
  (gl:polygon-mode gl#front-and-back gl!fill)
  (glut:swap-buffers))

(register-display-function display)
(gl:enable gl#depth-test)
(format #t "Leaving ~a.~%" (current-filename))

