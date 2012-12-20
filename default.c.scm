;;; default config for c based viewer.
;;;

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

(load "default.shader")

(let ((home (getenv "HOME")))
  (append-image-path (string-append home "/render-data/images"))
  (append-image-path (string-append home "/render-data/images/wikimedia"))
  (append-image-path (string-append home "/render-data/images/sponza")))

(defmacro cmdline (x)
  `(query-cmdline ',x))

;(define use-graph #f)
(define use-graph #t)

(define the-scene (if use-graph (make-graph-scene "default")
                                (make-scene "default")))

(define (custom-uniform-handler de uniform location)
  (cond ((string=? uniform "light_dir") (gl:uniform3f location 0 -1 -0.2))
        ((string=? uniform "light_col") (gl:uniform3f location 1 .9 .9))
		((string=? uniform "hemi_dir") 
		   (let ((h (cmdline hemi-dir))) 
		 	 (gl:uniform3f location (car h) (cadr h) (caddr h))))
		(else #f)))

(define (make-de name mesh material)
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
    de))

(define (make-de-idx name mesh material pos len)
  (let ((de (make-de name mesh material)))
    (drawelement-index-buffer-range! de pos len)))

(let ((fallback (make-material "fallback" (list 1 0 0 1) (list 1 0 0 1) (list 0 0 0 1))))
  (receive (min max) (if use-graph
                         (load-objfile-and-create-objects-with-single-vbo (cmdline model) (cmdline model) make-de-idx fallback)
                         (load-objfile-and-create-objects-with-separate-vbos (cmdline model) (cmdline model) make-de fallback))
  ;(receive (min max) 
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

(gl:enable gl#depth-test)
(format #t "Leaving ~a.~%" (current-filename))
