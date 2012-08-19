(format #t "Entering ~a.~%" (current-filename))

;;; modules
(use-modules (ice-9 receive))

;;; actual code

(define x-res 1)
(define y-res 1)

(receive (x y w h) (viewport)
  (format #t "~a x ~a~%" w h)
  (set! x-res w)
  (set! y-res h))

;(let ((cam (make-perspective-camera "cam" (list 0 0 5) (list 0 0 -1) (list 0 1 0) 35 (/ x-res y-res) 1 1000)))
;  (use-camera cam))

(load "default.shader")

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
		((string=? uniform "hemi_dir") 
		   (let ((h (cmdline hemi-dir))) 
		 	 (gl:uniform3f location (car h) (cadr h) (caddr h))))
		(else #f))
	)

  
(define (testcall name mesh material)
  (format #t "callback with ~a  | ~a  |  ~a~%" name (mesh-name mesh) (material-name material))
  (let* ((shader (if (cmdline hemi)
				     (if (material-has-textures? material)
				         (find-shader "diffuse-hemi+tex")
				         (find-shader "diffuse-hemi"))
					 (if (material-has-textures? material)
					     (find-shader "diffuse-dl+tex")
						 (find-shader "diffuse-dl"))))
		 (de (make-drawelement name mesh shader material)))
    (format #t "shader found: ~a~%" (shader-name shader))
    (add-drawelement-to-scene the-scene de)
	(prepend-uniform-handler de 'default-matrix-uniform-handler)
	(prepend-uniform-handler de 'default-material-uniform-handler)
	(prepend-uniform-handler de custom-uniform-handler)
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
	 (format #t "POS -> ~a~%" pos)
	  (while (> near (/ distance 100))
	    (format #t "Near ~a  -> ~a (~a)~%" near (/ near 10) distance)
	    (set! near (/ near 10)))
	  (while (< far (* distance 2))
	    (set! far (* far 2)))
	  (format #t "n = ~a, f = ~a~%" near far)
	  (let ((cam (make-perspective-camera "cam" pos (list 0 0 -1) (list 0 1 0) 35 (/ x-res y-res) near far)))
        (use-camera cam))
      (set-move-factor! (/ distance 20)))))

(format #t "Leaving ~a.~%" (current-filename))
