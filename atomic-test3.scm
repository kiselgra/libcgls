(format #t "Entering ~a.~%" (current-filename))

;;; modules
(use-modules (ice-9 receive))
(use-modules (rnrs bytevectors))

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

(define drawelements '())

(make-texture-without-file "frag_mutex" gl#texture-2d x-res y-res gl#red gl#r32f gl#float)
(make-texture-without-file "frag_arrays" gl#texture-2d x-res (* y-res 1) gl#rgba gl#rgba32f gl#float)   ; 32 slots per pixel

(define (atomic-buffer-handler de u l)
  (cond ((string=? u "mutex_buffer") (gl:uniform1i l 0))
        ((string=? u "per_frag_array") (gl:uniform1i l 1))
        (else #f)))


(define (testcall name mesh material)
  (let* ((shader (if (cmdline hemi)
				     (if (material-has-textures? material)
				         (find-shader "diffuse-hemi+tex")
				         (find-shader "diffuse-hemi/frag-arrays"))
					 (if (material-has-textures? material)
					     (find-shader "diffuse-dl+tex")
						 (find-shader "diffuse-dl"))))
		 (de (make-drawelement name mesh shader material)))
    (add-drawelement-to-scene the-scene de)
	(prepend-uniform-handler de 'default-matrix-uniform-handler)
	(prepend-uniform-handler de 'default-material-uniform-handler)
	(prepend-uniform-handler de custom-uniform-handler)
	(prepend-uniform-handler de atomic-buffer-handler)
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
	  (while (< far (* distance 4))
	    (set! far (* far 2)))
	  (let ((cam (make-perspective-camera "cam" pos (list 0 0 -1) (list 0 1 0) 35 (/ x-res y-res) near far)))
        (use-camera cam))
      (set-move-factor! (/ distance 20)))))

;; tex textured quad
(let* ((tqma (make-material "atquad" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad-with-tc "atquad"))
       (tqsh (find-shader "quad/show-frag-array-len"))
       (de (make-drawelement "atquad" tqme tqsh tqma)))
  (material-add-texture tqma (find-texture "frag_mutex"))
  (prepend-uniform-handler de 'default-material-uniform-handler)
  (prepend-uniform-handler de atomic-buffer-handler))

(define atomic-counter (make-atomic-buffer "test" 1 1))
(define copy-of-atomic-buffer #f)

(define r .2)
(define g .3)
(define b .8)

(define command-queue '())
(defmacro enqueue (cmd)
  `(begin
      (format #t "cmd: .~a.~%" ',cmd)
      (set! command-queue (cons ',cmd command-queue))))
(define (apply-commands)
  (for-each primitive-eval
            (reverse command-queue))
  (set! command-queue '()))

(define (display)
  (gl:clear-color r g b 1)
  (apply-commands)
  (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
  (reset-atomic-buffer atomic-counter 0)
  (bind-atomic-buffer atomic-counter 0)
  (bind-texture-as-image (find-texture "frag_mutex") 0 0 #x88ba gl#r32i)
  (bind-texture-as-image (find-texture "frag_arrays") 1 0 #x88ba gl#r32i)
  (gl:disable gl#depth-test)
  (gl:color-mask gl#false gl#false gl#false gl#false)
  (gl:depth-mask gl#false)
  (for-each render-drawelement
            drawelements)
  (gl:color-mask gl#true gl#true gl#true gl#true)
  (gl:depth-mask gl#true)
  (gl:finish 0)
  (render-drawelement (find-drawelement "atquad"))
  (unbind-texture-as-image (find-texture "frag_mutex") 0)
  (unbind-atomic-buffer atomic-counter 0)
  (gl:finish 0) ;; bug in wrapper/gen -> glFinish(void);
  (set! copy-of-atomic-buffer (read-atomic-buffer atomic-counter))
  (format #t "bv0: ~a~%" (bytevector-s32-native-ref copy-of-atomic-buffer 0))
  (glut:swap-buffers))

(register-display-function display)
(gl:enable gl#depth-test)

(format #t "Leaving ~a.~%" (current-filename))
