(format #t "Entering ~a.~%" (current-filename))
(include "std.scm")

(define memory-factor 10)

(load "poster.shader")

(let ((home (getenv "HOME")))
  (append-image-path (string-append home "/render-data/images"))
  (append-image-path (string-append home "/render-data/images/wikimedia"))
  (append-image-path (string-append home "/render-data/images/sponza")))

(defmacro cmdline (x)
  `(query-cmdline ',x))

(define the-scene (make-scene "default"))

(define spot-follow-cam #f)
(define spot-pos (make-vec -908 330 -256))
(define spot-dir (make-vec 0.3 -0.55 .78))
(define shadow-cam (make-perspective-camera "shadowcam" spot-pos spot-dir (make-vec  0.29 0.83 0.48) 25 1 1 5000));16384))

(define gl#compare-r-to-texture gl#compare-ref-to-texture)

;; scene handling
(define (light-uniform-handler de uniform location)
  (cond ((string=? uniform "spot_pos") (gl:uniform3f location (vec-x spot-pos) (vec-y spot-pos) (vec-z spot-pos)))
        ((string=? uniform "spot_dir") (gl:uniform3f location (vec-x spot-dir) (vec-y spot-dir) (vec-z spot-dir)))
        ((string=? uniform "spot_col") (gl:uniform3f location 1 .9 .9))
	((string=? uniform "spot_cutoff") (gl:uniform1f location (* 3.1416 25 1/180)))
	((string=? uniform "light_col") (gl:uniform3f location .4 .4 .4));(gl:uniform3f location .15 .1 .1)) ;(gl:uniform3f location .2 .2 .3))
        ((string=? uniform "hemi_dir") 
           (let ((h (cmdline hemi-dir))) 
              (gl:uniform3f location (car h) (cadr h) (caddr h))))
        (else #f)))

(define (lax-tex-uniform-handler de uniform location)
  (cond ((string=? "tex0" uniform) (gl:uniform1i location 0) #t)
	((string=? "tex1" uniform) (gl:uniform1i location 1) #t)
	((string=? "tex2" uniform) (gl:uniform1i location 2) #t)
	((string=? "tex3" uniform) (gl:uniform1i location 3) #t)
	((string=? "tex4" uniform) (gl:uniform1i location 4) #t)
	(else #f)))

(define (setup-shadow-fbo w h)
  (let ((depth (make-texture-without-file "shadow-depth" gl#texture-2d w h gl#depth-component gl#depth-component32f gl#float))
	(fbo (make-framebuffer "shadow" w h)))
    (bind-framebuffer fbo)
    (bind-texture depth 0)
    (attach-texture-as-depthbuffer fbo (texture-name depth) depth)
    (check-framebuffer-setup fbo)
    (unbind-framebuffer fbo)))  
(setup-shadow-fbo 1024 1024)

(define (shaodw-uniform-handler de uniform location)
  (cond ((string=? uniform "shadow_view") (uniform-camera-view-matrix location shadow-cam))
        ((string=? uniform "shadow_proj") (uniform-camera-proj-matrix location shadow-cam))
        ((string=? uniform "shadow_map") (gl:uniform1i location 7))
        (else #f)))
	
(define mouse-x 0)
(define mouse-y 0)

(define verbose-passes #f)
(defmacro make-pass (name drawelements classification . body)
  `(let* ((shaders '())
	  (drawelements ,drawelements)
	  (render-drawelements (lambda* (:optional code) (for-each (lambda (de sh)
								     (when sh
								       (when code (code de sh))
								       (render-drawelement-with-shader de sh)))
								   drawelements shaders)))
	  (classify (lambda () (set! shaders (map ,classification drawelements))))
	  (body (lambda () ,@body))
	  (name ,name)
	  (timer (make-vector 20 0))
	  (tidx 0))
     (classify)
     (let ((handler (lambda (message . args)
		      (case message
			((run) (check-for-gl-errors (string-append "before " ,name))
			 (if verbose-passes (format #t "--> ~a~%" name))
			 (gl:finish 0)
			 (let ((start (glut:time-stamp)))
			   (body)
			   (let* ((stop (glut:time-stamp))
				  (diff (- stop start)))
			     (vector-set! timer tidx diff)
			     (set! tidx (modulo (1+ tidx) 20))))
			 (gl:finish 0)
			 (check-for-gl-errors (string-append "after " ,name)))
			((reset-drawelements!) (set! drawelements (car args)) (classify))
			((time) (/ (reduce + 0 (vector->list timer)) 20))
			((name) name)
			(else (throw 'invalid-pass-message message args))))))
       (set! all-passes (append all-passes (list handler)))  ; we want to keep the order.
       handler)))
	 
(define (setup-opaque-rendering)
  (let ((depth (make-texture-without-file "colorbuffer-depth" gl#texture-2d x-res y-res gl#depth-component gl#depth-component32f gl#float))
        (color (make-texture-without-file "colorbuffer-color" gl#texture-2d x-res y-res gl#rgba gl#rgba8 gl#unsigned-byte))
        (fbo (make-framebuffer "colorbuffer" x-res y-res)))
    (bind-framebuffer fbo)
    (bind-texture color 0)
    (attach-texture-as-colorbuffer fbo (texture-name color) color)
    (bind-texture depth 1)
    (attach-texture-as-depthbuffer fbo (texture-name depth) depth)
    (check-framebuffer-setup fbo)
    (unbind-framebuffer fbo)))

 
(load "sceneloader.scm")
(register-scene-uniform-handlers light-uniform-handler
				 shaodw-uniform-handler)
(load-scene)
(setup-opaque-rendering)

;; tex textured quad
;; 
(let* ((tqma (make-material "texquad" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad-with-tc "texquad"))
       (tqsh (find-shader "texquad-with-depth"))
       (de (make-drawelement "texquad" tqme tqsh tqma)))
  (prepend-uniform-handler de 'default-material-uniform-handler)
  (prepend-uniform-handler de lax-tex-uniform-handler))

(defmacro with-viewport (vp . body)
  (let-optional vp (ox oy dx dy)
    `(receive (oldox oldoy olddx olddy) (get-viewport)
       (gl:viewport ,ox ,oy ,dx ,dy)
       (let ((res (begin ,@body)))
         (gl:viewport oldox oldoy olddx olddy)
         res))))

(define all-passes '())

(define shadow-map-pass
  (let ((shader (find-shader "render-shadowmap")))
    (make-pass "shadow map generation" scene-drawelements
	       (lambda (de)
		 shader)
	       (let ((fbo (find-framebuffer "shadow"))
		     (sm (find-texture "shadow-depth")))
		 (bind-framebuffer fbo)
		 (gl:clear gl#depth-buffer-bit)
		 (gl:enable gl#polygon-offset-fill)
		 (gl:polygon-offset 1 1)
		 (disable-color-output
		   (render-drawelements))
		 (unbind-framebuffer fbo)
		 (gl:disable gl#polygon-offset-fill)))))

(define base-image-pass
  (let ((fbo (find-framebuffer "colorbuffer"))
	(shadow-depth (find-texture "shadow-depth")))
    (make-pass "base image" scene-drawelements
	       (lambda (de)
		 (drawelement-shader de))
	       (begin
		 (bind-framebuffer fbo)
		 (gl:clear-color .1 .3 .6 1)
		 (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
		 (bind-texture shadow-depth 7)
		 (render-drawelements)
		 (unbind-texture shadow-depth)
		 (unbind-framebuffer fbo)
		 ))))

(define show-base-image-pass
  (let ((coltex (find-texture "colorbuffer-color"))
	(depthtex (find-texture "colorbuffer-depth"))
	(texquad (find-drawelement "texquad")))
    (make-pass "show base image" '()
	       (lambda (de) #f)
	       (begin
		 (bind-texture coltex 0)
		 (bind-texture depthtex 1)
		 (gl:disable gl#depth-test)
		 (render-drawelement texquad)
		 (gl:enable gl#depth-test)
		 (unbind-texture depthtex)
		 (unbind-texture coltex)))))

(define whole-frame-time 0)
(define number-of-frames 0)
(define (print-pass-timings)
  (for-each (lambda (pass) (format #t "~10,3f ms | ~a~%" (pass 'time) (pass 'name)))
	    all-passes)
  (let ((sum (fold (lambda (p sum) (+ (p 'time) sum)) 0 all-passes)))
    (format #t "--------------+~%~10,3f ms   (~3,3f ms)~%~%" sum (exact->inexact (/ whole-frame-time number-of-frames))))
  (set! whole-frame-time 0)
  (set! number-of-frames 0))

(define print-timings #t)

(let* (; the opaque render target
       (fbo (find-framebuffer "opaque"))
       (coltex (find-texture "colorbuffer-color"))
       (depthtex (find-texture "colorbuffer-depth"))
       (shadow-depth (find-texture "shadow-depth"))
       ; timer-stuff
       (t-frame 0)
       (print-timer (glut:time-stamp))
       (print-timings 
        (lambda ()
          (when (and (> (- (glut:time-stamp) print-timer) 10000)
                     print-timings)
	    (print-pass-timings)
            (set! print-timer (glut:time-stamp)))))
       ; the actual display function
       (display/glut 
	(lambda ()
	  (apply-commands)
	  (print-timings)

	  (when spot-follow-cam
	    (set! spot-pos (vec-add (cam-pos (current-camera)) (make-vec 4 0 2)))
	    (set! spot-dir (cam-dir (current-camera))))

	  (with-viewport (0 0 10 10)
			 (let ((x 1)) x)
			 )

	  (check-for-gl-errors "right at the beginning")

	  (begin
	    (use-camera (find-camera "shadowcam"))
	    (shadow-map-pass 'run)
	    (use-camera (find-camera "scene-cam"))
	    
	    ;; generate the base image.
	    (base-image-pass 'run)
	    (show-base-image-pass 'run)
	    )

	  (glut:swap-buffers)
	  (set! whole-frame-time (+ whole-frame-time t-frame))
	  (set! number-of-frames (1+ number-of-frames)))))
  (register-display-function display/glut))


(define (key ch x y)
  (standard-key-function ch x y))
(register-key-function key)

(define (momo x y)
  (standard-mouse-motion-function x y)
  (begin
    (set! mouse-x x)
    (set! mouse-y y)))
(register-mouse-motion-function momo)

(define (reload-shaders)
  (enqueue (load "depth-tsm.shader")))

;; initial gl setup
;;

;; done
;;
(format #t "Leaving ~a.~%" (current-filename))
