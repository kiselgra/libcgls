(format #t "Entering ~a.~%" (current-filename))
(include "std.scm")

(define memory-factor 10)

(load "tsm.shader")

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

(define (make-gbuffer w h)
  (letrec ((diffuse-tex  (make-texture-without-file "gbuffer/diff" gl#texture-2d w h gl#rgba gl#rgba8 gl#unsigned-byte))
	   (specular-tex (make-texture-without-file "gbuffer/spec" gl#texture-2d w h gl#rgba gl#rgba8 gl#unsigned-byte))
	   (normal-tex   (make-texture-without-file "gbuffer/norm" gl#texture-2d w h gl#rgba gl#rgba16f gl#unsigned-short))
	   (position-tex (make-texture-without-file "gbuffer/wpos" gl#texture-2d w h gl#rgba gl#rgba32f gl#float))
	   (depth-tex    (make-texture-without-file "gbuffer/depth" gl#texture-2d w h gl#depth-component gl#depth-component32f gl#float))
	   (fbo          (make-framebuffer "gbuffer" w h))
	   (link         (lambda (fun tex pos name)
			   (bind-texture tex pos)
			   (fun fbo name tex))))
    (bind-framebuffer fbo)
    (bind-texture diffuse-tex 0)
    (link attach-texture-as-colorbuffer diffuse-tex  0 "diffuse")
    (link attach-texture-as-colorbuffer specular-tex 1 "specular")
    (link attach-texture-as-colorbuffer normal-tex   2 "normal")
    (link attach-texture-as-colorbuffer position-tex 3 "position")
    (link attach-texture-as-depthbuffer depth-tex    4 "depth" )
    (check-framebuffer-setup fbo)
    (unbind-framebuffer fbo)
    (for-each unbind-texture (list diffuse-tex specular-tex normal-tex position-tex depth-tex))
    ; return our handler
    (lambda (msg . x)
      (case (msg)
	((bind) (bind-framebuffer fbo))
	((unbind) (unbind-framebuffer fbo))
	((tex) (case (car x)
		 ((diffuse)  diffuse-tex)
		 ((specular) specular-tex)
		 ((normal)   normal-tex)
		 ((position) position-tex)
		 ((depth)    depth-tex)
		 (else       #f)))
	((size) (values w h))
	(else #f)))))

(define gbuffer (make-gbuffer x-res y-res))

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

(define (make-mm-shadow-buffers name w h storage-factor)
  (define (setup-shadow-fbo)
    (let ((depth (make-texture-without-file "shadow-depth" gl#texture-2d w h gl#depth-component gl#depth-component32f gl#float))
	  (color (make-texture-without-file "shadow-color" gl#texture-2d w h gl#rgba gl#rgba8 gl#unsigned-byte)) ; for debug visualization
	  (fbo (make-framebuffer "shadow" w h)))
      (bind-framebuffer fbo)
      (bind-texture depth 0)
      (attach-texture-as-colorbuffer fbo (texture-name color) color)
      (attach-texture-as-depthbuffer fbo (texture-name depth) depth)
      (check-framebuffer-setup fbo)
      (unbind-framebuffer fbo)))  
  (setup-shadow-fbo)

  (let ((frag-head-name (string-append name "_" "head_buffer"))
	(frag-tail-name (string-append name "_" "tail_buffer"))
	(frag-depths-name (string-append name "_" "frag_depths"))
	(wh-name (string-append name "_" "buffer_size")))
    ;; buffers for depth array management
    (let ((frag-head (make-texture-without-file frag-head-name gl#texture-2d w h gl#red gl#r32f gl#float))
	  (frag-tail (make-texture-without-file frag-tail-name gl#texture-2d w (* h storage-factor) gl#red gl#r32f gl#float))
	  (frag-depths (make-texture-without-file frag-depths-name gl#texture-2d w (* h storage-factor) gl#red gl#r32f gl#float)))
      ;; buffer ids
      (let ((frag-head-id 0)
	    (frag-tail-id 1)
	    (frag-depths-id 3))
	(let ((handler (lambda (de u loc)
			 (cond ((string=? u frag-head-name) (gl:uniform1i loc frag-head-id))
			       ((string=? u frag-tail-name) (gl:uniform1i loc frag-tail-id))
			       ((string=? u frag-depths-name) (gl:uniform1i loc frag-depths-id))
			       ((string=? u wh-name) (gl:uniform2i loc w h))
			       (else #f))))
	      (short-handler (lambda (de u loc)
			       (cond ((string=? u "head_buffer") (gl:uniform1i loc frag-head-id))
				     ((string=? u "tail_buffer") (gl:uniform1i loc frag-tail-id))
				     ((string=? u "frag_depths") (gl:uniform1i loc frag-depths-id))
				     ((string=? u "wh") (gl:uniform2i loc w h))
				     (else #f))))
	      (bind (lambda (args)
		      (for-each (lambda (arg) (case arg
						((head) (bind-texture-as-image frag-head frag-head-id 0 gl!!read-write gl#r32i))
						((tail) (bind-texture-as-image frag-tail frag-tail-id 0 gl!!read-write gl#r32i))
						((depths) (bind-texture-as-image frag-depths frag-depths-id 0 gl!!read-write gl#r32f))
						(else (throw 'unknown-mm-buffer arg))))
				args)))
	      (unbind (lambda (args)
			(for-each (lambda (arg) (case arg
						  ((head) (unbind-texture-as-image frag-head frag-head-id))
						  ((tail) (unbind-texture-as-image frag-tail frag-tail-id))
						  ((depths) (unbind-texture-as-image frag-depths frag-depths-id))
						  (else (throw 'unknown-mmsm-buffer arg))))
				  args))))
	  (lambda (message . args)
	    (case message
	      ((handle) handler)
	      ((handle-shorthand) short-handler)
	      ((bind) (bind args))
	      ((unbind) (unbind args))
	      ((size) (values w h))
	      (else (throw 'unknown-mm-shadow-message message)))))))))


(define (shaodw-uniform-handler de uniform location)
  (cond ((string=? uniform "shadow_view") (uniform-camera-view-matrix location shadow-cam))
        ((string=? uniform "shadow_proj") (uniform-camera-proj-matrix location shadow-cam))
        ((string=? uniform "shadow_map") (gl:uniform1i location 7))
        (else #f)))
	
(define mmsm (make-mm-shadow-buffers "shadow" 1024 1024 10))
(define (mmsm-handler de u l)
  ((mmsm 'handle) de u l))

(define mouse-x 0)
(define mouse-y 0)
(define level 1)
(define (selection-uniform-handler de u l)
  (receive (w h) (mmsm 'size)
    (cond ((string=? u "xywh") (gl:uniform4i l (floor (* w (/ mouse-x x-res))) (floor (- h (* h (/ mouse-y y-res)))) (1- (expt 2 level)) (1- (expt 2 level))) #t)
	  (else #f))))

;; this one is for debugging, only.
(define atomic-counter (make-atomic-buffer "test" 1 1))
(define copy-of-atomic-buffer #f)


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
			 (memory-barrier!!)
			 (let ((start (glut:time-stamp)))
			   (body)
			   (let* ((stop (glut:time-stamp))
				  (diff (- stop start)))
			     (vector-set! timer tidx diff)
			     (set! tidx (modulo (1+ tidx) 20))))
			 (gl:finish 0)
			 (memory-barrier!!)
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
				 shaodw-uniform-handler
				 mmsm-handler)
(load-scene)
(setup-opaque-rendering)

;; tex textured quad
;; 
(let* ((tqma (make-material "texquad" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad-with-tc "texquad"))
       (tqsh (find-shader "texquad-with-depth"))
       (de (make-drawelement "texquad" tqme tqsh tqma)))
  (prepend-uniform-handler de 'default-material-uniform-handler)
  (prepend-uniform-handler de mmsm-handler)
  (prepend-uniform-handler de selection-uniform-handler)
  (prepend-uniform-handler de lax-tex-uniform-handler))

(let* ((tqma (make-material "texquad/ca" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/clear-array"))
       (tqsh (find-shader "texquad/clear-array"))
       (de (make-drawelement "texquad/ca" tqme tqsh tqma)))
  (prepend-uniform-handler de mmsm-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))
 
(let* ((tqma (make-material "texquad/cc" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/clear-color"))
       (tqsh (find-shader "texquad/clear-color"))
       (de (make-drawelement "texquad/cc" tqme tqsh tqma)))
  (prepend-uniform-handler de mmsm-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))

(let* ((f-mat (make-material "frumat" (make-vec 1 1 0 1) (make-vec 1 1 0 1) (make-vec 0 0 0 1)))
       (f-mesh (make-simple-wire-frustum "frufru" spot-pos spot-dir (make-vec  0.29 0.83 0.48) 25 1 10 5000))
       (f-shader (find-shader "solid-line"))
       (de (make-drawelement "frustum" f-mesh f-shader f-mat)))
  (prepend-uniform-handler de 'default-material-uniform-handler)
  (prepend-uniform-handler de 'default-matrix-uniform-handler)
  )

(defmacro with-viewport (vp . body)
  (let-optional vp (ox oy dx dy)
    `(receive (oldox oldoy olddx olddy) (get-viewport)
       (gl:viewport ,ox ,oy ,dx ,dy)
       (let ((res (begin ,@body)))
         (gl:viewport oldox oldoy olddx olddy)
         res))))

(define gl!!read-only #x88B8)
(define gl!!write-only #x88B9)
(define gl!!read-write #x088ba)

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
		 ;(bind-texture sm 0)
		 ;(save-texture/png sm "shadow.png")
		 ;(unbind-texture sm)
		 (gl:disable gl#polygon-offset-fill)))))

(define clear-shadow-arrays-pass
  (let ((clear-quad-1 (find-drawelement "texquad/clear-array"))
	(clear-quad-2 (find-drawelement "texquad/clear-color")))
    (make-pass "clear shadow arrays" '()
	       (lambda (de) #f)
	       (begin
		 (mmsm 'bind 'head 'tail)
		 (render-drawelement clear-quad-1)
		 (mmsm 'unbind 'head 'tail)
		 (mmsm 'bind 'depths)
		 (render-drawelement clear-quad-1)
		 (mmsm 'unbind 'depths)))))

(define shadow-frag-collector-pass
  (let ((shader (find-shader "shadow-frag-collect")))
    (receive (w h) (mmsm 'size)
      (make-pass "collect shadow fragments" scene-drawelements
		 (lambda (de)
		   shader)
		 (with-viewport (0 0 w h)
	           (reset-atomic-buffer atomic-counter 0)
		   (bind-atomic-buffer atomic-counter 0)
		   (gl:enable gl#polygon-offset-fill)
		   (gl:polygon-offset 1 1)
		   (disable-color-output
		    (disable-depth-output
		     (mmsm 'bind 'depths 'head 'tail)
		     (render-drawelements)
		     (mmsm 'unbind 'depths 'head 'tail)))
		   (gl:disable gl#polygon-offset-fill)
		   (gl:finish 0)
		   (memory-barrier!!)
		   (unbind-atomic-buffer atomic-counter 0)
		   )))))

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
                 (render-drawelement (find-drawelement "frufru"))
		 (unbind-texture shadow-depth)
		 (unbind-framebuffer fbo)
		 ))))

(define shadowcam-image-pass
  (let ((fbo (find-framebuffer "shadow")))
    (make-pass "shadowcam image" scene-drawelements
	       (lambda (de)
		 (find-shader (string-append (shader-name (drawelement-shader de)) "/noshadow")))
	       (begin
		 (bind-framebuffer fbo)
		 (gl:clear-color .1 .3 .6 1)
		 (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
		 (render-drawelements)
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

(define show-shadowcam-image-pass
  (let ((coltex (find-texture "shadow-color"))
	(depthtex (find-texture "shadow-depth"))
	(texquad (find-drawelement "texquad"))
	(counter-vis (find-shader "frag-count-vis")))
    (make-pass "show shadowcam image" '()
	       (lambda (de) #f)
	       (begin
		 (bind-texture coltex 0)
		 (bind-texture depthtex 1)
		 (gl:disable gl#depth-test)
		 (if (eq? render-mode 'shadow-frag-len)
		     (begin
		       (mmsm 'bind 'depths 'head 'tail)
		       (render-drawelement-with-shader texquad counter-vis)
		       (mmsm 'unbind 'depths 'head 'tail))
		     (render-drawelement texquad))
		 (gl:enable gl#depth-test)
		 (unbind-texture depthtex)
		 (unbind-texture coltex)))))

(define show-shadowcam-sort
  (let ((texquad (find-drawelement "texquad"))
	(sort-f (find-shader "sort-shadow-frags")))
    (receive (w h) (mmsm 'size)
      (make-pass "sort fragments (and output some dummy color)" '()
		 (lambda (de) #f)
		 (begin
		   (gl:disable gl#depth-test)
		   (with-viewport (0 0 w h)
		     (mmsm 'bind 'depths 'head 'tail)
		     (render-drawelement-with-shader texquad sort-f)
		     (mmsm 'unbind 'depths 'head 'tail))
		   (gl:enable gl#depth-test))))))

(define show-shadowcam-sort-bla2
  (let ((coltex (find-texture "shadow-color"))
	(depthtex (find-texture "shadow-depth"))
	(texquad (find-drawelement "texquad"))
	(sort-c (find-shader "check-shadow-sort")))
    (receive (w h) (mmsm 'size)
      (make-pass "check fragment ordering" '()
		 (lambda (de) #f)
		 (begin
		   (gl:disable gl#depth-test)
		   (with-viewport (0 0 w h)
		     (mmsm 'bind 'depths 'head 'tail)
		     (render-drawelement-with-shader texquad sort-c)
		     (mmsm 'unbind 'depths 'head 'tail))
		   (gl:enable gl#depth-test))))))
	
(define whole-frame-time 0)
(define number-of-frames 0)
(define (print-pass-timings)
  (for-each (lambda (pass) (format #t "~10,3f ms | ~a~%" (pass 'time) (pass 'name)))
	    all-passes)
  (let ((sum (fold (lambda (p sum) (+ (p 'time) sum)) 0 all-passes)))
    (format #t "--------------+~%~10,3f ms   (~3,3f ms)~%~%" sum (exact->inexact (/ whole-frame-time number-of-frames))))
  (set! whole-frame-time 0)
  (set! number-of-frames 0))


;; the display routine registered with glut
(set-move-factor! (/ (move-factor) 2))

(define render-mode 'default)

(defmacro start-timer ()
  `(set! t-start (glut:time-stamp)))
(defmacro with-timer (end . body)
  `(begin
      (gl:finish 0)
      (memory-barrier!!)
      (let ((timer-start (glut:time-stamp)))
        ,@body
        (gl:finish 0)
        (memory-barrier!!)
        (set! ,end (+ ,end (- (glut:time-stamp) timer-start))))))

(define fps 'not-ready)
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

	   (set! t-frame 0)
	   (with-timer
	     t-frame
	     (check-for-gl-errors "right at the beginning")

;	     (begin   ;; shadow stuff
;	       (use-camera (find-camera "shadowcam"))
;	       (shadow-map-pass 'run)
;	       (clear-shadow-arrays-pass 'run)
;	       (transparent-shadow-list-pass 'run)
;	       (use-camera (find-camera "cam")))
	     (case render-mode
	       ((default)
		(begin
		  (use-camera (find-camera "shadowcam"))
		  (shadow-map-pass 'run)
		  (clear-shadow-arrays-pass 'run)
		  (shadow-frag-collector-pass 'run)
		  (use-camera (find-camera "scene-cam"))
		  (show-shadowcam-sort 'run)
		  
		  ;; generate the base image.
		  (base-image-pass 'run)
		  (show-base-image-pass 'run)
		  ))

	       ((shadowmap shadow-frag-len)
		(begin
		  (use-camera (find-camera "shadowcam"))
		  (shadowcam-image-pass 'run)
		  (show-shadowcam-image-pass 'run)))

	       ((sort-vis sort-vis2)
		(begin
		  (use-camera (find-camera "shadowcam"))
		  (shadow-map-pass 'run)
		  (clear-shadow-arrays-pass 'run)
		  (shadow-frag-collector-pass 'run)
		  (use-camera (find-camera "scene-cam"))
		  (show-shadowcam-sort 'run)
		  (if (eq? render-mode 'sort-vis2)
		      (show-shadowcam-sort-bla2 'run))

		  ))
	       )   ; rendermode

             (glut:swap-buffers))
             (set! whole-frame-time (+ whole-frame-time t-frame))
	     (set! number-of-frames (1+ number-of-frames))
	     )))
  (register-display-function display/glut))


(define (key ch x y)
  (let ((ms (lambda () (format #t "switched render-mode to ~a.~%" render-mode))))
    (case ch
      ((#\1) (set! render-mode 'default) (ms))
      ((#\2) (set! render-mode 'shadowmap) (ms))
      ((#\3) (set! render-mode 'shadow-frag-len) (ms))
      ((#\4) (set! render-mode 'sort-vis) (ms))
      ((#\5) (set! render-mode 'sort-vis2) (ms))
      ((#\+) (set! level (1+ level)))
      ((#\-) (set! level (1- level)))
      (else
        (if (eq? render-mode 'default)
	    (standard-key-function ch x y))))))
(register-key-function key)

(define (momo x y)
  (if (eq? render-mode 'default)
      (standard-mouse-motion-function x y)
      (begin
	(set! mouse-x x)
	(set! mouse-y y))))
(register-mouse-motion-function momo)

(define (reload-shaders)
  (enqueue (load "depth-tsm.shader")))

;; initial gl setup
;;

;; done
;;
(format #t "Leaving ~a.~%" (current-filename))
