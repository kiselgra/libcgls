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

(define (setup-shadow-fbo)
  (let ((depth (make-texture-without-file "shadow-depth" gl#texture-2d 512 512 gl#depth-component gl#depth-component32f gl#float))
	(color (make-texture-without-file "shadow-color" gl#texture-2d 512 512 gl#rgba gl#rgba8 gl#unsigned-byte)) ; for debug visualization
        (fbo (make-framebuffer "shadow" 512 512)))
    (bind-framebuffer fbo)
    (bind-texture depth 0)
    (attach-texture-as-colorbuffer fbo (texture-name color) color)
    (attach-texture-as-depthbuffer fbo (texture-name depth) depth)
    (check-framebuffer-setup fbo)
    (unbind-framebuffer fbo)))
(setup-shadow-fbo)

(define (shaodw-uniform-handler de uniform location)
  (cond ((string=? uniform "shadow_view") (uniform-camera-view-matrix location shadow-cam))
        ((string=? uniform "shadow_proj") (uniform-camera-proj-matrix location shadow-cam))
        ((string=? uniform "shadow_map") (gl:uniform1i location 7))
        (else #f)))

;; scene handling
(define (custom-uniform-handler de uniform location)
  (cond ((string=? uniform "spot_pos") (gl:uniform3f location (vec-x spot-pos) (vec-y spot-pos) (vec-z spot-pos)))
        ((string=? uniform "spot_dir") (gl:uniform3f location (vec-x spot-dir) (vec-y spot-dir) (vec-z spot-dir)))
        ((string=? uniform "spot_col") (gl:uniform3f location 1 .9 .9))
	((string=? uniform "spot_cutoff") (gl:uniform1f location (* 3.1416 25 1/180)))
	((string=? uniform "light_col") (gl:uniform3f location .4 .4 .4));(gl:uniform3f location .15 .1 .1)) ;(gl:uniform3f location .2 .2 .3))
        ((string=? uniform "hemi_dir") 
           (let ((h (cmdline hemi-dir))) 
              (gl:uniform3f location (car h) (cadr h) (caddr h))))
        (else #f)))

(define (depth-uniform-handler de uniform location)
  (cond ((string=? "depth" uniform)
         (gl:uniform1i location (material-number-of-textures (drawelement-material de))) #t)
        (else #f)))

(define (lax-tex-uniform-handler de uniform location)
  (cond ((string=? "tex0" uniform) (gl:uniform1i location 0) #t)
	((string=? "tex1" uniform) (gl:uniform1i location 1) #t)
	((string=? "tex2" uniform) (gl:uniform1i location 2) #t)
	((string=? "tex3" uniform) (gl:uniform1i location 3) #t)
	((string=? "tex4" uniform) (gl:uniform1i location 4) #t)
	(else #f)))

(define (make-oit-buffers name w h storage-factor)
  ;; buffer names
  (let ((frag-head-name (string-append name "_" "head_buffer"))
	(frag-tail-name (string-append name "_" "tail_buffer"))
	(frag-colors-name (string-append name "_" "frag_colors"))
	(frag-depths-name (string-append name "_" "frag_depths"))
	(opaque-name (string-append name "_" "opaque_depth"))
	(wh-name (string-append name "_" "buffer_size")))
    ;; buffers for depth array management
    (let ((frag-head (make-texture-without-file frag-head-name gl#texture-2d w h gl#red gl#r32f gl#float))
	  (frag-tail (make-texture-without-file frag-tail-name gl#texture-2d w (* h storage-factor) gl#red gl#r32f gl#float))
	  (frag-colors (make-texture-without-file frag-colors-name gl#texture-2d w (* h storage-factor) gl#rgba gl#rgba8 gl#unsigned-byte))
	  (frag-depths (make-texture-without-file frag-depths-name gl#texture-2d w (* h storage-factor) gl#red gl#r32f gl#float)))
      ;; buffer ids
      (let ((frag-head-id 0)
	    (frag-tail-id 1)
	    (frag-colors-id 2)
	    (frag-depths-id 3))
	(let ((handler (lambda (de u loc)
			 (cond ((string=? u frag-head-name) (gl:uniform1i loc frag-head-id))
			       ((string=? u frag-tail-name) (gl:uniform1i loc frag-tail-id))
			       ((string=? u frag-colors-name) (gl:uniform1i loc frag-colors-id))
			       ((string=? u frag-depths-name) (gl:uniform1i loc frag-depths-id))
			       ((string=? u opaque-name) (gl:uniform1i loc (material-number-of-textures (drawelement-material de))))
			       ((string=? u wh-name) (gl:uniform2i loc w h))
			       (else #f))))
	      (short-handler (lambda (de u loc)
			       (cond ((string=? u "head_buffer") (gl:uniform1i loc frag-head-id))
				     ((string=? u "tail_buffer") (gl:uniform1i loc frag-tail-id))
				     ((string=? u "frag_colors") (gl:uniform1i loc frag-colors-id))
				     ((string=? u "frag_depths") (gl:uniform1i loc frag-depths-id))
				     ((string=? u "wh") (gl:uniform2i loc w h))
				     (else #f))))
	      (bind (lambda (args)
		      (for-each (lambda (arg) (case arg
						((head) (bind-texture-as-image frag-head frag-head-id 0 gl!!read-write gl#r32i))
						((tail) (bind-texture-as-image frag-tail frag-tail-id 0 gl!!read-write gl#r32i))
						((colors) (bind-texture-as-image frag-colors frag-colors-id 0 gl!!read-write gl#rgba8))
						((depths) (bind-texture-as-image frag-depths frag-depths-id 0 gl!!read-write gl#r32f))
						(else (throw 'unknown-oit-buffer arg))))
				args)))
	      (unbind (lambda (args)
			(for-each (lambda (arg) (case arg
						  ((head) (unbind-texture-as-image frag-head frag-head-id))
						  ((tail) (unbind-texture-as-image frag-tail frag-tail-id))
						  ((colors) (unbind-texture-as-image frag-colors frag-colors-id))
						  ((depths) (unbind-texture-as-image frag-depths frag-depths-id))
						  (else (throw 'unknown-oit-buffer arg))))
				  args))))
	  (lambda (message . args)
	    (case message
	      ((handle) handler)
	      ((handle-shorthand) short-handler)
	      ((bind) (bind args))
	      ((unbind) (unbind args))
	      (else (throw 'unknown-oit-message message)))))))))
			 
(define cam-oit (make-oit-buffers "cam" x-res y-res 10))
(define shadow-oit (make-oit-buffers "shadow" 512 512 10))

(define (atomic-buffer-handler de u l)
  (let ((handled (if (= (current-camera) shadow-cam)
		     ((shadow-oit 'handle-shorthand) de u l)
		     ((cam-oit 'handle-shorthand) de u l))))
    (cond (handled #t)
	  (((cam-oit 'handle) de u l) #t)
	  (((shadow-oit 'handle) de u l) #t)
	  (else #f))))

(define (make-mm-shadow-buffers name w h storage-factor)
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
						  (else (throw 'unknown-oit-buffer arg))))
				  args))))
	  (lambda (message . args)
	    (case message
	      ((handle) handler)
	      ((handle-shorthand) short-handler)
	      ((bind) (bind args))
	      ((unbind) (unbind args))
	      (else (throw 'unknown-mm-shadow-message message)))))))))
	
(define mmsm (make-mm-shadow-buffers "shadow" 512 512 10))
(define (mmsm-handler de u l)
  ((mmsm 'handle) de u l))

(define mouse-x 0)
(define mouse-y 0)
(define level 1)
(define (selection-uniform-handler de u l)
  (cond ((string=? u "xywh") (gl:uniform4i l (floor (* 512 (/ mouse-x x-res))) (floor (- 512 (* 512 (/ mouse-y y-res)))) (1- (expt 2 level)) (1- (expt 2 level))) #t)
	(else #f)))

;; this one is for debugging, only.
(define atomic-counter (make-atomic-buffer "test" 1 1))
(define copy-of-atomic-buffer #f)

;; scene loading
(define drawelements '())
(define drawelement-shaders '())

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
	 
(define (is-transparent de)
  (let* ((material (drawelement-material de))
	 (diffuse (material-diffuse-color material)))
    (cond ((and (< (vec-a diffuse) 1) (> (vec-a diffuse) 0)) #t)
	  ((string-contains (material-name material) "fabric")
	   (set-material-diffuse-color! material (make-vec 1 1 1 .3))
	   #t)
	  (else #f))))

(define (setup-scene)
  (define use-dragon #f)
  (define (create-drawelement name mesh material)
    (let* ((shader (if (cmdline hemi)
                       (if (material-has-textures? material)
                           (find-shader "diffuse-hemi+spot+tex")
                           (find-shader "diffuse-hemi+spot"))
                           ;(find-shader "diffuse-hemi+tex")
                           ;(find-shader "diffuse-hemi"))
                       (if (material-has-textures? material)
                           (find-shader "diffuse-dl+tex")
                           (find-shader "diffuse-dl"))))
	   (bla (format #t "create de ~a ~a ~a~%" name mesh material))
           (de (make-drawelement name mesh shader material)))
      (add-drawelement-to-scene the-scene de)
      (prepend-uniform-handler de 'default-matrix-uniform-handler)
      (prepend-uniform-handler de 'default-material-uniform-handler)
      (prepend-uniform-handler de custom-uniform-handler)
      (prepend-uniform-handler de depth-uniform-handler)
      (prepend-uniform-handler de shaodw-uniform-handler)
      (prepend-uniform-handler de atomic-buffer-handler)
      ))
  
  (let ((fallback-material (make-material "fallback" (make-vec 1 0 0 1) (make-vec 1 0 0 1) (make-vec 0 0 0 1))))
    (receive (min max) (load-objfile-and-create-objects-with-separate-vbos (cmdline model) (cmdline model) create-drawelement fallback-material)
      (let* ((near 1)
             (far 1)
             (diam (vec-sub max min))
             (diam/2 (vec-div-by-scalar diam 2))
             (center (vec-add min diam/2))
             (distance (vec-length diam))
             (pos (vec-add center (make-vec 0 0 distance))))
        (while (> near (/ distance 100))
          (set! near (/ near 10)))
        (while (< far (* distance 2))
          (set! far (* far 2)))
        (format #t "----> ~a ~a~%" near far)
        ;(let ((cam (make-perspective-camera "cam" pos (make-vec 0 0 -1) (make-vec 0 1 0) 35 (/ x-res y-res) near far)))
	(let ((cam (make-perspective-camera "cam" (make-vec -1242 163 -69) (make-vec 1 0 0) (make-vec 0 1 0) 35 (/ x-res y-res) near far)))  ;near ;far)))
          (use-camera cam))
        (set-move-factor! (/ distance 40)))))
  
  (let ((bunny-mat (make-material "bunnymat" (make-vec 0 0 0 0) (make-vec .8 0 0 .2) (make-vec 0 0 0 1))))
    (load-objfile-and-create-objects-with-separate-vbos "/home/kai/render-data/models/bunny-70k.obj" "bunny70k" create-drawelement bunny-mat))
  
  (if use-dragon
      (let ((dragon-mat (make-material "dragonmat" (make-vec 0 0 0 0) (make-vec 0 .7 0 .2) (make-vec 0 0 0 1))))
	(load-objfile-and-create-objects-with-separate-vbos "/home/kai/render-data/models/drache.obj" "dragon" create-drawelement dragon-mat)))
  
  (let ((bunny (find-drawelement "bunny70k/bunny"))
        (trafo (make-rotation-matrix (make-vec 1 0 0) (/ 3.1416 -2))))
    (mset! trafo 3 1 -43)
    (mset! trafo 3 0 -750)
    (set-de-trafo! bunny trafo))
   
  (if use-dragon
      (let* ((dragon (find-drawelement "dragon/dragon_nObject1Shape"))
	     (trafo-x (make-rotation-matrix (make-vec 1 0 0) (/ 3.1416 -2)))
	     (trafo-y (make-rotation-matrix (make-vec 0 0 1) (/ 3.1416 -2)))
	     (trafo (multiply-matrices trafo-x trafo-y)))
	(set-material-diffuse-color! (drawelement-material dragon) (make-vec 0 .7 0 .7))
	(mset! trafo 3 0 -790)
	(mset! trafo 3 1 -43)
	(mset! trafo 3 2 250)
	(set-de-trafo! dragon trafo)))
  
  (for-each (lambda (de)
              (let* ((de-id (find-drawelement de))
		     (shader (drawelement-shader de-id))
                     (use-coll-shader (is-transparent de-id)))
                ;; fabric is not transparent yet, because the shader for use with textures does not exist.
                (set! drawelements (cons de-id drawelements))
                (set! drawelement-shaders (cons (if use-coll-shader (find-shader (string-append (shader-name shader) "/collect")) #f)
                                                drawelement-shaders))))
            (list-drawelements))
)

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

 
(setup-scene)
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

; (let* ((tqma (make-material "texquad/aa" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
;        (tqme (make-quad "texquad/apply-array"))
;        (tqsh (find-shader "texquad/apply-array"))
;        (de (make-drawelement "texquad" tqme tqsh tqma)))
;   (material-add-texture tqma (find-texture "colorbuffer-color"))
;   (prepend-uniform-handler de atomic-buffer-handler)
;   (prepend-uniform-handler de 'default-material-uniform-handler))
;  
(let* ((tqma (make-material "texquad/ca" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/clear-array"))
       (tqsh (find-shader "texquad/clear-array"))
       (de (make-drawelement "texquad/ca" tqme tqsh tqma)))
  (prepend-uniform-handler de atomic-buffer-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))
 
(let* ((tqma (make-material "texquad/cc" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/clear-color"))
       (tqsh (find-shader "texquad/clear-color"))
       (de (make-drawelement "texquad/cc" tqme tqsh tqma)))
  (prepend-uniform-handler de atomic-buffer-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))

(defmacro with-viewport (vp . body)
  (let-optional vp (ox oy dx dy)
    `(receive (oldox oldoy olddx olddy) (get-viewport)
       (gl:viewport ,ox ,oy ,dx ,dy)
       (let ((res ,body))
         (gl:viewport oldox oldoy olddx olddy)
         res))))

(define gl!!read-only #x88B8)
(define gl!!write-only #x88B9)
(define gl!!read-write #x088ba)

(define all-passes '())

(define shadow-map-pass
  (let ((shader (find-shader "render-shadowmap")))
    (make-pass "shadow map generation" drawelements
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

(define clear-shadow-arrays-pass-oit
  (let ((clear-quad-1 (find-drawelement "texquad/clear-array"))
	(clear-quad-2 (find-drawelement "texquad/clear-color")))
    (make-pass "clear shadow arrays" '()
	       (lambda (de) #f)
	       (begin
		 (shadow-oit 'bind 'head 'tail)
		 (render-drawelement clear-quad-1)
		 (shadow-oit 'unbind 'head 'tail)
		 (shadow-oit 'bind 'colors 'depths)
		 (render-drawelement clear-quad-1)
		 (shadow-oit 'unbind 'colors 'depths)))))

(define clear-transparency-arrays-pass
  (let ((clear-quad-1 (find-drawelement "texquad/clear-array"))
	(clear-quad-2 (find-drawelement "texquad/clear-color")))
    (make-pass "clear transparency arrays" '()
	       (lambda (de) #f)
	       (begin
		 (cam-oit 'bind 'head 'tail)
		 (render-drawelement clear-quad-1)
		 (cam-oit 'unbind 'head 'tail)
		 (cam-oit 'bind 'colors 'depths)
		 (render-drawelement clear-quad-2)
		 (cam-oit 'unbind 'colors 'depths)))))
 
(define transparent-shadow-list-pass
  (let ((hemi+spot (find-shader "transp-shadow-collect"))
	(hemi+spot+tex (find-shader "shadow-collect+tex")))
    (make-pass "transparent shadow linked list pass" drawelements
	       (lambda (de)
		 (if (is-transparent de)
		     (if (material-has-textures? (drawelement-material de))
			 hemi+spot+tex
			 hemi+spot)
		     #f))
	       (with-viewport (0 0 512 512)
	         (let ((opaque-depth-texture (find-texture "shadow-depth")))
	         	 (reset-atomic-buffer atomic-counter 0)
	         	 (bind-atomic-buffer atomic-counter 0)
	         	 (gl:enable gl#polygon-offset-fill)
	         	 (gl:polygon-offset 1 1)
	         	 (disable-color-output
	         	   (disable-depth-output
	         	     (shadow-oit 'bind 'colors 'depths 'head 'tail)
	         	     (render-drawelements (lambda (de sh) 
	         	 			    (bind-texture opaque-depth-texture (material-number-of-textures (drawelement-material de)))))
	         	     (shadow-oit 'unbind 'colors 'depths 'head 'tail)
	         	     (unbind-texture opaque-depth-texture)))
	         	 (gl:disable gl#polygon-offset-fill)
	         	 (gl:finish 0)
	         	 (memory-barrier!!)
	         	 (unbind-atomic-buffer atomic-counter 0))))))

(define shadow-frag-collector-pass
  (let ((shader (find-shader "shadow-frag-collect"))
	(resx 512)
	(resy 512))
    (make-pass "collect shadow fragments" drawelements
	       (lambda (de)
		 shader)
	       (gl:viewport 0 0 resx resy)
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
	       (gl:viewport 0 0 1366 768)
	       (unbind-atomic-buffer atomic-counter 0)
	       )))

(define base-image-pass
  (let ((fbo (find-framebuffer "colorbuffer"))
	(shadow-depth (find-texture "shadow-depth")))
    (make-pass "base image" drawelements
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

(define shadowcam-image-pass
  (let ((fbo (find-framebuffer "shadow")))
    (make-pass "shadowcam image" drawelements
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
	
(define collect-transparent-fragments-pass
  (let ((depthtex (find-texture "opaque-depth"))) ; depth from cam.
    (make-pass "transparent fragment collector" drawelements
	       (lambda (de)
		 (if (is-transparent de)
		     (find-shader (string-append (shader-name (drawelement-shader de)) "/collect"))
		     #f))
	       (begin
		 (reset-atomic-buffer atomic-counter 0)
		 (bind-atomic-buffer atomic-counter 0)
		 (disable-color-output
		  (disable-depth-output
		   (for-each (lambda (de shader)
			       (when shader
				 (bind-texture depthtex (material-number-of-textures (drawelement-material de)))
				 (cam-oit 'bind 'colors 'depths 'head 'tail)
				 (render-drawelement-with-shader de shader)
				 (cam-oit 'unbind 'colors 'depths 'head 'tail)
				 (unbind-texture depthtex)))
			     drawelements
			     shaders)))
		 (unbind-atomic-buffer atomic-counter 0)))))

(define apply-transparency-pass
  (let ((quad (find-drawelement "texquad/apply-array")))
    (make-pass "apply transparency" '()
	       (lambda (de) #f)
	       (begin
		 (gl:clear-color .1 .3 .6 1)
		 (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
	         (cam-oit 'bind 'colors 'depths 'head 'tail)
                 (render-drawelement quad)
	         (cam-oit 'unbind 'colors 'depths 'head 'tail)))))

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
          (when (and (> (- (glut:time-stamp) print-timer) 30000)
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
		  (use-camera (find-camera "cam"))
		  
		  ;; generate the base image.
		  (base-image-pass 'run)
		  (show-base-image-pass 'run)
		  ))

	       ((shadowmap shadow-frag-len)
		(begin
		  (use-camera (find-camera "shadowcam"))
		  (shadowcam-image-pass 'run)
		  (show-shadowcam-image-pass 'run))))
	      
;	     (begin   ;; transparency
;	       (clear-transparency-arrays-pass 'run)
;               ;(memory-barrier!!)
;               ;(gl:finish 0)
;               ;; render to fragment array buffer
;	       (collect-transparent-fragments-pass 'run)
;               ;(memory-barrier!!)
;               ;(gl:finish 0)
;               ;(memory-barrier!!)
;	       (apply-transparency-pass 'run) )
     	       
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
