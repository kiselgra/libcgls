(format #t "Entering ~a.~%" (current-filename))

;;; modules
(use-modules (ice-9 receive))
(use-modules (rnrs bytevectors))

;; nicer cl-style keyword syntax 
(read-set! keywords 'prefix)

;;; code to definitely move someplace else

(define (print-matrix bv)
  (if (not (bytevector? bv))
      (error "not a matrix ~a~%" bv)
      (do ((y 0 (+ y 1)))
          ((>= y 4))
        (do ((x 0 (+ x 1)))
            ((>= x 4))
          (display (bytevector-ieee-single-native-ref bv (* 4 (+ (* x 4) y))))
          (display "\t"))
        (newline))))

;;; actual code

(define x-res 1)
(define y-res 1)

(receive (x y w h) (viewport)
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

(define the-scene (make-scene "default"))

;; scene handling
(define (custom-uniform-handler de uniform location)
  (cond ((string=? uniform "light_dir") (gl:uniform3f location 0 -1 -0.2))
        ((string=? uniform "light_col") (gl:uniform3f location 1 .9 .9))
		((string=? uniform "hemi_dir") 
		   (let ((h (cmdline hemi-dir))) 
		 	 (gl:uniform3f location (car h) (cadr h) (caddr h))))
		(else #f)))

(define (dp-uniform-handler de uniform location)
  (cond ((string=? "depth" uniform)
         (gl:uniform1i location (material-number-of-textures (drawelement-material de))) #t)
        (else #f)))

;; scene loading
(define drawelements '())
(define peeling-shaders '())

(define (create-drawelement name mesh material)
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
	(prepend-uniform-handler de dp-uniform-handler)
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
	  (let ((cam (make-perspective-camera "cam" pos (make-vec 0 0 -1) (make-vec 0 1 0) 35 (/ x-res y-res) near far)))
	  ;(let ((cam (make-perspective-camera "cam" (make-vec -64.431862 698.080017 390.393158) (make-vec -0.319869 -0.316504 -0.893030) (make-vec -0.025723 0.945105 -0.325747) 35 (/ x-res y-res) near far)))
        (use-camera cam))
      (set-move-factor! (/ distance 40)))))

(let ((bunny-mat (make-material "bunnymat" (make-vec 0 0 0 0) (make-vec .8 0 0 .3) (make-vec 0 0 0 1))))
  (load-objfile-and-create-objects-with-separate-vbos "/home/kai/render-data/models/bunny-70k.obj" "bunny70k" create-drawelement bunny-mat))

(for-each (lambda (de)
            (let* ((de-id (find-drawelement de))
                   (shader (drawelement-shader de-id))
                   (material (drawelement-material de-id))
                   (diffuse (material-diffuse-color material))
                   (use-peel-shader (cond ((and (< (vec-a diffuse) 1) (> (vec-a diffuse) 0)) #t)
                                          ((string-contains (material-name material) "fabric")
                                           (set-material-diffuse-color! material (make-vec 1 1 1 .6))
                                           #t)
                                          (else #f))))
              (format #t "diffuse of ~a is ~a.~%" material diffuse)
              (set! drawelements (cons de-id drawelements))
              (set! peeling-shaders (cons (if use-peel-shader (find-shader (string-append (shader-name shader) "/dp")) #f)
                                          peeling-shaders))))
          (list-drawelements))


;; depthpeeling buffers
;; - the fbos are called 'layers'
;; - there are max-layers color buffers (including the base layer), all held in one list
;; - there are 3 depth buffers: the one of the base layer and two buffers used in rotation to implement depth peeling
(define max-layers 10)
(define opaque-depth (make-texture-without-file "opaque-depth" gl#texture-2d x-res y-res gl#depth-component gl#depth-component32f gl#float))
(define depth-0 (make-texture-without-file "dp-depth-0" gl#texture-2d x-res y-res gl#depth-component gl#depth-component32f gl#float))
(define depth-1 (make-texture-without-file "dp-depth-1" gl#texture-2d x-res y-res gl#depth-component gl#depth-component32f gl#float))
(define ids (let ((i 0))    ; this list is used to generate appropriate names for the color texs and fbos.
              (map (lambda (_) (let ((old i)) (set! i (+ i 1)) old))
                   (make-list max-layers))))
(define color-texs (map (lambda (i) (make-texture-without-file (format #f "coltex-~a" i) gl#texture-2d x-res y-res gl#rgba gl#rgba8 gl#unsigned-byte))
                        ids))
(define fbos (map (lambda (i) (make-framebuffer (format #f "layer-~a" i) x-res y-res))
                  ids))
                        
(let loop ((color-tex (car color-texs)) (depth-tex opaque-depth) (color-texs (cdr color-texs)) (fbo (car fbos)) (fbos (cdr fbos)))
  (format #t "Making Fbo '~a' with color texture '~a' and depth buffer '~a'~%" (framebuffer-name fbo) (texture-name color-tex) (texture-name depth-tex))
  (bind-framebuffer fbo)
  (bind-texture color-tex 0)
  (attach-texture-as-colorbuffer fbo (texture-name color-tex) color-tex)
  (bind-texture depth-tex 1)
  (attach-texture-as-depthbuffer fbo (texture-name depth-tex) depth-tex)
  (check-framebuffer-setup fbo)
  (unbind-framebuffer fbo)
  (if (not (null? color-texs))
      (loop (car color-texs)
            (if (equal? depth-tex depth-0) depth-1 depth-0)
            (cdr color-texs)
            (car fbos)
            (cdr fbos))))

 
;; tex textured quad
;; 
(let* ((tqma (make-material "texquad" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad-with-tc "texquad"))
       (tqsh (find-shader "texquad"))
       (de (make-drawelement "texquad" tqme tqsh tqma)))
  (material-add-texture tqma (car color-texs))
  (prepend-uniform-handler de 'default-material-uniform-handler))


;; copy a depth buffer from a texture to an fbo.
;;
;(let ((copy-geometry (make-drawelement "depth-copy-quad" (find-mesh "texquad") (find-shader "copy-depth") (find-material "texquad"))))
(define* (copy-depth-buffer :key from to)
  (let ((shader (find-shader "copy-depth"))
        (mesh (find-mesh "texquad")))
    (bind-framebuffer to)
    (gl:color-mask gl#false gl#false gl#false gl#false)
    (gl:depth-func gl#always)
    (bind-shader shader)
    (bind-texture from 0)
    (gl:uniform1i (uniform-location shader "depth") 0)
    (bind-mesh mesh)
    (draw-mesh mesh gl#triangles)
    (unbind-mesh mesh)
    (unbind-texture from)
    (unbind-framebuffer to)
    (gl:depth-func gl#less)
    (gl:color-mask gl#true gl#true gl#true gl#true)))
    
    
 
;; on the fly eval of gl code

(define command-queue '())
(defmacro enqueue (cmd)
  `(begin
      (format #t "cmd: .~a.~%" ',cmd)
      (set! command-queue (cons ',cmd command-queue))))
(define (apply-commands)
  (for-each primitive-eval
            (reverse command-queue))
  (set! command-queue '()))

;; the display routine registered with glut

(define peel #t)
(define peel-debug #f)

(define (display/glut)
  (gl:clear-color .9 .9 .6 1)
  (apply-commands)

  ;; generate the base image.
  (let ((fbo (car fbos))
        (coltex (car color-texs))
        (depthtex opaque-depth))
    (bind-framebuffer fbo)
    (gl:clear-color .1 .3 .6 1)
    (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
    (for-each (lambda (de transparent)
                (when (not transparent)
                  (render-drawelement de)))
              drawelements
              peeling-shaders)
    (unbind-framebuffer fbo)
    (bind-texture coltex 0)
    (if peel-debug (save-texture/png coltex "layer-0-c.png")))
  
  ;; display the base image
  (gl:clear-color .1 .3 .6 1)
  (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
  (render-drawelement (find-drawelement "texquad/texquad"))

  (when peel 
    ;;; generate the different depth slice images
    ;; first: the initial minimal depth is on the near plane
    (gl:clear-depthf 0)
    (bind-framebuffer (list-ref fbos 2))
    (gl:clear gl#depth-buffer-bit)
    (unbind-framebuffer (list-ref fbos 2))
    (gl:clear-depthf 1)
    (let peeling-loop ((fbos (cdr fbos)) (color-texs (cdr color-texs)) (depth-tex depth-1) (i 1))
      (let ((fbo (car fbos)) (color-tex (car color-texs)))
        ;; each time: initialize the depth buffer of the current slice to the opaqe objects. noting behind them can be seen.
        (if peel-debug (format #t "rendering to layer ~a. using fbo ~a, minimal depth taken from ~a.~%" i (framebuffer-name fbo) (texture-name depth-tex)))
        (copy-depth-buffer :from opaque-depth :to fbo)
        (bind-framebuffer fbo)
        ;; now render to the slice (clearing to alpha=0 ignores unset fragments in the blending pass).
        (gl:clear-color 0.3 0.3 0.3 0)
        (gl:clear gl#color-buffer-bit)
        (gl:enable gl#polygon-offset-fill)
        (gl:polygon-offset (- i) (- i))
        (for-each (lambda (de sh)
                    (when sh
                      (bind-texture depth-tex (material-number-of-textures (drawelement-material de)))
                      (render-drawelement-with-shader de sh) ; (find-shader "diffuse-hemi/dp"))
                      (unbind-texture depth-tex)))
                  drawelements
                  peeling-shaders)
        (unbind-framebuffer fbo)
        (when peel-debug
          (bind-texture color-tex 0)
          (save-texture/png color-tex (format #f "layer-~a-c.png" i)))
        (gl:disable gl#polygon-offset-fill))
      (if (not (null? (cdr fbos)))
          (peeling-loop (cdr fbos) (cdr color-texs) (if (equal? depth-tex depth-0) depth-1 depth-0) (1+ i))))
    ;; now blend the layers
    (gl:enable gl#blend)
    (gl:blend-func gl#src-alpha gl#one-minus-src-alpha)
    (gl:depth-func gl#always)
    (let ((shader (find-shader "texquad"))
          (mesh (find-mesh "texquad")))
      (let blend-loop ((color-texs (reverse (cdr color-texs))))
        (bind-shader shader)
        (bind-texture (car color-texs) 0)
        (gl:uniform1i (uniform-location shader "tex0") 0)
        (bind-mesh mesh)
        (draw-mesh mesh gl#triangles)
        (unbind-mesh mesh)
        (unbind-texture (car color-texs))
        (if (not (null? (cdr color-texs)))
          (blend-loop (cdr color-texs))))
    )
    (gl:depth-func gl#less)
    (gl:disable gl#blend))
  (set! peel-debug #f)

  (glut:swap-buffers))

;; initial gl setup
;;
(register-display-function display/glut)
(gl:enable gl#depth-test)

;; done
;;
(format #t "Leaving ~a.~%" (current-filename))
