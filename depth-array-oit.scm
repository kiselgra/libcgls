(format #t "Entering ~a.~%" (current-filename))

;;; modules
(use-modules (ice-9 receive))
(use-modules (rnrs bytevectors))

;; nicer cl-style keyword syntax 
(read-set! keywords 'prefix)

;;; code to definitely move someplace else

(define (mref bv x y)
  (bytevector-ieee-single-native-ref bv (* 4 (+ (* x 4) y))))

(define (mset! bv x y to)
  (bytevector-ieee-single-native-set! bv (* 4 (+ (* x 4) y)) to))

(define (print-matrix bv)
  (if (not (bytevector? bv))
      (error "not a matrix ~a~%" bv)
      (do ((y 0 (+ y 1)))
          ((>= y 4))
        (do ((x 0 (+ x 1)))
            ((>= x 4))
          (display (mref bv x y)) ;bytevector-ieee-single-native-ref bv (* 4 (+ (* x 4) y))))
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

(define (depth-uniform-handler de uniform location)
  (cond ((string=? "depth" uniform)
         (gl:uniform1i location (material-number-of-textures (drawelement-material de))) #t)
        (else #f)))

;; buffers for depth array management
(make-texture-without-file "frag_mutex" gl#texture-2d x-res y-res gl#red gl#r32f gl#float)
(make-texture-without-file "frag_colors" gl#texture-2d x-res (* y-res 20) gl#rgba gl#rgba8 gl#unsigned-byte)
(make-texture-without-file "frag_depths" gl#texture-2d x-res (* y-res 20) gl#red gl#r32f gl#float)

(define (atomic-buffer-handler de u l)
  (cond ((string=? u "mutex_buffer") (gl:uniform1i l 0))
        ((string=? u "per_frag_colors") (gl:uniform1i l 1))
        ((string=? u "per_frag_depths") (gl:uniform1i l 2))
        ((string=? u "wh") (gl:uniform2i l x-res y-res))
        (else #f)))

;; this one is for debugging, only.
(define atomic-counter (make-atomic-buffer "test" 1 1))
(define copy-of-atomic-buffer #f)

;; scene loading
(define drawelements '())
(define drawelement-shaders '())

(define (setup-scene)
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
      (prepend-uniform-handler de depth-uniform-handler)
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
        ;(let ((cam (make-perspective-camera "cam" pos (make-vec 0 0 -1) (make-vec 0 1 0) 35 (/ x-res y-res) near far)))
	    (let ((cam (make-perspective-camera "cam" (make-vec -1342 163 -69) (make-vec 1 0 0) (make-vec 0 1 0) 35 (/ x-res y-res) near far)))
          (use-camera cam))
        (set-move-factor! (/ distance 40)))))
  
  (let ((bunny-mat (make-material "bunnymat" (make-vec 0 0 0 0) (make-vec .8 0 0 .3) (make-vec 0 0 0 1))))
    (load-objfile-and-create-objects-with-separate-vbos "/home/kai/render-data/models/bunny-70k.obj" "bunny70k" create-drawelement bunny-mat))
  
  (let ((dragon-mat (make-material "dragonmat" (make-vec 0 0 0 0) (make-vec 0 .7 0 .4) (make-vec 0 0 0 1))))
    (load-objfile-and-create-objects-with-separate-vbos "/home/kai/render-data/models/drache.obj" "dragon" create-drawelement dragon-mat))
  
  (let ((bunny (find-drawelement "bunny70k/bunny"))
        (trafo (make-rotation-matrix (make-vec 1 0 0) (/ 3.1416 -2))))
    (mset! trafo 3 1 -43)
    (mset! trafo 3 0 -750)
    (set-de-trafo! bunny trafo))
  
  (let* ((dragon (find-drawelement "dragon/dragon_nObject1Shape"))
         (trafo-x (make-rotation-matrix (make-vec 1 0 0) (/ 3.1416 -2)))
         (trafo-y (make-rotation-matrix (make-vec 0 0 1) (/ 3.1416 -2)))
         (trafo (multiply-matrices trafo-x trafo-y)))
    (set-material-diffuse-color! (drawelement-material dragon) (make-vec 0 .7 0 .4))
    (mset! trafo 3 0 -700)
    (mset! trafo 3 1 -43)
    (mset! trafo 3 2 150)
    (set-de-trafo! dragon trafo))
  
  (for-each (lambda (de)
              (let* ((de-id (find-drawelement de))
                     (shader (drawelement-shader de-id))
                     (material (drawelement-material de-id))
                     (diffuse (material-diffuse-color material))
                     (use-coll-shader (cond ((and (< (vec-a diffuse) 1) (> (vec-a diffuse) 0)) #t)
                                            ((string-contains (material-name material) "fabric")
                                             (set-material-diffuse-color! material (make-vec 1 1 1 .6))
                                             #t)
                                            (else #f))))
                ;; fabric is not transparent yet, because the shader for use with textures does not exist.
                (set! drawelements (cons de-id drawelements))
                (set! drawelement-shaders (cons (if use-coll-shader (find-shader (string-append (shader-name shader) "/collect")) #f)
                                                drawelement-shaders))))
            (list-drawelements))
)

(define (setup-opaque-rendering)
  (let ((depth (make-texture-without-file "opaque-depth" gl#texture-2d x-res y-res gl#depth-component gl#depth-component32f gl#float))
        (color (make-texture-without-file "opaque-color" gl#texture-2d x-res y-res gl#rgba gl#rgba8 gl#unsigned-byte))
        (fbo (make-framebuffer "opaque" x-res y-res)))
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
  (material-add-texture tqma (find-texture "opaque-color"))
  (material-add-texture tqma (find-texture "opaque-depth"))
  (prepend-uniform-handler de 'default-material-uniform-handler))

(let* ((tqma (make-material "texquad/aa" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/apply-array"))
       (tqsh (find-shader "texquad/apply-array"))
       (de (make-drawelement "texquad" tqme tqsh tqma)))
  (prepend-uniform-handler de atomic-buffer-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))

(let* ((tqma (make-material "texquad/ca" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/clear-array"))
       (tqsh (find-shader "texquad/clear-array"))
       (de (make-drawelement "texquad" tqme tqsh tqma)))
  (prepend-uniform-handler de atomic-buffer-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))

(let* ((tqma (make-material "texquad/cc" (make-vec 0 0 0 1) (make-vec 0 1 0 1) (make-vec 0 0 0 1)))
       (tqme (make-quad "texquad/clear-color"))
       (tqsh (find-shader "texquad/clear-color"))
       (de (make-drawelement "texquad" tqme tqsh tqma)))
  (prepend-uniform-handler de atomic-buffer-handler)
  (prepend-uniform-handler de 'default-material-uniform-handler))


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

;; convenience

(defmacro disable-color-output body
  `(begin (gl:color-mask gl#false gl#false gl#false gl#false)
          ,@body
          (gl:color-mask gl#true gl#true gl#true gl#true)))

(defmacro disable-depth-output body
  `(begin (gl:depth-mask gl#false)
          ,@body
          (gl:depth-mask gl#true)))

(define gl!!read-write #x088ba)

;; the display routine registered with glut

(define (display/glut)
  (gl:clear-color .9 .9 .6 1)
  (apply-commands)

  (check-for-gl-errors "right at the beginning")
  ;; generate the base image.
  (let ((fbo (find-framebuffer "opaque"))
        (coltex (find-texture "opaque-color"))
        (depthtex (find-texture "opaque-depth")))
    (bind-framebuffer fbo)
    (gl:clear-color .1 .3 .6 1)
    (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
    (for-each (lambda (de transparent)
                (when (not transparent)
                  (render-drawelement de)))
              drawelements
              drawelement-shaders)
    (unbind-framebuffer fbo)
    )

  ;; clear arrays
  (check-for-gl-errors "before array clear")
  (let ((frag-counter (find-texture "frag_mutex"))
        (frag-colors (find-texture "frag_colors")))
    (bind-texture-as-image frag-counter 0 0 gl!!read-write gl#r32i)
    (render-drawelement (find-drawelement "texquad/clear-array"))
    (unbind-texture-as-image frag-counter 0)
    (bind-texture-as-image frag-colors 1 0 gl!!read-write gl#rgba8)
    (render-drawelement (find-drawelement "texquad/clear-color"))
    (unbind-texture-as-image frag-colors 0)
    )
  
  (gl:finish 0)
   
  (let ((frag-colors (find-texture "frag_colors")))
    (bind-texture frag-colors 0)
;    (save-texture/png frag-colors "bla1.png")
    (unbind-texture frag-colors))

  
  ;; display the base image
  (check-for-gl-errors "before mapping of base image")
  (gl:clear-color .1 .3 .6 1)
  (gl:clear (logior gl#color-buffer-bit gl#depth-buffer-bit))
  (render-drawelement (find-drawelement "texquad/texquad"))


;  (for-each (lambda (de transparent)
;              (when transparent
;                (render-drawelement de)))
;            drawelements
;            drawelement-shaders)


  ;; render to fragment array buffer
  (check-for-gl-errors "before array collect shader")
  (reset-atomic-buffer atomic-counter 0)
  (bind-atomic-buffer atomic-counter 0)
  (let ((depthtex (find-texture "opaque-depth"))
        (frag-counter (find-texture "frag_mutex"))
        (frag-colors (find-texture "frag_colors"))
        (frag-depths (find-texture "frag_depths")))
    (disable-color-output
      (disable-depth-output
        (for-each (lambda (de shader)
                    (when shader
                      (bind-texture depthtex (material-number-of-textures (drawelement-material de)))
                      (bind-texture-as-image frag-counter 0 0 gl!!read-write gl#r32i)
                      (bind-texture-as-image frag-colors 1 0 gl!!read-write gl#rgba8)
                      (bind-texture-as-image frag-depths 2 0 gl!!read-write gl#r32f)
                      (render-drawelement-with-shader de shader)
                      (unbind-texture-as-image frag-depths 2)
                      (unbind-texture-as-image frag-colors 1)
                      (unbind-texture-as-image frag-counter 0)
                      (unbind-texture depthtex)))
                  drawelements
                  drawelement-shaders))
        ))
  (unbind-atomic-buffer atomic-counter 0)

  ;; read debugging info
  (gl:finish 0) ;; bug in wrapper/gen -> glFinish(void);
  (check-for-gl-errors "before atomic dl")
  (set! copy-of-atomic-buffer (read-atomic-buffer atomic-counter))
  (format #t "fragments rendered in collection pass: ~a~%" (bytevector-s32-native-ref copy-of-atomic-buffer 0))

  (gl:finish 0)
  (check-for-gl-errors "before using the array info")

  (let ((frag-counter (find-texture "frag_mutex"))
        (frag-colors (find-texture "frag_colors"))
        (frag-depths (find-texture "frag_depths")))
    (bind-texture-as-image frag-counter 0 0 gl!!read-write gl#r32i)
    (bind-texture-as-image frag-colors 1 0 gl!!read-write gl#rgba8)
    (bind-texture-as-image frag-depths 2 0 gl!!read-write gl#r32f)
    (render-drawelement (find-drawelement "texquad/apply-array"))
    (unbind-texture-as-image frag-depths 2)
    (unbind-texture-as-image frag-colors 1)
    (unbind-texture-as-image frag-counter 0))

  (gl:finish 0)

  (check-for-gl-errors "before debug output")
  (let ((frag-colors (find-texture "frag_colors")))
    (bind-texture frag-colors 0)
;    (save-texture/png frag-colors "bla2.png")
    (unbind-texture frag-colors))

  (glut:swap-buffers))

;; initial gl setup
;;
(register-display-function display/glut)
(gl:enable gl#depth-test)

;; done
;;
(format #t "Leaving ~a.~%" (current-filename))
