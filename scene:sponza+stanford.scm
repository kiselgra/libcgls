(define use-dragon #f)

(let ((home (getenv "HOME")))
(setup-scene (string-append home "/render-data/models/sponza.obj"))

(let* ((oldcam (find-camera "scene-cam"))
       (near (cam-near oldcam))
       (far (cam-far oldcam))
       (bla (delete-camera oldcam))
       (cam (make-perspective-camera "scene-cam" (make-vec -1242 163 -69) (make-vec 1 0 0) (make-vec 0 1 0) 35 (/ x-res y-res) near far)))  ;near ;far)))
  (use-camera cam))


  (let ((bunny-mat (make-material "bunnymat" (make-vec 0 0 0 0) (make-vec .8 0 0 .2) (make-vec 0 0 0 1))))
    (add-model-to-scene (string-append home "/render-data/models/bunny-70k.obj") "bunny70k" bunny-mat))
  
  (if use-dragon
      (let ((dragon-mat (make-material "dragonmat" (make-vec 0 0 0 0) (make-vec 0 .7 0 .2) (make-vec 0 0 0 1))))
	(add-model-to-scene (string-append home "/home/kai/render-data/models/drache.obj") "dragon" dragon-mat)))
  
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
)
