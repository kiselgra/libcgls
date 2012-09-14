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

(defmacro cmdline (x)
  `(query-cmdline ',x))


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

(gl:enable gl#depth-test)

    
    
(format #t "Leaving ~a.~%" (current-filename))
