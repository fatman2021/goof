;;; tools for writing binary images

;;;
(in-package #:goof)

(defun write-header ()
  (format nil "DC.b")
  )

(defun compile-initialization ()
  ;; initialize stack address
  (concatenate
   'string
   (format nil "    ; initialization ~%~%    MOVE.L #$~x, a6~%" *stack-address*)
   ;; initialize variable values
   (format nil "    ; setting free pointer ~%~a~%"
           (compile-word `(code (move.w (:immediate ,(- *ram-free-pt* #xFF0000)) d7)
                                end-code
                                free-pt!))
           )))

(defun compile-end-address ()
  (format nil "END ~a~%" *start-address*))

(defun compile-start-address ()
  (format nil "ORG ~a~%" *start-address*))

(defun compile-halt ()
  (let ((end-loop-name (gensym "loop")))
        ;(format nil "~a: ~%    ; program ended, infinite loop ~%
                                        ;JMP ~a~%~%" end-loop-name end-loop-name)
    (format nil "    SIMHALT~%")))

(defun compile-set-variables ()
  ;; set free pt for memory allocations
  (format nil "~a~%" (compile-word `(,*ram-free-pt* set-free-pt))))

;; compiles an entire forth image
(defun compile-image (file start-words)

  (let ((words-to-compile (make-hash-table :test 'eq)))
    (with-open-file (out file :if-exists :supersede :if-does-not-exist :create :direction :output)
      (format out "    ~a~%" (compile-start-address))

      
      (format out "~a~%" (compile-initialization))
      
      
      (mapcar
       (lambda (start-word)
         (typecase start-word
           (number (format out "~a~%" (compile-push start-word)))
           (symbol
            (shake-threads start-word words-to-compile)
            (format out "    ; initial call to ~a    ~%    JSR ~a ~%"
                    start-word
                    (format nil "~a" (get-word-label start-word))))))
       start-words)

      
      (format out "~%")

      (format out "~a~%" (compile-halt))
      
      ;; compile dependencies
      (maphash (lambda (word v) (declare (ignore v))
                  (format t "compiling word ~a~%" word)
                  (format out "~a" (compile-word word)))
               words-to-compile)
      
      
      (format out "~%~%    ~a" (compile-end-address)))))



