(in-package #:goof)

(cl-interpol:enable-interpol-syntax)


(defparameter *start-address* #x000000 "Start address of code to be assembled.")
(defparameter *stack-address* #xFF0000 "Location in memory of the forth parameter stack.")
(defvar *compiling-word* nil)
(defparameter *ram-free-pt* #xFF0100 "Location where forth will start allocating variables and create/does words.")

;; maximum number of instructions to inline
;; this is a pretty bad metric, but I don't know how long instructions
;; are, so it's the best I can currently do
(defparameter *max-inline-length* 3)

;; Compiler API

;;  word (symbol/list) -> String (assembly code)
(defgeneric compile-word (word)
  (:documentation "Compiles a word to 68k assembly code."))

;; code-word (symbol/list of 'lisp-style' assembly) -> String (assembly code)
(defgeneric compile-code-word (word)
  (:documentation "Compiles a code word to 68k assembly code."))

;; colon-word (symbol/list of defined words) -> String (assembly code)
(defgeneric compile-colon-word (word)
  (:documentation "Compiles a colon word to 68k assembly code."))





;; a 'naked' code word is one without a return-from-subroute opcode at
;; the end
(defmacro def-naked-code (name code)
  `(progn
     (setf (gethash ',name *code-words*) ,code
           (gethash ',name *code-word-syms*) (gensym "code"))))

;; same as above, but adds an 'rts' to the end
(defmacro defcode (name code)
  `(def-naked-code ,name (append ,code '((rts)))))

;; 
(defmacro defcolon (name words)
  `(progn
     (setf (gethash ',name *colon-words*) '(,@words exit)
           (gethash ',name *colon-word-syms*) (gensym "colon"))))

;; used to define compiler macros
;; a compiler macro (in this context) is a lambda that takes the rest of
;; the words in the currently compiling word,
;; performs some operation on them, and returns the rest of the words
;; that need to be compiled (and optionally some other bit of code to
;; be assembled in the compiled word)
(defmacro defcompile (name &body body)
  `(setf
    (gethash ',name *compiler-macros*)
    (lambda (words)
        ,@body)))



;; defining constants
;; (these only exist at compile-time, unlike interactive forths)

;; defconst defines a compiler macro that appends an operation to push
;; the constant value onto the words to be compiled
(defmacro defconst (name value)
  `(progn
     (defcompile ,name
         (cons ,value words))))

(defmacro defvariable (name)
  `(progn
     (let ((location (allocate-new-var-cell)))
       ;; define a compiler macro that expands to the allocated address
       (defcompile ,name
         (cons location words))
       ;; define a compiler macro that expands to a read from the
       ;; allocated address
       (defcompile ,(symb name '@)
         `(code
          (move.w d7 (:pre-dec a6))
           (move.w (:absolute ,location) d7)
           end-code
           ,@words))
       ;; define a compiler macro that expands to a write to the
       ;; allocated address
       (defcompile ,(symb name '!)
         `(code
           (move.w d7 (:absolute ,location)) ;; write to variable address
           (move.w (:post-inc a6) d7)   ;; move stack up
           end-code
           ,@words)))))

(defmacro defobject (name cells)
  ;; allocate new cells for each invocation,
  ;; rather than just once (as per variables)
  `(progn
     (defcompile ,name
       (cons
        (compile-allocate-new-var-cell)
        )
       (let ((location (allocate-new-var-cell)))))))


(defun allocate-new-var-cell ()
  (prog1 *ram-free-pt*
    (incf *ram-free-pt* 2)))

;; tables for word and symbols

(defvar *code-words* (make-hash-table :test 'equalp))
(defvar *code-word-syms* (make-hash-table :test 'equalp))
(defvar *colon-words* (make-hash-table :test 'equalp))
(defvar *colon-word-syms* (make-hash-table :test 'equalp))
(defvar *compiler-macros* (make-hash-table :test 'equalp))
(defvar *constants* (make-hash-table :test 'equalp))
(defvar *peephole-optimizations* (make-hash-table :test 'equalp))

;; return stack pointer and stack pointer
(defparameter *rsp* 'a7)
(defparameter *sp* 'a6)

;; initialization and such
(defun compile-start-address ()
  (format nil "ORG ~a~%" *start-address*))

(defun compile-end-address ()
  (format nil "END ~a~%" *start-address*))

(defun compile-initialization ()
  (format nil "    ; initialization ~%~%    MOVE.L #$~x, a6" *stack-address*))


(defun compile-halt ()
  (let ((end-loop-name (gensym "loop")))
        ;(format nil "~a: ~%    ; program ended, infinite loop ~%
                                        ;JMP ~a~%~%" end-loop-name end-loop-name)
    (format nil "    SIMHALT~%")))

;; Compiles an entire forth asm image
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


(defun get-word-code (name)
  (if (gethash name *code-words*)
      (gethash name *code-words*)
      (gethash name *colon-words*)))

(defun get-word-label (name)
  (if (gethash name *code-word-syms*)
      (gethash name *code-word-syms*)
      (gethash name *colon-word-syms*)))



(defmethod compile-word ((word symbol))
  (let ((*compiling-word* word))
    (cond
      ((gethash word *colon-words*) (compile-colon-word word))
      ((gethash word *code-words*) (compile-code-word word))
      (t (error "Cannot find word named ~a." word)))))

(defmethod compile-word ((word list))
  (let ((*compiling-word* (if *compiling-word* *compiling-word* :anonymous)))
    (etypecase (car word)
      (symbol (compile-colon-word word))
      (number (compile-colon-word word))
      (list (compile-code-word word)))))

(defun compile-label (label)
  (format nil "~a: ~%" label))



(defmethod compile-code-word ((word list))
  (if word
      (apply
       #'concatenate
       'string
       (compile-label (gethash word *code-word-syms*)) 
       (append
        (list (format nil "    ; definition for ~a ~%" word))
        (list (translate-asm word)))) (error "Cannot find code word wordd ~a." word)))


(defmethod compile-code-word ((word symbol))
  (let ((code (gethash word *code-words*))
        (sym  (gethash word *code-word-syms*)))
    (if code
        (apply
         #'concatenate
         'string
         (compile-label sym) 
         (append
          (list (format nil "    ; definition for ~a ~%" word))
          (list (translate-asm code)))) (error "Cannot find code word word ~a." word))))

;; old tos in memory version

;; (defun compile-push (number)
;;   (format nil "    MOVE.W #~a, -(a6)~%" number))

(defun compile-push (number)
  (format nil "    MOVE.W d7, -(a6)~%    MOVE.W #~a, d7~%" number))



(defun compile-list-of-words (word-name words &key (header-string
                                                         (format nil "    ; definition of ~a~%" word-name))
                                                (inlining t))
  (labels ((recur (rem-words)
             (unless (null rem-words)

               (let ((word (car rem-words)))
                 (typecase word
                   (number (cons (compile-push word)
                                 (recur (cdr rem-words))))
                   (symbol
                    (if (gethash word *compiler-macros*)
                        (multiple-value-bind (rem-words result)
                            (funcall (gethash word *compiler-macros*)
                                     (cdr rem-words))
                          (if result
                              (append result
                                    (recur rem-words))
                              (recur rem-words)))
                        
                        (cons
                         (cond ((eq 'exit word) (compile-exit))
                               ((or (and (not inlining) (short-word-p word))
                                    (super-short-word-p word))
                                (compile-inline-body word))
                               ((eq 'exit (second rem-words))
                                (prog1 (compile-word-jump word)
                                  (setf rem-words (cdr rem-words))))
                               (t (compile-word-call word)))
                         (recur (cdr rem-words))))))))))
    (if words
        (apply #'concatenate 'string
               
               (when word-name (compile-label (gethash word-name *colon-word-syms*)))
               
               (append
                (when (and word-name (not inlining))
                  (list header-string))
                (recur words)))
        (if word-name
            (error "Cannot find colon word word ~a."  word-name)
            (error "Please supply at least one word to compile.")))))

(defmethod compile-colon-word ((words list))
  (compile-list-of-words nil words :inlining nil))

(defmethod compile-colon-word ((word symbol))
  (compile-list-of-words word (gethash word *colon-words*) :inlining nil))

;; if it's short enough (1 instruction?) always inline, even nested
(defun super-short-word-p (word)
  (let ((*max-inline-length* 2))
    (declare (special *max-inline-length*))
    (short-word-p word)))

(defun short-word-p (word)
  (let* ((code-word (gethash word *code-words*))
         (colon-word (gethash word *colon-words*))
         (word (or colon-word code-word)))
    (and word (>= *max-inline-length* (length word)))))

(defun compile-exit ()
  (format nil "    RTS~%"))


(defun compile-word-call (word)
  (compile-word-branch word "JSR" "call to"))

(defun compile-word-jump (word)
  (compile-word-branch word "JMP" "tail-call to"))

(defun compile-word-branch (word branch-type text)
  (let ((name (cond
                ((gethash word *colon-words*) (gethash word *colon-word-syms*))
                ((gethash word *code-words*) (gethash word *code-word-syms*))
                (t (error "Cannot find word named ~a." word)))))
    (format nil "    ~a ~a ; ~a ~a~%" branch-type (format nil "~a" name) text word)))

;; only code words for now
(defun compile-inline-body (word)
  (let ((colon-word (gethash word *colon-words*))
        (code-word (gethash word *code-words*)))
    (or (and colon-word (compile-inline-colon-word word colon-word))
        (and code-word (compile-inline-code-word word code-word)))))

(defun compile-inline-colon-word (word-name words)
  (compile-list-of-words word-name
                         (if (eq (car (last words)) 'exit)
                             (butlast words)
                             words)
                         :header-string
                         (format nil "    ; inlining ~a~%" word-name)
                         ;; no nested inlining
                         :inlining t))

(defun compile-inline-code-word (word-name asm)
  ;; strip rts from the end if it exists
  (concatenate
   'string
   (format nil "    ; inlining ~a ~%" word-name)
   (if (eq (caar (last asm)) 'rts)
       (translate-asm (butlast asm))
       (translate-asm asm))))

(defun translate-asm (code)
  (apply #'concatenate 'string (mapcar #'translate-instruction code)))

(defun translate-instruction (instruction)
  (destructuring-bind (opcode &optional op-a op-b) instruction

    (with-output-to-string (out)
      (if (eq :label opcode)
          (format out "~a:~%" op-a)
          (progn
            (format out "    ~a" opcode)
            (when op-a
              (format out " ~a" (translate-operand op-a)))
            (when op-b
              (format out ", ~a" (translate-operand op-b)))
            (format out " ~%"))))))

(defun translate-operand (operand)
  (typecase operand
    (symbol operand)
    (list (translate-addressing-mode operand))))


(defun translate-addressing-mode (operand)
  (destructuring-bind (addressing operand &rest operands) operand
    (case addressing
      ((:pre-dec :pre-decrement)
       #?"-(${operand})")
      ((:post-inc :post-increment)
       #?"(${operand})+")
      ((:ind :indi :indirect)
       #?"(${operand})")
      ((:disp :displace :displacement :indirect-displacement)
       #?"${operand}(${(first operands)})")
      ((:indexed-indirect :indirect-indexed :indi-indexed :indirect-idx :idx-indi :index-indi :indexed-indi  :index-indirect)
       #?"${operand}(${(first operands)},${(second operands)})")
      ((:abs :absolute :addr :address :absolute-address)
       #?"${operand}")
      ((:pc-displacement :pc-disp)
       #?"${operand}(PC)")
      ((:pc-index :pc-indexed)
       #?"${operand}(PC,${(first operands)})")
      ((:imm :immediate)
       #?"#${operand}")
      (otherwise (error "Unknown addressing type ~a." addressing)))))



;; returns a list of words that need to be compiled
(defun shake-threads (word &optional (shaken-words (make-hash-table :test 'eq)))
  (let ((words-to-shake (list word)))
    (setf (gethash word shaken-words) t)
    (loop while words-to-shake do
         (let ((next-set-of-words-to-shake (list)))
           (loop for word in words-to-shake do
                (let ((new-words-to-shake (gethash word *colon-words*)))
                  (loop for new-word in new-words-to-shake do
                       (unless (or (gethash new-word shaken-words)
                                   (and (not (gethash new-word *colon-words*))
                                        (not (gethash new-word *code-words*))))
                         (setf (gethash new-word shaken-words) t)
                         (push new-word next-set-of-words-to-shake)))))
           (setf words-to-shake next-set-of-words-to-shake)))
    shaken-words))


