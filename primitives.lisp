(in-package #:goof)

;; exit from a colon word (though this pretty much always gets
;; converted into a straight 'rts' in it's callee, which has the same effect)
(defcode exit  `((addq (:imm 2) ,*rsp*)))

;; duplicate the top item on the stack
(defcode dup   `((move.w (:indirect ,*sp*) (:pre-dec ,*sp*))))

;; drop the top item off the stack
(defcode drop  `((addq (:imm 2) ,*sp*)))


(defcode 2drop `((addq (:imm 4) ,*sp*)))

(defcode 2dup  `((move.l ,*sp* (:pre-dec ,*sp*))))

;; ( a b -- a b a )
(defcode over  `((move.w (:displacement 2 ,*sp*) (:pre-dec ,*sp*))))

(defcode r>    `((move.w (:post-inc ,*rsp*) (:pre-dec ,*sp*))))

(defcode r>>   `((move.l (:post-inc ,*rsp*) (:pre-dec ,*sp*))))

(defcode >r    `((move.w (:post-inc ,*sp*)  (:pre-dec ,*rsp*))))

(defcode >>r   `((move.l (:post-inc ,*sp*)  (:pre-dec ,*rsp*))))



(defcode +     `((add.w (:post-inc ,*sp*)
                         (:indirect ,*sp*))))

(defcode -      `((sub.w (:post-inc ,*sp*)
                         (:indirect ,*sp*))))

(defcode *     `((mult.q (:post-inc ,*sp*)
                          (:indirect ,*sp*))))

(defcode 2*    `((lsl (:imm 1) (:indirect ,*sp*))))

(defcode 4*    `((lsl (:imm 2) (:indirect ,*sp*))))

(defcode 2/    `((rsl (:imm 1) (:indirect ,*sp*))))

(defcode 4/    `((rsl (:imm 2) (:indirect ,*sp*))))

(defcode inc   `((addq (:imm 1) (:indirect ,*sp*))))

(defcode 2+    `((addq (:imm 2) (:indirect ,*sp*))))

(defcode 4+    `((addq (:imm 4) (:indirect ,*sp*))))

(defcode dec   `((subq (:imm 1) (:indirect ,*sp*))))

(defcode swap  `((move.l (:indirect ,*sp*) d7)
                  (swap d7)
                  (move.l d7 (:indirect ,*sp*))))

(defcode rot   `((movem.w (:indirect ,*sp*) d0-d2)
                  (exg d2 d1)
                  (exg d1 d0)
                  (movem.w d0-d2 (:indirect ,*sp*))))


(defcode -rot  `((movem.w (:indirect ,*sp*) d0-d2)
                  (exg d0 d1)
                  (exg d1 d2)
                  (movem.w d0-d2 (:indirect ,*sp*))))

(defcode lit   `((move.w (:indirect ,*rsp*) (:pre-dec ,*sp*))
                 (addq (:imm 2) (:indirect ,*rsp*))))

(defcode execute `((jmp (:indirect ,*rsp*))))

(defcode ?dup  (let ((name (gensym "label")))
                 `((tst (:indirect ,*sp*))
                   (beq ,name)
                   (move.w (:indirect ,*sp*) (:pre-dec ,*sp*))
                   (:label ,name))))

;; should this be destructive?
(defcode =0  `((tst.w (:indirect ,*sp*))))



;; ;; general compare word, preferred to use this
;; ;; (or one of the compare-with-zero words),
;; ;; then use the contextually correct branching word
(defcode = `((cmp (:indirect ,*sp*) (:indirect ,*sp*))))
(defcode cmp `((cmp (:indirect ,*sp*) (:indirect ,*sp*))))

;; first operand is the divisor
;; remainder is second on stack
(defcode /mod `((move.w (:post-inc ,*sp*) d7)
                (move.w (:post-inc ,*sp*) d6)
                (divs d7 d6)
                (swap d6)
                (move.l d6 (:pre-dec ,*sp*))
                (moveq.l (:imm 0) d6)
                (moveq.l (:imm 0) d7)))

(defcode mod  `((move.w (:post-inc ,*sp*) d7)
                (move.w (:post-inc ,*sp*) d6)
                (divs d7 d6)
                (swap d6)
                (move.w d6 (:pre-dec ,*sp*))
                (moveq.l (:imm 0) d6)
                (moveq.l (:imm 0) d7)))

;; (defcode =)
;; (defcode <)
;; (defcode <0)
;; (defcode >)
;; (defcode >0)
;; (defcode >=)
;; (defcode >=0)
;; (defcode <=0)

(defcolon gcd (?dup =0 =if swap over recurse then))

(defun compile-if (test-instruction words)
  (let* ((then (position 'then words))
         (else (position 'else words))
         (if-words
          (cond
            (else (subseq words 0 else))
            (then (subseq words 0 then))))
         (else-words (if else
                         (subseq words else)))
         (rest-words (subseq words (1+ then))))
    ;(format t "if-words: ~a~%" if-words)
    ;(format t "else-words: ~a~%" else-words)
    ;(format t "then-words: ~a~%" rest-words)
    (let ((label (gensym "label")))
      (values rest-words
              (list
               ;; test already performed
               ;; if not successful, jump past if-words
               (translate-instruction `(,test-instruction ,label))
               (compile-word if-words)
               (compile-label label))))))

(defcompile =if 
  (compile-if 'bne words))


(defcompile /=if
  (compile-if 'beq words))




;; create a word that when called will allocate a new word on the
;; stack, and return the address of that word
;; when the code for the address of that word is executed,
;; the parameter variables defined will be on the stack and the words executed
(defcompile create
    (let* ((does (position 'does> words))
           (create-words (if does
                             (subseq words 0 does)
                             words))
           (does-words (if does
                           (subseq words (1+ does))
                           nil)))
      ;(compile-allocate-word-label)
      ;(compile-allocate-allocate-slots-from-stack)
      (format t "create words: ~a~%" create-words)
      (format t "does words: ~a~%" does-words)))

(defcompile code
    (let* ((end-code (position 'end-code words)))
      (assert end-code)
      ;(format t "assembled code: ~a~%" )
      ;; return words after code fragment
      (values (subseq words (1+ end-code))
              (mapcar #'translate-instruction (subseq words 0 end-code)))))

(defcompile recurse
  (let* ((code-sym (gethash *compiling-word* *code-word-syms*))
         (colon-sym (gethash *compiling-word* *colon-word-syms*))
         (sym (or code-sym colon-sym)))
    (if (eq :anonymous *compiling-word*)
        (values words
                (list (format nil "; recursive call to anonymous word not compiled")))
        (values words
                (list (format nil "    JMP ~a ; recursive call ~%" sym))))))
