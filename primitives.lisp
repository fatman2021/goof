(in-package #:goof)

;; exit from a colon word (though this pretty much always gets
;; converted into a straight 'rts' in it's callee, which has the same effect)
(defcode exit  `((addq (:imm 2) ,*rsp*)))

;; duplicate the top item on the stack
(defcode dup   `((move.w (:indirect ,*sp*) (:pre-dec ,*sp*))))

;; drop the top item off the stack
;; old tos in memory version
;; (defcode drop  `((addq (:imm 2) ,*sp*)))
;; tos in d7 version
;; (defcode drop `((move.w (:post-inc a6) d7)))

(defcode 2drop `((addq (:imm 4) ,*sp*)))

(defcode 2dup  `((move.l ,*sp* (:pre-dec ,*sp*))))

;; ( a b -- a b a )
;; old tos in memory version
;; (defcode over  `((move.w (:displacement 2 ,*sp*) (:pre-dec ,*sp*))))

;;( (a6) d7 -- (a6) d7 (a6) )
;; tos in d7 version
(defcode over `((move.w (:indirect a6) d6)
                (move.w d7 (:pre-dec a6))
                (move.w d6 d7)))


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

;; old tos in memory version
;; (defcode swap  `((move.l (:indirect ,*sp*) d7)
;;                  (swap d7)
;;                  (move.l d7 (:indirect ,*sp*))))

;; tos in d7 version
(defcode swap `((move.w (:indirect ,*sp*) d6)
                (move.w d7 (:indirect ,*sp*))
                (move.w d6 d7)))


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
;; old tos in memory version
;; (defcode =0  `((tst.w (:indirect ,*sp*))))
;; tos in d7 version
(defcode =0  `((tst.w d7)))


;; ;; general compare word, preferred to use this
;; ;; (or one of the compare-with-zero words),
;; ;; then use the contextually correct branching word

;; old tos in memory version
;; (defcode = `((cmp (:indirect ,*sp*) (:indirect ,*sp*))))
;; (defcode cmp `((cmp (:displacement 2 ,*sp*) (:indirect ,*sp*))))

;; tos in d7 version
(defcode = `((cmp (:indirect ,*sp*) d7)))
(defcode cmp `((cmp (:indirect ,*sp*) d7)))


;; first operand is the divisor
;; remainder is second on stack
(defcode /mod `((moveq.l (:imm 0) d6)
                (moveq.l (:imm 0) d7)
                (move.w (:post-inc ,*sp*) d7)
                (move.w (:post-inc ,*sp*) d6)
                (divs.w d7 d6)
                (swap d6)
                (move.l d6 (:pre-dec ,*sp*))))

;; old tos in memory
;; (defcode mod  `((moveq.l (:imm 0) d6)
;;                 (moveq.l (:imm 0) d7)
;;                 (move.w (:post-inc ,*sp*) d7)
;;                 (move.w (:post-inc ,*sp*) d6)
;;                 (divs.w d7 d6)
;;                 (swap d6)
;;                 (move.w d6 (:pre-dec ,*sp*))))

;; tos in d7 version
(defcode mod `((moveq.l (:imm 0) d6)
               (move.w (:post-inc ,*sp*) d6)
               (divs.w d7 d6)
               (swap d6)
               (move.w d6 d7)))

;; (defcode =)
;; (defcode <)
;; (defcode <0)
;; (defcode >)
;; (defcode >0)
;; (defcode >=)
;; (defcode >=0)
;; (defcode <=0)





;; swap ( a b -- b a )
;; over ( b a -- b a b )

;; ( a d7 -- b a d7 )
;; tos in d7
(defcode swap-over
    `((move.w (:indirect a6) d6)
      (move.w d7 (:indirect a6))
      (move.w d6 (:pre-dec a6))))

;; swap-over ( a b -- b a b )
;; move.w (a6), d6
;; move.w d7, (a6)
;; move.w d6, (a6)


(defcolon gcd (=0 /=if swap-over mod recurse then drop))


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
                (list (format nil "    ; skipping recursive call to anonymous word ~%")))
        (values words
                (list (format nil "    ; recursive call ~%    JMP ~a~%" sym))))))


;; 123 33 gcd took 1736 cycles with tos in memory and inlining set to 3
;; 123 33 gcd took 1400 cycles with tos in memory and inlining set to 8

;; 123 33 gcd took 1464 cycles with tos in d7 and inlining set to 4
;; 123 33 gcd took 1284 cycles with tos in d7 and inlining set to 8

;; 123 33 gcd took 1204 cycles with tos in d7, inlining set to 8, and
;; with swap-over word (rather than swap over)

;; 123 33 gcd took 2722 cycles with tos in memory, and no inlining

;; swap-over ( a b -- b a b )
;; move.w (a6), d6
;; move.w d7, (a6)
;; move.w d6, (a6)

