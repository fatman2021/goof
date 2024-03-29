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
(defcode drop `((move.w (:post-inc a6) d7)))

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


;; return stack

(defcode r>    `((move.w (:post-inc ,*rsp*) (:pre-dec ,*sp*))))

(defcode r>>   `((move.l (:post-inc ,*rsp*) (:pre-dec ,*sp*))))

(defcode >r    `((move.w (:post-inc ,*sp*)  (:pre-dec ,*rsp*))))

(defcode >>r   `((move.l (:post-inc ,*sp*)  (:pre-dec ,*rsp*))))


;; arithmetic operations

(defcode +     `((add.w (:post-inc ,*sp*)
                         d7)))

(defcode -      `((sub.w (:post-inc ,*sp*)
                         d7)))

(defcode *     `((muls.w (:post-inc ,*sp*)
                         d7)))

(defcode 2*    `((lsl (:imm 1) d7)))

(defcode 4*    `((lsl (:imm 2) d7)))

(defcode 2/    `((rsl (:imm 1) d7)))

(defcode 4/    `((rsl (:imm 2) d7)))

(defcode inc   `((addq (:imm 1) d7)))

(defcode 2+    `((addq (:imm 2) d7)))

(defcode 4+    `((addq (:imm 4) d7)))

(defcode dec   `((subq (:imm 1) d7)))


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


(defcode execute `((jmp (:indirect ,*rsp*))))

(defcode ?dup  (let ((name (gensym "label")))
                 `((tst (:indirect ,*sp*))
                   (beq ,name)
                   (move.w (:indirect ,*sp*) (:pre-dec ,*sp*))
                   (:label ,name))))


;;; comparison operators


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


;;;  memory operations

;; caution, these do not drop the addresses provided to them off the stack
;; because dropping unecessary values is faster than duplicating them beforehand
 
(defcode @  ;; ( addr -- addr val-at-address )
    `((move.w d7 a5)
      (move.w (:indirect a5) d7)))

(defcode ! ;; ( addr val -- addr )
    `((move.w (:indirect a6) a5)
      (move.w d7 (:indirect a5))
      (move.w a5 d7)))

;; reads the value onto the stack and increments the address
(defcode @+ ;; ( addr -- addr+1 val-at-address )
    `((move.w d7 a5)
      (move.w (:post-inc a5) d7)
      (move.w a5 (:pre-dec a6))))

;; stores a value into memory and increments the address
(defcode !+ ;; ( addr val -- addr+1 )
    `((move.w (:post-inc a6) a5)
      (move.w d7 (:post-inc a5))
      (move.w a5 d7)))

;; copies a byte from the source address to the destination address
(defcode b@ ; ( src-addr dest-addr -- src-addr dest-addr )
  `((move.w (:indirect a6) a5)
    (move.w d7 a4)
    (move.b (:indirect a5) (:indirect a4))))

;; copies a word from the source address to the destination address
(defcode w@ ; ( src-addr dest-addr -- src-addr dest-addr )
  `((move.w (:indirect a6) a5)
    (move.w d7 a4)
    (move.w (:indirect a5) (:indirect a4))))

;; copies a longword from the source address to the destination address
(defcode lw@ ; ( src-addr dest-addr -- src-addr dest-addr )
  `((move.w (:indirect a6) a5)
    (move.w d7 a4)
    (move.l (:indirect a5) (:indirect a4))))


;; copies a byte from the source address to the destination address
;; incrementing both addresses
(defcode b@+ ; ( src-addr dest-addr -- src-addr dest-addr )
  `((move.w (:indirect a6) a5)
    (move.w d7 a4)
    (move.b (:post-inc a5) (:post-inc a4))
    (move.w a5 (:indirect a6))
    (move.w a4 d7)))

;; copies a word from the source address to the destination address
;; incrementing both addresses
(defcode w@+ ; ( src-addr dest-addr -- src-addr dest-addr )
  `((move.w (:indirect a6) a5)
    (move.w d7 a4)
    (move.w (:post-inc a5) (:post-inc a4))
    (move.w a5 (:indirect a6))
    (move.w a4 d7)))

;; copies a longword from the source address to the destination address
;; incrementing both address
(defcode lw@+ ; ( src-addr dest-addr -- src-addr dest-addr )
  `((move.w (:indirect a6) a5)
    (move.w d7 a4)
    (move.l (:post-inc a5) (:post-inc a4))
    (move.w a5 (:indirect a6))
    (move.w a4 d7)))



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



(defconst word-size 2)
(defconst stack-base *stack-address*)
(defvariable free-pt)


; translates address



(defcolon cells  ;; ( n -- n*cell-size )
    ( word-size * ))


(defcolon alloc  ;; ( n -- addr )
    ( cells free-pt@ dup rot + free-pt!))



(let ((rstack-base-var-address (allocate-new-var-cell))
      (free-pt-address (allocate-new-var-cell)))
  (defcode rstack-base ;; ( -- rstack-base )
      `((move.w d7 (:pre-dec a6))
        (move.w (:absolute ,rstack-base-var-address) d7)))
  (defcode set-rstack-base ;; ( n -- )
      `((move.w d7 (:absolute ,rstack-base-var-address))
        (move.w (:post-inc a6) d7)))
  (defcode free-pt ;; ( -- free-pt-address )
      `((move.w d7 (:pre-dec a6))
        (move.l (:absolute ,free-pt-address) d7)))
  (defcode set-free-pt ;; ( addr -- )
      `((move.w d7 (:absolute ,free-pt-address))
        (move.w (:post-inc a6) d7))))


(defcolon alloc-new-var-cell
    ( 1 alloc ))


;; this shit is complicated
;; we need to have some sort of mapping between words and created
;; words at runtime
(defcompile create
  (let* ((does (position 'does> words))
         (create-words (if does
                           (subseq words 0 does)
                           words))
         (does-words (if does
                         (subseq words (+ 1 does)))))
    (format t "create words: ~a~% does words: ~a~%" create-words does-words)
    (values
     nil
     (list (compile-word create-words)
           (compile-word does-words)))
    ;; allocate one variable cell for each word on the stack
    ;; for does, move variables in each cell onto the stack
    ;; before executing words
    ))
