(in-package #:goof)

(defcode exit  `((addq (:imm 2) ,*rsp*)))

(defcode dup   `((move.w (:indirect ,*sp*) (:pre-dec ,*sp*))))

(defcode drop  `((addq (:imm 2) ,*sp*)))

(defcode 2drop `((addq (:imm 4) ,*sp*)))

(defcode 2dup  `((move.l ,*sp* (:pre-dec ,*sp*))))

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

;; (defcode =0)
;; (defcode =)
;; (defcode <)
;; (defcode <0)
;; (defcode >)
;; (defcode >0)
;; (defcode >=)
;; (defcode >=0)
;; (defcode <=0)

;; ;; general compare word, preferred to use this
;; ;; (or one of the compare-with-zero words),
;; ;; then use the contextually correct branching word
;; (defcode cmp)



;; (defcode =       `((cmp (:displacement 2 ,*sp*))
;;                     (:indirect ,*sp*)))


(defconst un 1)

(defcolon push1 (un dup +))

(defcolon square (dup +))

(defcolon push2 (push1 push1 +))

(defcolon yield (r>> swap >>r))

(defcolon yield2 (yield yield))

(defcompile if 
  (let ((then (position 'then words))
        (else (position 'else words)))
    (append (list 'setup)
            (subseq words 0 then)
            (list 'then-words)
            (if else
                (subseq words then else)
                (subseq words then))
            (when else (list 'else-words))
            (when else (subseq words else)))))


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
