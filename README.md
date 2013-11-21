GOOF is a forth compiler for the motorola 68000,
designed for game development on the Sega Genesis (Megadrive) game console.

GOOF stands for Game-oriented object forth, which is inspired by the GOOL language used in Crash Bandicoot.


It's still a work-in-progress, but it currently supports defining 68k code words, colon words, and compiler macros.




Example of usage:

```lisp
;; adds 2 to the stack pointer, in effect taking the top item off
(defcode   'drop `(addq (:imm 2), ,*sp*))

;; add the item top of the stack with the next item below, popping the top item off
(defcode   '+   `(add.w (:post-inc ,*sp*) (:indirect ,*sp*)))


(compile-word 'drop)
"code1032: 
    ; definition for DROP 
    ADDQ #2, A6 
    RTS 
"


(compile-word '+)
"code1039: 
    ; definition for + 
    ADD.W (A6)+, (A6) 
    RTS 
"

;; test compiling an anonymous colon word with the above defined code words
(compile-word '(drop swap + exit))
"    ; inlining DROP 
    ADDQ #2, A6 
    JSR code1050 ; call to SWAP (defined elsewhere)
    ; inlining + 
    ADD.W (A6)+, (A6) 
    RTS
"

(defvariable counter) ;; define a variable in RAM 

;; defines the words counter, counter@, and counter!
;; read as counter (push the variable location on the stack),
;; counter-read (push the value of the variable on the stack),
;; and counter-set (set the variable to the value on the top of the stack)


(compile-word '(counter))

"   MOVE.W d7, -(a6)     ;; push rest of stack downward
    MOVE.W #4096, d7     ;; push address on stack
"
(compile-word '(counter@))
"   MOVE.W D7, -(A6)     ;; push rest of stack downward
    MOVE.W 4096, D7      ;; push value at address on stack
"
(compile-word '(counter!))
"   MOVE.W D7, 4096      ;; store value on top of stack at address
    MOVE.W (A6)+, D7     ;; pop rest of stack upward
"
```
