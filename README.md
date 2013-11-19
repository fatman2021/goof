GOOF is a subroutine-threaded forth compiler for the motorola 68000,
designed for game development on the Sega Genesis (megadrive) game console.

I'm still working on adding in basic forth primitives, but it
currently supports defining 68k code words, colon words, and compiler
macros.






Basic usage:

```lisp
;; adds 2 to the stack pointer, in effect taking the top item off
(defcode   'drop `(addq (:imm 2), ,*sp*))
;; add the item top of the stack with the next item below, popping the top item off
(defcode   '+   `(add (:post-inc ,*sp*) (:indirect ,*sp*)))


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

;; test compiling an anonymous colon word with the two above code words
(compile-word '(drop + exit))
"   ; inlining DUP 
    ADDQ #2, A6
    ; inlining + 
    ADD.W (A6)+, (A6) 
    RTS
"
```

It inlines both dup and + because they are relatively short words (and
the CALL/RETURN overhead far outweighs the operations they perform)
