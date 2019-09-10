;
; The name game for C64
;
; Commodore 64/128
; Assembly Language Programming
; Mark Andrews
;
; Originally written in Merlin and C64 Assembler
; Converted to DASM by Andy Johnson
;
; dasm name.asm -onasm.prg -v4
;
; C64
; LOAD"NAME.PRG",8,1
; SYS32768

; Directive to tell DASM which processor
; we want to use and where to put the program
; in the C64 Memory

        processor 6502
        org $8000

;
; Define some common characters
;

eol     equ $0d ;Return
eof     equ $03 ;EOF CHR
fillch  equ $20 ;SPACE

; Define the buffer length

buflen  equ 40

; Define a readable name for builtin

chrin   equ $ffcf
chrout  equ $ffd2
temptr  equ $fb

; All the sub-routines are defined up front
; so jump to the start of the name game

        jmp start

;
; Data Section
;

; Set aside some room for input

txtbuf  ds      40

; and declare some strings that
; are used by the name game

title   dc.b  "THE NAME GAME",$0d
hello   dc.b  "HELLO, ",$03
query   dc.b  "WHAT IS YOUR NAME?",$0d
name    dc.b  "GEORGE",$0d
rebuff  dc.b  "GO AWAY, ",$03
demand  dc.b  "BRING ME GEORGE!!",$0d
greet   dc.b  "HI, GEORGE!",$0d

;
; Clear Text Buffer by filling with spaces
;

fill    lda     #fillch         ; load a with $20
        ldx     #buflen         ; load x with buffer length`
dofill  dex                     ; decrement X
        sta     txtbuf,x        ; store $20 at txtbuf[x]
        bne     dofill          ; if X != 0 then goto dofill
        rts                     ; return
;
; Print Routine
;
; Whatever we print, the address of the string has
; to be in our temporary pointer and the line must
; end with an EOL character.

print   ldy     #0              ; Set index to 0 
show    lda     (temptr),y      ; Load the character stored at Y
        cmp     #eof            ; Compare Y to EOF
        beq     done            ; if Y = EOF then DONE
        pha                     ; Store a before call to chrout
        jsr     chrout          ; Print to the screen
        pla                     ; "pop" a from the stack
        cmp     #eol            ; Compare A to EOL
        bne     next            ; if a != EOL then next character
        jmp     done            ; Jump to done
next    iny                     ; a != EOL, increment Y
        cpy     #buflen         ; Compare Y to buffer length
        bcc     show            ; Y != bufflen so continue
done    rts                     ; return

;
; Print "THE NAME GAME"
;

start   lda     #eol            ; load A with EOL character
        jsr     chrout          ; print EOL to screen
        lda     #<title         ; load A with low byte of title string
        sta     temptr          ; store it at the temp pointer
        lda     #>title         ; load A with high byte of title string
        sta     temptr+1        ; store it at teh temp pointer + 1
        jsr     print           ; call print routine
        lda     #eol            ; load A witl EOL
        jsr     chrout          ; and print it

;
; Print "Hello . . ."
;

        lda     #<hello         ; load A with LSB of hello string
        sta     temptr          ; store LSB in temporary pointer
        lda     #>hello         ; load A with the MSB
        sta     temptr+1        ; store MSB in temporary pointer + 1
        jsr     print           ; call print routine

;
; Print "What is your name?"
;

ask     lda     #<query         ; load A with LSB of query string
        sta     temptr          ; store LSB in temporary pointer
        lda     #>query         ; load A with MSB of query string
        sta     temptr+1        ; store MSB in temporary pointer + 1
        jsr     print           ; call print routine
        lda     #eol            ; load a with EOL
        jsr     chrout          ; print it on the screen
;
; input a typed line
;

        jsr     fill            ; load the buffer with zeros
        ldx     #0              ; load X with 0 (our index)
key     jsr     chrin           ; get one character for the keyboard
        sta     txtbuf,x        ; place in txtbuf[x]
        cmp     #eol            ; compare A with EOL
        beq     compare         ; we are done so, go check if it's George
        inx                     ; increment X
        jmp     key             ; go get next character
;
; Is the name "George"?
; Interestingly, this would work right if we just
; had the name GEORGE's length stored some where but
; since the input always puts an EOL after the entry,
; it is used as the comparison terminator

compare jsr     chrout          ; A holds EOL so, just print it
        ldx     #0              ; load X, our index, with 0
check   lda     txtbuf,x        ; load A with txtbuf[x]
        cmp     name,x          ; compare A with name[x]
        bne     nogood          ; if it doesn't match, jump to nogood
        cmp     #eol            ; compare A to EOL
        beq     dunit           ; if it is equal, we are done
        inx                     ; increment X
        cpx     #buflen         ; compare X to buffer length
        bcs     dunit           ; if equal, jump to done
        jmp     check           ; get the next character
;
; No; Print "Go away . . ."
; No comments, by now, you should know
; However, these print something like
; Go Away, Andy
;
; Bring me George!!

nogood  lda     #eol
        jsr     chrout
        lda     #<rebuff
        sta     temptr
        lda     #>rebuff
        sta     temptr+1
        jsr     print

;
; Print player's name
;

        lda     #<txtbuf
        sta     temptr
        lda     #>txtbuf
        sta     temptr+1
        jsr     print
        lda     #eol
        jsr     chrout

;
; Print "Bring Me George!"
;

        lda     #<demand
        sta     temptr
        lda     #>demand
        sta     temptr+1
        jsr     print
        lda     #eol
        jsr     chrout
        jmp     ask

;
; The player did it so
; This routine prints
; Hi George!
;

dunit   lda     #eol
        jsr     chrout
        lda     #<greet
        sta     temptr
        lda     #>greet
        sta     temptr+1
        jsr     print
        rts
