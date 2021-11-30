
	processor 6502
        include "vcs.h"
        include "macro.h"        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variaveis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        seg.u Variables
	org $80

Temp		byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Code 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	seg Code
        org $f000

Reset
	CLEAN_START
        
             
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
StartFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VSYNC e VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; on VBLANK
    sta VSYNC                ; on VSYNC
    REPEAT 3
        sta WSYNC            ;  3  linhas do VSYNC
    REPEND
    lda #0
    sta VSYNC                ; off VSYNC
    REPEAT 31
        sta WSYNC            ; 31 linhas do VBLANK
    REPEND
    sta VBLANK               ; off VBLANK
    
    
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scanlines Principais -> 192
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



   
    ;lda #0
    ;sta PF0
    lda #%11111111
    sta PF1
    stx PF2
    REPEAT 14
       sta WSYNC   
    REPEND
    
   
    ;lda #0
    ;sta PF0
    ;lda #%11111111
    ;sta PF1
    ;stx PF2
   ; REPEAT 30
   ;    sta WSYNC   
   ; REPEND




GameVisibleLine:
    lda #$0C
    sta COLUBK               ;  background color
    lda #$00
    sta COLUPF               ; playfield  color 
    lda #%00000001
    sta CTRLPF               ;  playfield reflexo on  (00000001) off (00000000)
    lda #$F0
    sta PF0                  
    lda #0
    sta PF1                  
    lda #0
    sta PF2                 

    ldx #184              ;numero de scanlines
    
  
    
  
   ; lda #%11111111
   ; sta PF0
   ; lda #%11111111
    ;sta PF1
   ; lda #%11111111
   ; sta PF2
   ; REPEAT 7
   ;    sta WSYNC   
   ; REPEND
    
    
    
.GameLineLoop:
    sta WSYNC
    dex                      ; X--
    bne .GameLineLoop        ; loop game scanline 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overscan
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ;  on VBLANK 
    REPEAT 30
        sta WSYNC            ;  30 linhas de VBlank Overscan
    REPEND
    lda #0
    sta VBLANK               ; off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loop para StartFrame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp StartFrame           


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Final do cartucho

	org $fffc
        .word Reset	
        .word Reset	
