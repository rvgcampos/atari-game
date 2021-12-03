	processor 6502

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Arquivos requeridos com registradores do VCS e com algumas macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
    include "vcs.h"
    include "macro.h" 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Segmento com variaveis não inicializadas
;;; Memória varia de $80 ate $FF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
P0XPos byte                 ; Player 0 - eixo-x 
P1XPos byte                 ; Player 1 - eixo-x 
P0YPos byte                 ; Player 0 - eixo-y 
P1YPos byte                 ; Player 1 - eixo-y 
Score1  byte		    ;digitos pra scoreP0 
Score2  byte           	    ;digitos pra scoreP1
Timer  byte		    ;digitos pra timer
Temp   byte		    ;auxiliar para armazenar o valor do score
UnidadeDigitos	word	     ;digitos referentes das unidades
DezenaDigitos	word	     ;digitos referentes as dezenas
Score1Sprite   byte            ;arma
Score2Sprite   byte
TimerSprite  byte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definição de constantes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PLAYER0_HEIGHT = 24          ; Altura do sprite do player 0
PLAYER1_HEIGHT = 24          ; Altura do sprite do player 0
DIGITOS_HEIGHT = 5	     ; Altura do sprite dos digitos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Começo da ROM (o jogo em si) em $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000     

Start:
	CLEAN_START             ; Limpar a memória e registradores da TIA

    ldx #$08
    stx COLUBK              ; Cor do background (Cinza)

    ldx #$C6
    stx COLUPF              ; Cor do playfield (Verde)
    
  ; ldy #%00000010          ; CTRLPF D1 cor esquerda  P0 e direita P1 (score)
  ; sty CTRLPF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iniciar Variáveis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #30
    sta P0XPos     

    lda #110
    sta P1XPos    
    
    lda #10
    sta P0YPos     

    lda #50
    sta P1YPos  
    
    lda #01
    sta Score1                ; Score1 = 0
    lda #02
    sta Score2		      ; Score2 = 0
    lda #03
    sta Timer                 ; Timer = 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Renderizar o frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextFrame:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setando eixo-x do Player 0 e Player 1 e Digitos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda P0XPos
    ldy #0
    jsr SetObjectXPos        ; Setando a posição horizontal do Player 0

    lda P1XPos
    ldy #1
    jsr SetObjectXPos        ; Setando a posição horizontal do Player 1
    
    jsr CalculateDigitOffset  ; calculo dos digitos do score

    sta WSYNC
    sta HMOVE                ; Aplica a posição horizontal

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desenhar VSYNC e VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; Setando VBLANK on
    sta VSYNC                ; Setando VSYNC on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VSYNC (3 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        sta WSYNC  
    REPEND
    lda #0
    sta VSYNC      ; Desliga VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VBLANK (37 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 33;;;;;;;;;;;;;;;;;;;;;;;33 bote 33
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK     ; Desliga VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alterando o registrador CTRLPF para permitir a reflexão do playfield 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ldx #%00000001 
    stx CTRLPF
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanlines Principais (192 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Pulando 14 linhas, ou seja, tornando as cinza (Background) (14 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 14
       sta WSYNC   
    REPEND

    ; Parte superior do playfield (Verde) (7 linhas)
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Lateral do playfield (Verde) e Players (150 linhas)
    ldx #%00100000
    stx PF0
    ldx #%00000000
    stx PF1
    ldx #%10000000
    stx PF2
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    
    lda #0                  
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF
    ;sta COLUBK
   ; lda #$1E
  ;  sta COLUBK
    ;lda #$1C
    ;sta COLUPF
    ;lda #%00000000
    ;sta CTRLPF
    ;REPEAT 10
     ;   sta WSYNC
    ;REPEND
    
    lda #%00010010	; score mode + 2 pixel ball
        sta CTRLPF
        lda #$48
        sta COLUP0	; set color for left
        lda #$a8
        sta COLUP1
    
    ldx #DIGITOS_HEIGHT
    
.ScoreDigitLoop:
    ldy DezenaDigitos      ; get the tens digit offset for the Score
    lda Digitos,Y             ; load the bit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta Score1Sprite          ; save the score tens digit pattern in a variable

    ldy UnidadeDigitos     ; get the ones digit offset for the Score
    lda Digitos,Y             ; load the digit bit pattern from lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora Score1Sprite          ; merge it with the saved tens digit sprite
    sta Score1Sprite          ; and save it
    sta WSYNC                ; wait for the end of scanline
    sta PF1                  ; update the playfield to display the Score sprite
   
   
   


    ldy DezenaDigitos+1    ; get the left digit offset for the Timer
    lda Digitos,Y             ; load the digit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta Score2Sprite           ; save the timer tens digit pattern in a variable

    ldy UnidadeDigitos+1    ; get the ones digit offset for the Timer
    lda Digitos,Y             ; load digit pattern from the lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora Score2Sprite           ; merge with the saved tens digit graphics
    sta Score2Sprite          ; and save it

    jsr Sleep12Cycles        ; wastes some cycles

    sta PF1                  ; update the playfield for Timer display

    ldy Score1Sprite          ; preload for the next scanline
    sta WSYNC                ; wait for next scanline

    sty PF1                  ; update playfield for the score display
    inc DezenaDigitos
    inc DezenaDigitos+1
    inc UnidadeDigitos
    inc UnidadeDigitos+1    ; increment all digits for the next line of data

    jsr Sleep12Cycles        ; waste some cycles

    dex                      ; X--
    sta PF1                  ; update the playfield for the Timer display
    bne .ScoreDigitLoop      ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC
    
 
;ScoreLoop:
 ;   lda Digitos,Y
  ;  sta PF1
   ; sta WSYNC
    ;iny
    ;cpy #10
    ;bne ScoreLoop

    
   ; REPEAT 
    ;    sta WSYNC
    ;REPEND
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIM do Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
    
    
    

    ldx #75                  ; 2-scanline kernel
.GameLineLoop:
.AreWeInsideP0Sprite:
    txa                      ; Transferir X para A
    sec                      ; Setando carry flag antes da subtração
    sbc P0YPos               ; Subtrai de A o eixo-y do Player 0
    cmp PLAYER0_HEIGHT       ; Verifica se já chegou na altura correta para começar a desenhar o sprite
    bcc .DesenhaSpriteP0     ; Se chegou, chama DesenhaSpriteP0
    lda #0                   ; Caso não, indice = 0, imprime tudo preto
.DesenhaSpriteP0:
    tay                      ; Transfere A para Y
    lda P0Bitmap,Y           ; Carrega em A o bitmap do Player 0
    sta WSYNC                ; Espera scanline
    sta GRP0                 ; Habilita gráficos do Player 0
    lda P0Color,Y            ; Carrega em A as cores do Player 0
    sta COLUP0               ; Define as cores do Player 0

.AreWeInsideP1Sprite:
    txa                      ; Transferir X para A
    sec                      ; Setando carry flag antes da subtração
    sbc P1YPos               ; Subtrai de A o eixo-y do Player 1
    cmp PLAYER1_HEIGHT       ; Verifica se já chegou na altura correta para começar a desenhar o sprite
    bcc .DesenhaSpriteP1     ; Se chegou, chama DesenhaSpriteP1
    lda #0                   ; Caso não, indice = 0, imprime tudo preto
.DesenhaSpriteP1:
    tay                      ; Transfere A para Y
    lda P1Bitmap,Y           ; Carrega em A o bitmap do Player 1
    sta WSYNC                ; Espera scanline
    sta GRP1                 ; Habilita gráficos do Player 1
    lda P1Color,Y            ; Carrega em A as cores do Player 1
    sta COLUP1               ; Define as cores do Player 1

    dex                      ; X--
    bne .GameLineLoop        ; Repete até imprimir as (75 linhas)
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    
    lda #0                  
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF
    ;sta COLUBK
   ; lda #$1E
  ;  sta COLUBK
    ;lda #$1C
    ;sta COLUPF
    ;lda #%00000000
    ;sta CTRLPF
    ;REPEAT 10
     ;   sta WSYNC
    ;REPEND
    
    ;lda #%00010010	; score mode + 2 pixel ball
     ;   sta CTRLPF
      ;  lda #$48
       ; sta COLUP0	; set color for left
        ;lda #$a8
        ;sta COLUP1
    
    ldx #DIGITOS_HEIGHT
    
.ScoreDigitLoop1:
    ldy DezenaDigitos      ; get the tens digit offset for the Score
    lda Digitos,Y             ; load the bit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta Score1Sprite          ; save the score tens digit pattern in a variable

    ldy UnidadeDigitos     ; get the ones digit offset for the Score
    lda Digitos,Y             ; load the digit bit pattern from lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora Score1Sprite          ; merge it with the saved tens digit sprite
    sta Score1Sprite          ; and save it
    sta WSYNC                ; wait for the end of scanline
    sta PF1                  ; update the playfield to display the Score sprite
   
   
   


    ldy DezenaDigitos+1    ; get the left digit offset for the Timer
    lda Digitos,Y             ; load the digit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta Score2Sprite           ; save the timer tens digit pattern in a variable

    ldy UnidadeDigitos+1    ; get the ones digit offset for the Timer
    lda Digitos,Y             ; load digit pattern from the lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora Score2Sprite           ; merge with the saved tens digit graphics
    sta Score2Sprite          ; and save it

    jsr Sleep12Cycles        ; wastes some cycles

    sta PF1                  ; update the playfield for Timer display

    ldy Score1Sprite          ; preload for the next scanline
    sta WSYNC                ; wait for next scanline

    sty PF1                  ; update playfield for the score display
    inc DezenaDigitos
    inc DezenaDigitos+1
    inc UnidadeDigitos
    inc UnidadeDigitos+1    ; increment all digits for the next line of data

    jsr Sleep12Cycles        ; waste some cycles

    dex                      ; X--
    sta PF1                  ; update the playfield for the Timer display
    bne .ScoreDigitLoop1      ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC
    
 
;ScoreLoop:
 ;   lda Digitos,Y
  ;  sta PF1
   ; sta WSYNC
    ;iny
    ;cpy #10
    ;bne ScoreLoop

    
   ; REPEAT 
    ;    sta WSYNC
    ;REPEND
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FIM do Timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

    ; Parte inferior do playfield (Verde) (7 linhas)
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND
    
 

    ; Pulando 14 linhas, ou seja, tornando as cinza (14 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 4
        sta WSYNC
    REPEND
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desenhando as 30 linhas do VBLANK (overscan) (30 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2      
    sta VBLANK  
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK
   
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input para o player 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP0Up:
    lda #%00010000
    bit SWCHA
    bne CheckP0Down
    inc P0YPos

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    dec P0YPos

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    dec P0XPos

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput
    inc P0XPos

NoInput:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input para o player 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP1Up:
    lda #%00000001
    bit SWCHA
    bne CheckP1Down
    inc P1YPos

CheckP1Down:
    lda #%00000010
    bit SWCHA
    bne CheckP1Left
    dec P1YPos

CheckP1Left:
    lda #%00000100
    bit SWCHA
    bne CheckP1Right
    dec P1XPos

CheckP1Right:
    lda #%00001000
    bit SWCHA
    bne NoInput1
    inc P1XPos

NoInput1:

CheckCollisionP0PF:
    lda #%10000000
    bit CXP0FB
    bne .CollisionP0PF
    jmp EndCollisionCheck

.CollisionP0PF:
    dec P0XPos
    dec P0YPos
CheckCollisionP1PF:
    lda #%10000000
    bit CXP1FB
    bne .CollisionP1PF
    jmp EndCollisionCheck

.CollisionP1PF:
    inc P1XPos
    inc P1YPos
    
EndCollisionCheck:
    sta CXCLR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pular para o proximo frame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp NextFrame

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subrotina para lidar com a posição fina no eixo-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A guarda a posição no eixo-x
;;; Y é o objeto (0:player0, 1:player1, 2:missile0, 3:missile1, 4:ball)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

SetObjectXPos subroutine
    sta WSYNC                ; Começa uma scanline
    sec                      ; Setar carry flag
.Div15Loop
    sbc #15                  ; Subtrai 15 do registrador A
    bcs .Div15Loop           ; Loop até carry flag ficar limpa
    eor #7                   ; Torna o range entre -8 to 7
    asl
    asl
    asl
    asl                      ; 4 shift lefts para obter apenas 4 bits mais significativos
    sta HMP0,Y               ; Armazena a posição "fina" em HMxx
    sta RESP0,Y              ; Fixa a posição do objeto 
    rts
    
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subrotina para lidar com os digitos do Score
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                   ; X register is the loop counter
.PrepareScoreLoop            ; this will loop twice, first X=1, and then X=0

    lda Score1,X              ; load A with Timer (X=1) or Score (X=0)
    and #$0F                 ; remove the tens digit by masking 4 bits 00001111
    sta Temp                 ; save the value of A into Temp
    asl                      ; shift left (it is now N*2)
    asl                      ; shift left (it is now N*4)
    adc Temp                 ; add the value saved in Temp (+N)
    sta UnidadeDigitos,X    ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score1,X              ; load A with Timer (X=1) or Score (X=0)
    and #$F0                 ; remove the ones digit by masking 4 bits 11110000
    lsr                      ; shift right (it is now N/2)
    lsr                      ; shift right (it is now N/4)
    sta Temp                 ; save the value of A into Temp
    lsr                      ; shift right (it is now N/8)
    lsr                      ; shift right (it is now N/16)
    adc Temp                 ; add the value saved in Temp (N/16+N/4)
    sta DezenaDigitos,X    ; store A in TensDigitOffset+1 or TensDigitOffset

    dex                      ; X--
    bpl .PrepareScoreLoop    ; while X >= 0, loop to pass a second time

    rts
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to waste 12 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsr takes 6 cycles
;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts   
    
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lookup table para os bitmaps dos players
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Bitmap:
    .byte #%00000000;$80
    ;.byte #%01100001;$80
    ;.byte #%01000010;$80
    ;.byte #%01000100;$80
    ;.byte #%01001000;$70
    ;.byte #%01010000;$42
    ;.byte #%01100000;$42
    ;.byte #%01000000;$42
    ;.byte #%01000000;$3A
    ;.byte #%00011100;$38
    ;.byte #%11111000;$36
    
    ;pernas
   	.byte #%11110111;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    
    ;corpo
    .byte #%11100110;$30
    .byte #%11100100;$30
    .byte #%11101100;$30
    .byte #%11101000;$30
    .byte #%11101000;$30
    .byte #%11111000;$30
    .byte #%11111000;$30
    .byte #%11111000;$30
    
    ;cabeça
    .byte #%11111000;$36
    .byte #%11001000;$36
    .byte #%11011000;$36
    .byte #%11111000;$36
    .byte #%11101000;$36
    .byte #%11110000;$36
    .byte #%11111100;$30
    .byte #%11110000;$30

P1Bitmap:
    .byte #%00000000;$80
    ;.byte #%10000110;$80
    ;.byte #%01000010;$80
    ;.byte #%00100010;$80
    ;.byte #%00010010;$70
    ;.byte #%00001010;$42
    ;.byte #%00000110;$42
    ;.byte #%00000010;$42
    ;.byte #%00000010;$3A
    
     ;pernas
   	.byte #%11101111;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    
    ;corpo
    .byte #%01100111;$32
    .byte #%00100111;$32
    .byte #%00100111;$32
    .byte #%00110111;$32
    .byte #%00010111;$32
    .byte #%00010111;$32
    .byte #%00011111;$32
    .byte #%00011111;$32
    
    ;cabeça
    .byte #%00011111;$34
    .byte #%00010011;$34
    .byte #%00011011;$34
    .byte #%00011111;$34
    .byte #%00010111;$34
    .byte #%00011111;$34
    .byte #%00111111;$30
    .byte #%00001111;$30

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lookup table para cores dos players
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    .byte #$00;
    ;.byte #$80;
    ;.byte #$80;
    ;.byte #$80;
    ;.byte #$70;
    ;.byte #$42;
    ;.byte #$42;
    ;.byte #$42;
    ;.byte #$3A;

    ;cor das pernas
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    
    ;cor do corpo
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    
    ;cor da cabeça
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$30;
    .byte #$30;

P1Color:
    .byte #$00;
    ;.byte #$80;
    ;.byte #$80;
    ;.byte #$80;
    ;.byte #$70;
    ;.byte #$42;
    ;.byte #$42;
    ;.byte #$42;
    ;.byte #$3A;
    
    ;cor das pernas
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    
    ;cor do corpo
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    
    ;cor da cabeça
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$30;
    .byte #$30;
    
    
    
;Numbers:
    ;.byte #%00001110   ; ########
    ;.byte #%00001110   ; ########
    ;.byte #%00000010   ;      ###
    ;.byte #%00000010   ;      ###
    ;.byte #%00001110   ; ########
    ;.byte #%00001110   ; ########
    ;.byte #%00001000   ; ###
    ;.byte #%00001000   ; ###
    ;.byte #%00001110   ; ########
   ; .byte #%00001110   ; ########
    
Digitos:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completar a ROM para 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start
