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
ScoreP0 byte                ; Placar do Player 0
ScoreP1 byte                ; Placar do Player 1
Temp byte                   ; Variável temporária para guardar valores do placar
OnesDigitOffset word        ; lookup table offset for the score 1's digit
TensDigitOffset word        ; lookup table offset for the score 10's digit
ScoreP0Sprite               ; Store the sprite bit pattern for the score
ScoreP1Sprite               ; Store the sprite bit pattern for the timer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definição de constantes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PLAYER0_HEIGHT = 24          ; Altura do sprite do player 0
PLAYER1_HEIGHT = 24          ; Altura do sprite do player 0
DIGITS_HEIGHT = 5             ; Altura do 'sprite' dos digitos

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Começo da ROM (o jogo em si) em $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000     

Start:
	CLEAN_START             ; Limpar a memória e registradores da TIA

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

    lda #2
    sta ScoreP0
    lda #3
    sta ScoreP1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Renderizar o frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextFrame:
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
    lda #0
    sta COLUBK
    REPEAT 33
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setando eixo-x do Player 0 e Player 1 (VBLANK)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda P0XPos
    ldy #0
    jsr SetObjectXPos        ; Setando a posição horizontal do Player 0

    lda P1XPos
    ldy #1
    jsr SetObjectXPos        ; Setando a posição horizontal do Player 1

    jsr CalculateDigitOffset ; Calcular o placar

    sta WSYNC
    sta HMOVE                ; Aplica a posição horizontal

    lda #0
    sta VBLANK               ; Desliga VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desenhar o placar (20 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta COLUBK
    sta CTRLPF

    lda #$1E                    ; Amerelo
    sta COLUPF
    
    ldx #DIGITS_HEIGHT          ; X guarda o valor de 5
.ScoreDigitLoop:
    ldy TensDigitOffset         ; get the tens digit offset for the Score
    lda Digits,Y                ; load the bit pattern from lookup table
    and #$F0                    ; Mask/remove the graphics for the ones digit
    sta ScoreP0Sprite           ; Save the score tens digit pattern in variable

    ldy OnesDigitOffset         ; get the ones digit offset for the Score
    lda Digits,Y                ; load the bit pattern from lookup table
    and #$0F                    ; Mask/remove the graphics for the tens digit
    ora ScoreP0Sprite           ; merge it with the saved tens digit sprite
    sta ScoreP0Sprite           ; and saved it
    sta WSYNC                   ; wait for the end of scanline
    sta PF1                     ; update the playfield to display for the Score Sprite

    ldy TensDigitOffset+1       ; get the left digit of Player 1
    lda Digits,Y                ; load the bit pattern from lookup table
    and #$F0                    ; Mask/remove the graphics for the ones digit
    sta ScoreP1Sprite           ; Save the score tens digit pattern in variable

    ldy OnesDigitOffset+1         ; get the ones digit offset for the Score
    lda Digits,Y                ; load the bit pattern from lookup table
    and #$0F                    ; Mask/remove the graphics for the tens digit
    ora ScoreP1Sprite           ; merge it with the saved tens digit sprite
    sta ScoreP1Sprite           ; and saved it

    jsr Sleep12Cycles           ; waste some cycles

    sta PF1                     ; update the playfield for Timer display

    ldy ScoreP0Sprite           ; preload for next scanline
    sta WSYNC                   ; wait for next scanline

    sty PF1                     ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles           ; waste some cycles

    dex                         ; X--
    sta PF1                     ; update the playfield for the Timer display
    bne .ScoreDigitLoop         ; Se dex != 0, então pule para ScoreDigitLoop

    sta WSYNC

    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta WSYNC
    sta WSYNC
    sta WSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanlines Principais (192 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Pulando 14 linhas, ou seja, tornando as cinza (Background) (14 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2 

    ldx #$08
    stx COLUBK              ; Cor do background (Cinza)
    ldx #$C6
    stx COLUPF              ; Cor do playfield (Verde)
    ldx #%00000001 
    stx CTRLPF

    REPEAT 3
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

    ldx #84                  ; 2-scanline kernel
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
    REPEAT 3
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
    lda P0YPos
    cmp #60
    bpl CheckP0Down
    inc P0YPos

CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    lda P0YPos
    cmp #2
    bmi CheckP0Left
    dec P0YPos

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    lda P0XPos
    cmp #2
    bmi CheckP0Right
    dec P0XPos

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput0
    lda P0XPos
    cmp #58
    bpl NoInput0
    inc P0XPos

NoInput0:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input para o player 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP1Up:
    lda #%00000001
    bit SWCHA
    bne CheckP1Down
    lda P1YPos
    cmp #60
    bpl CheckP1Down
    inc P1YPos

CheckP1Down:
    lda #%00000010
    bit SWCHA
    bne CheckP1Left
    lda P1YPos
    cmp #3
    bmi CheckP1Left
    dec P1YPos

CheckP1Left:
    lda #%00000100
    bit SWCHA
    bne CheckP1Right
    lda P1XPos
    cmp #78
    bmi CheckP1Right
    dec P1XPos

CheckP1Right:
    lda #%00001000
    bit SWCHA
    bne NoInput1
    lda P1XPos
    cmp #134
    bpl NoInput1
    inc P1XPos

NoInput1:

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
;;; Subrotina para lidar com o placar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Converte os nibbles das variáveis ScoreP0 e ScoreP1
;;; em offsets para que assim seja possível exibir os valores do placar
;;; Cada dígito tem uma altura de 5 bytes.
;;;
;;; Para cada low nibble precisamos multiplicar por 5
;;;     - Podemos usar left shifts para multiplicar por 2
;;;     - Para qualquer número N, o valor 5N = (N*2*2)+N
;;; Para cada upper nibble, já que é multiplicado por 16, precisamos
;;; dividir por 16 e multiplicar por 5:
;;;     - Podemos usar right shifts para dividir por 2
;;;     - Para qualquer número N, o valor de (N/16)*5 = (N/2/2) +(N/2/2/2/2) 
CalculateDigitOffset subroutine
    ldx #1                   ; Contador do loop
.PrepareScoreLoop            ; Var rodar o loop duas vezes, primeiro X=1 e depois com X=0
    lda ScoreP0,X            ; Colocar o valor de A em ScoreP1(X=1) ou ScoreP0(X=0)
    and #$0F                 ; Remove os 10-digitos pela mascara de 4 bits 00001111
    sta Temp                 ; Save o valor na variável temporária
    asl                      ; Shift left (agora é N*2)
    asl                      ; Shift left (agora é N*2)
    adc Temp                 ; Adicionar o valor salvo na variável Temp (+N)
    sta OnesDigitOffset,X    ; Salva o valor de A em OnesDigitOffset+1 ou OnesDigitOffset+0

    lda ScoreP0,X              ; Colocar o valor de A em ScoreP1(X=1) ou ScoreP0(X=0)
    and #$F0                 ; Remove os 1-digitos pela mascara de 4 bits 11110000
    lsr                      ; Shift right (agora é N/2)  
    lsr                      ; Shift right (agora é N/4)  
    sta Temp                 ; Salva o valor de A na variável Temp
    lsr                      ; Shift right (agora é N/8)
    lsr                      ; Shift right (agora é N/16)
    adc Temp                 ; adiciona o valor guardado em Temp (N/16 + N/4)
    sta TensDigitOffset,X    ; Salva o valor de A em TensDigitOffset+1 ou TensDigitOffset+0

    dex
    bpl .PrepareScoreLoop    ; Enquanto X>=0, faz o loop

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subrotina para gastar 12 ciclos de CPU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; jsr takes 6 cycles
;;; rts takes 6 cycles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lookup table para os digitos
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Digits:
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lookup table para os bitmaps dos players
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Bitmap:
    .byte #%00000000;$80
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completar a ROM para 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start