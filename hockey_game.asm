	processor 6502
    include "vcs.h"
    include "macro.h"  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Segmento com variaveis não inicializadas
;;; Memória varia de $80 ate $FF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
P0XPos byte                 ; Player 0 - eixo-x 
P1XPos byte                 ; Player 1 - eixo-x 
P0YPos byte                 ; Player 0 - eixo-y 
P1YPos byte                 ; Player 1 - eixo-y 
PlayerHeight byte           ; Altura dos Players 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Começo da ROM (o jogo em si) em $F000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg code
    org $F000     

Start:
	CLEAN_START             ; Limpar a memória e registradores da TIA

    ldx #$08
    stx COLUBK              ; Cor do background (Cinza)

    ldx #$C6
    stx COLUPF              ; Cor do playfield (Verde)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iniciar Variáveis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #30
    sta P0XPos     

    lda #110
    sta P1XPos    
    
    lda #30
    sta P0YPos     

    lda #110
    sta P1YPos     

    lda #24
    sta PlayerHeight

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Renderizar o frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
NextFrame:
    lda #2
    sta VBLANK               ; Setando VBLANK on
    sta VSYNC                ; Setando VSYNC on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VSYNC (3 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 3
        sta WSYNC  ; first three VSYNC scanlines
    REPEND
    lda #0
    sta VSYNC      ; turn VSYNC off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VBLANK (30 linhas)
;;; Setar posição inicial do sprite enquanto estivermos no VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda P0XPos     ; Registrador A contém a posição no eixo-x desejada
    and #$7F       ; Operação AND com $7 para o valor ser positivo
    sta WSYNC      ; Espera scanline
    sta HMCLR      ; Limpa valores de posições horizontais
    sec            ; Setando a flag de carry antes da subtração

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Posicionando Player 0 no eixo-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DivideLoop:
    sbc #15        ; Subtrai 15 do do registrador A
    bcs DivideLoop ; Faz o loop enquanto a flag de carry está setada (ajuste GROSSO)

    eor #7         ; Ajusta o resto (da divisão) pra ficar entre -8 e 7 (ajuste FINO)
    asl            ; Operação de shift pois o registrador HMP0 apenas usa 4 bits
    asl
    asl
    asl
    sta HMP0       ; (Ajuste FINO)
    sta RESP0      ; 
    sta WSYNC      ; Espera scanline
    sta HMOVE      ; Aplica a posição fina no registrador

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda P1XPos     ; Registrador A contém a posição no eixo-x desejada
    and #$7F       ; Operação AND com $7 para o valor ser positivo
    sta WSYNC      ; Espera scanline
    sta HMCLR      ; Limpa valores de posições horizontais
    sec            ; Setando a flag de carry antes da subtração

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Posicionando Player 1 no eixo-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
DivideLoop1:
    sbc #15         ; Subtrai 15 do do registrador A
    bcs DivideLoop1 ; Faz o loop enquanto a flag de carry está setada (ajuste GROSSO)

    eor #7         ; Ajusta o resto (da divisão) pra ficar entre -8 e 7 (ajuste FINO)
    asl            ; Operação de shift pois o registrador HMP0 apenas usa 4 bits
    asl
    asl
    asl
    sta HMP1       ; (Ajuste FINO)
    sta RESP1      ; 
    sta WSYNC      ; Espera scanline
    sta HMOVE      ; Aplica a posição fina no registrador

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setar as próximas 35 linhas do VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 33
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK     ; Desliga VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alterando o registrador CTRLPF para permitir a reflexão do playfield 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #%00000001 
    stx CTRLPF
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanlines Principais (192 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Pulando 14 linhas, ou seja, tornando as preto (14 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 14
       sta WSYNC   
    REPEND

    ; Parte superior do playfield (7 linhas)
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Lateral do playfield (75 linhas)
    ldx #%00100000
    stx PF0
    ldx #%00000000
    stx PF1
    ldx #%10000000
    stx PF2
    REPEAT 75
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desenhando os players no eixo-x
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ldy #24 ; Tamanho dos sprites
DrawBitmap:
    lda P0Bitmap,Y ; Carrega o bitmap do Player 0
    sta GRP0       ; Armazena no registrador de graficos do Player 0
    lda P0Color,Y  ; Carrega a cor do Player 0
    sta COLUP0     ; Armazena no registrador de cor do Player 0

    lda P1Bitmap,Y ; Carrega o bitmap do Player 1
    sta GRP1       ; Armazena no registrador de graficos do Player 1
    lda P1Color,Y  ; Carrega a cor do Player 1
    sta COLUP1     ; Armazena no registrador de cor do Player 0

    sta WSYNC      ; Espera próxima linha

    dey
    bne DrawBitmap ; Repete próxima linha até terminar de desenhar

    lda #0
    sta GRP0       ; Desabilitar gráficos do player 0
    sta GRP1       ; Desabilitar gráficos do player 1

    REPEAT 66
        sta WSYNC
    REPEND

    ; Parte inferior do playfield (7 linhas)
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Pulando 14 linhas, ou seja, tornando as preto (14 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 14
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
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pular para o proximo frame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp NextFrame

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completar a ROM para 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start