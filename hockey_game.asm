
	processor 6502
    include "vcs.h"
    include "macro.h"  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Segmento com variaveis não inicializadas
;;; Memória varia de $80 ate $FF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    seg.u Variables
    org $80
P0XPos byte                 ; Sprite X coordinate 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Começo da ROM em $F000
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
;;; Iniciar Variavel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #30
    sta P0XPos     ; initialize player X coordinate

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setar as próximas 35 linhas do VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    REPEAT 35
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK     ; turn VBLANK off

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alterando o registrador CTRLPF para permitir a reflexão do playfield 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldx #%00000001 
    stx CTRLPF
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scanlines Principais (192 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ; Pulando 14 linhas, ou seja, tornando as preto
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 14
       sta WSYNC   
    REPEND

    ; Parte superior do playfield
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Lateral do playfield
    ldx #%00100000
    stx PF0
    ldx #%00000000
    stx PF1
    ldx #%10000000
    stx PF2
    REPEAT 75
        sta WSYNC
    REPEND

    ldy #9
DrawBitmap:
    lda P0Bitmap,Y ; load player bitmap slice of data
    sta GRP0       ; set graphics for player 0 slice

    lda P0Color,Y  ; load player color from lookup table
    sta COLUP0     ; set color for player 0 slice

    sta WSYNC      ; wait for next scanline

    dey
    bne DrawBitmap ; repeat next scanline until finished

    lda #0
    sta GRP0       ; disable P0 bitmap graphics

    ; Lateral do playfield
    /* ldx #%00100000
    stx PF0
    ldx #%00000000
    stx PF1
    ldx #%10000000
    stx PF2 */
    REPEAT 66
        sta WSYNC
    REPEND

    ; Parte inferior do playfield
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND


    ; Pulando 14 linhas, ou seja, tornando as preto
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 14
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desenhando as 30 linhas do VBLANK (overscan)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2      
    sta VBLANK  
    REPEAT 30
        sta WSYNC
    REPEND
    lda #0
    sta VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pular para o proximo frame 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    jmp NextFrame


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Bitmap:
    .byte #%00000000;$80
    .byte #%01100001;$80
    .byte #%01000010;$80
    .byte #%01000100;$80
    .byte #%01001000;$70
    .byte #%01010000;$42
    .byte #%01100000;$42
    .byte #%01000000;$42
    .byte #%01000000;$3A

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup table for the player colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
P0Color:
    .byte #$00;
    .byte #$80;
    .byte #$80;
    .byte #$80;
    .byte #$70;
    .byte #$42;
    .byte #$42;
    .byte #$42;
    .byte #$3A;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completar a ROM para 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start