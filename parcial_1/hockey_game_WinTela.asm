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
ScoreP0Sprite byte               ; Store the sprite bit pattern for the score
ScoreP1Sprite    byte           ; Store the sprite bit pattern for the timer
BallXPos byte
BallYPos byte
IndoDireita byte
IndoCima byte
Random byte
Random1 byte
velocidadex byte
BackgroundColor byte
CharAnimOffset byte
CharAnimOffset1 byte
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

    lda #0
    sta ScoreP0
    lda #0
    sta ScoreP1

    lda #70
    sta BallXPos 

    lda #80
    sta BallYPos  
    
    lda #1
    sta IndoDireita
    
    lda #0
    sta IndoCima
    
    lda #%11010100
    sta Random
    
    lda #%11010100
    sta Random1
    
    lda #$08
    sta BackgroundColor

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACRO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_BALL
        lda #0
    	cpx BallYPos
        bne .SkipMissileDraw
.DrawMissile:
	lda #%00000010
.SkipMissileDraw
        sta ENABL        
    ENDM

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

    lda BallXPos
    ldy #4
    jsr SetObjectXPos

    jsr CalculateDigitOffset ; Calcular o placar
    jsr GenerateSoundGame


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
    
    ldx #DIGITS_HEIGHT         ; X guarda o valor de 5
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
    
    lda #0
    sta AUDV0

    ldx BackgroundColor
    stx COLUBK              ; Cor do background (Cinza)
    ldx #$C6
    stx COLUPF              ; Cor do playfield (Verde)
    ldx #%00110001 
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
    ldx #%00000000
    stx PF2

    ldx #84                  ; 2-scanline kernel
.GameLineLoop:
    DRAW_BALL
.AreWeInsideP0Sprite:
    txa                      ; Transferir X para A
    sec                      ; Setando carry flag antes da subtração
    sbc P0YPos               ; Subtrai de A o eixo-y do Player 0
    cmp PLAYER0_HEIGHT       ; Verifica se já chegou na altura correta para começar a desenhar o sprite
    bcc .DesenhaSpriteP0     ; Se chegou, chama DesenhaSpriteP0
    lda #0                   ; Caso não, indice = 0, imprime tudo preto
.DesenhaSpriteP0:
    clc
    adc CharAnimOffset	
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
    clc
    adc CharAnimOffset1
    tay                      ; Transfere A para Y
    lda P1Bitmap,Y           ; Carrega em A o bitmap do Player 1
    sta WSYNC                ; Espera scanline
    sta GRP1                 ; Habilita gráficos do Player 1
    lda P1Color,Y            ; Carrega em A as cores do Player 1
    sta COLUP1               ; Define as cores do Player 1

    dex                      ; X--
    bne .GameLineLoop        ; Repete até imprimir as (75 linhas)
    
    lda #0
    sta CharAnimOffset
    lda #0
    sta CharAnimOffset1

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
    lda #25
    sta CharAnimOffset
CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    lda P0YPos
    cmp #2
    bmi CheckP0Left
    dec P0YPos
    lda #50
    sta CharAnimOffset

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    lda P0XPos
    cmp #3
    bmi CheckP0Right
    dec P0XPos
    lda #0
    sta CharAnimOffset

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne CheckP0Buttom;NoInput0
    lda P0XPos
    cmp #58
    bpl  CheckP0Buttom;NoInput0;CheckP0Buttom:
    inc P0XPos
    lda #0
    sta CharAnimOffset
    
    
    
CheckP0Buttom:
	 lda #%10000000
	bit INPT4
        bne NoInput0
        jmp NextFrame

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
    lda #25
    sta CharAnimOffset1

CheckP1Down:
    lda #%00000010
    bit SWCHA
    bne CheckP1Left
    lda P1YPos
    cmp #3
    bmi CheckP1Left
    dec P1YPos
    lda #50
    sta CharAnimOffset1

CheckP1Left:
    lda #%00000100
    bit SWCHA
    bne CheckP1Right
    lda P1XPos
    cmp #78
    bmi CheckP1Right
    dec P1XPos
    lda #0
    sta CharAnimOffset1

CheckP1Right:
    lda #%00001000
    bit SWCHA
    bne NoInput1
    lda P1XPos
    cmp #132
    bpl NoInput1
    inc P1XPos
    lda #0
    sta CharAnimOffset1

NoInput1:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CheckCollisionP0Ball:
    lda #%01000000
    bit CXP0FB 
    bne .CollisionPOBL
    jmp CheckCollisionP1Ball
.CollisionPOBL:
    lda #1
    sta IndoDireita
    jsr GenerateJetSound
    jsr GenerateRandomVelocity
    lda Random1
    bpl AumentaVelocidade
    lda #0
    sta velocidadex
    jmp EndCollisionCheck

  
CheckCollisionP1Ball:
    lda #%01000000
    bit CXP1FB 
    bne .CollisionP1BL
    jmp EndCollisionCheck
.CollisionP1BL:
    lda #0
    sta IndoDireita
    jsr GenerateJetSound
    jsr GenerateRandomVelocity
    lda Random1
    bpl AumentaVelocidade
    lda #0
    sta velocidadex
    jmp EndCollisionCheck
    
AumentaVelocidade:
    lda #1
    sta velocidadex
    
EndCollisionCheck          
    sta CXCLR

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda BallYPos
    cmp #84
    bpl MovimentoCima
    lda BallYPos
    cmp #2
    bmi MovimentoBaixo
    jmp MovimentoVertical
   
MovimentoCima:
    dec IndoCima
    jmp MovimentoVertical      
    
MovimentoBaixo:
    inc IndoCima
    jmp MovimentoVertical
    
MovimentoVertical:
    lda IndoCima
    cmp #1
    beq PlayfieldCima
    jmp PlayfieldBaixo
    
PlayfieldCima:
    inc BallYPos
    jmp Nada1
PlayfieldBaixo:
    dec BallYPos
Nada1:
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    lda BallXPos
    cmp #134
    bpl MovimentoDireita
    jmp ChecarEsquerda
ChecarEsquerda:
    lda BallXPos
    cmp #3
    bmi MovimentoEsquerda
    jmp Movimento
    ;inc BallXPos
    ;dec BallYPos
MovimentoDireita:
    ;dec IndoDireita
    inc ScoreP1
    jsr GenerateSoundGol
    jmp PosicaoInicial
    ;jmp Movimento      
    
MovimentoEsquerda:
    ;inc IndoDireita
    inc ScoreP0
    jsr GenerateSoundGol
    jmp PosicaoInicial
    ;jmp Movimento
    
PosicaoInicial:
    jsr GenerateRandomBall
    lda #70
    sta BallXPos 
    lda #0
    sta velocidadex
    
   
    ;lda #80
    ;sta BallYPos
    
Movimento:
    lda IndoDireita
    cmp #1
    beq PlayfieldDireita
    jmp PlayfieldEsquerda
    
PlayfieldDireita:
    lda velocidadex
    cmp #1
    beq dobra
    jmp Normal
dobra:
    inc BallXPos
    inc BallXPos
    jmp Nada
Normal:
    inc BallXPos
    jmp Nada
PlayfieldEsquerda:
    lda velocidadex
    cmp #1
    beq dobra2
    jmp Normal2
dobra2:
    dec BallXPos
    dec BallXPos
    jmp Nada
Normal2:   
    dec BallXPos
Nada:

    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
ScoreP0equal3:
    lda ScoreP0
    cmp #5
    bne ScoreP1qual3
    jmp GameOver0
ScoreP1qual3:
    lda ScoreP1
    cmp #5
    bne NADAAAA
    jmp GameOver1
GameOver0:
    lda #0
    sta ScoreP0
    sta ScoreP1
    jmp InitSystem
    ;lda #$30
    ;sta BackgroundColor
    
    lda #70
    sta BallXPos 
    lda #80
    sta BallYPos 
    
    jmp NADAAAAA
    
GameOver1:
    lda #0
    sta ScoreP0
    sta ScoreP1
    jmp InitSystem1
    ;lda #$30
    ;sta BackgroundColor
    
    lda #70
    sta BallXPos 
    lda #80
    sta BallYPos 
    
    jmp NADAAAAA    
    
NADAAAA:
    lda #$08
    sta BackgroundColor
    
NADAAAAA: 
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

GenerateJetSound subroutine
    lda #3
    sta AUDV0                ; set the audio volume register

    lda #5
    sta AUDC0                ; set the audio control register to white noise

    lda BallXPos              ; loads the accumulator with the jet y-position
    lsr
    lsr
    lsr                      ; divide the accumulator by 8 (using right-shifts)
    sta Temp                 ; save the Y/8 value in a temp variable
    lda #31
    sec
    sbc Temp                 ; subtract 31-(Y/8)
    lda #15
    sta AUDF0                ; set the audio frequency/pitch register

    rts
    
    
GenerateRandomBall subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random
    
    lsr
    lsr
    sta BallYPos
    lda #20
    adc BallYPos
    sta BallYPos
    
    rts
    
GenerateRandomVelocity subroutine
    lda Random1
    asl
    eor Random1
    asl
    eor Random1
    asl
    asl
    eor Random1
    asl
    rol Random1
    
    rts   

GenerateSoundGame subroutine
    lda #1
    sta AUDV1                ; set the audio volume register
    
    lda BallXPos              ; loads the accumulator with the jet y-position
    lsr
    lsr
    lsr                      ; divide the accumulator by 8 (using right-shifts)
   
    sta AUDF1                ; set the audio frequency/pitch register
    
    lda #8
    sta AUDC1               ; set the audio control register to white noise

    rts              ; set the audio frequency/pitch register 
    
GenerateSoundGol subroutine

	LDA #3
	STA AUDV0       
	LDA #13
	STA AUDC0
        lda #4
        sta AUDF0
        bne n1	   
    rts    
n1  
	LDA #9
	STA AUDV0       
	LDA #143
	STA AUDC0
        lda #9
        sta AUDF0	   
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

P0Bitmapup:

	.byte #%00000000;$80
        
         .byte #%00111111;--
        .byte #%01111110;--
        .byte #%01011010;--
        .byte #%01111110;--
        .byte #%01111110;--
        .byte #%01111010;--
        .byte #%01100110;--
        .byte #%01111110;--
        

         .byte #%11111111;--
        .byte #%11110001;--
        .byte #%11110001;--
        .byte #%11110001;--
        .byte #%11111001;--
        .byte #%11110101;--
        .byte #%11110011;--
        .byte #%11110001;--
	

      .byte #%10100001;--
        .byte #%10100001;--
        .byte #%10100001;--
        .byte #%10100001;--
        .byte #%10100001;--
        .byte #%10100001;--
        .byte #%10101111;--
        .byte #%10101111;--
        
P0Bitmapdown:

	.byte #%00000000;$80
        
        ;Frame0
        .byte #%10101111;$0E
        .byte #%10100001;$0E
        .byte #%10100001;$0E
        .byte #%10100001;$0E
        .byte #%10100001;$0E
        .byte #%10100001;$0E
        .byte #%10100001;$0E
        .byte #%10100001;$0E
        

         .byte #%11110001;$0E
        .byte #%11110011;$0E
        .byte #%11110101;$0E
        .byte #%11111001;$0E
        .byte #%11110001;$0E
        .byte #%11110001;$0E
        .byte #%11110001;$0E
        .byte #%11111111;$0E
	

         .byte #%11111100;--
        .byte #%11001100;--
        .byte #%11110100;--
        .byte #%11111100;--
        .byte #%11111100;--
        .byte #%10110100;--
        .byte #%11111100;--
        .byte #%01111000;--

        




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
    
P1Bitmapup:

	.byte #%00000000;$80
        
        ;Frame0
        .byte #%01111111;--
        .byte #%00111111;--
        .byte #%00101101;--
        .byte #%00111111;--
        .byte #%00111111;--
        .byte #%00111101;--
        .byte #%00110011;--
        .byte #%00111111;--
        

        .byte #%11111111;--
        .byte #%10001111;--
        .byte #%10001111;--
        .byte #%10001111;--
        .byte #%10011111;--
        .byte #%10101111;--
        .byte #%11001111;--
        .byte #%10001111;--
	

         .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
         .byte #%11110101;--
        .byte #%11110101;--
        
P1Bitmapdown:

	.byte #%00000000;$80
        
        ;Frame0
         .byte #%11110101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        .byte #%10000101;--
        

      .byte #%10001111;--
        .byte #%11001111;--
        .byte #%10101111;--
        .byte #%10011111;--
        .byte #%10001111;--
        .byte #%10001111;--
        .byte #%10001111;--
        .byte #%11111111;--
	

         .byte #%00111111;--
        .byte #%00110011;--
        .byte #%00101111;--
        .byte #%00111111;--
        .byte #%00111111;--
        .byte #%00101101;--
        .byte #%00111111;--
        .byte #%00011110;--     

            

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
    
    
P0Colorup:   
	.byte #$00;
        
         .byte #$30;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;    

        

        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        
        .byte #$70;
        .byte #$70;
        .byte #$70;
        .byte #$70;
        .byte #$70;
        .byte #$70;
        .byte #$70;
        .byte #$70
        
P0Colordown:   
	.byte #$00;
        
       .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
        

       .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        .byte #$40;
        
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
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    
    ;cor da cabeça
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$a4;
    .byte #$a4;wss
    
P1Colorup:   
	.byte #$00;
        
        .byte #$a4;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$a4;
    .byte #$a4;  
        

        byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
        
        .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
        
P1Colordown:   
	.byte #$00;
        
        .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
        

      byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
        
       .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$a4;
    .byte #$a4;    



 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PF_HEIGHT	    = 96	; The TV screen is 192 lines high.  With a 2 line kernal
						;(sort of) we will draw 2 lines every time we decrement						; this number
LETTER_COLOR    = $1E   ; Yellow. From chart at http://www.randomterrain. com
BG_COLOR		= $00	; Black background

InitSystem:

		;sei			; Set Interrupt
		;cld  		; Clear the decimal bit.
		;ldx #$FF	; Start at the top of the stack
		;txs			; Transfer to the stack
		;lda #0		
ClearMem:
		sta 0,X		; Store zero at (0+X)
		dex			; Do all of RAM
		bne ClearMem	; Repeat if we are not down to zero
         
        ; set player X course position    
       ; lda #15		; This will put it about 1/3rd of the way over
        ;sta PlayerX	; Store it in RAM
        
        ; set color of letters
        lda #LETTER_COLOR
        sta COLUP0	; Player zero's color register
 
     
Main:

; This is also a waste of time and space but its ok with such a simple program.

        jsr VerticalSync    ; Jump to SubRoutine VerticalSync
        jsr VerticalBlank   ; Jump to SubRoutine VerticalBlank
        jsr Kernel          ; Jump to SubRoutine Kernel
        jsr OverScan        ; Jump to SubRoutine OverScan
        jmp Main            ; JuMP to Main
        
  
VerticalSync:
        lda #2
        sta WSYNC   ; Any value halts CPU until start of next scanline
        sta VSYNC   ; D1=1, turns on Vertical Sync signal
        sta VBLANK  ; D1=1, turns on Vertical Blank signal (image output off)
        lda #47
        sta TIM64T  ; set timer for end of Vertical Blank
        sta WSYNC   ; 1st scanline
        sta WSYNC   ; 2nd scanline
        lda #0
        sta GRP0	; Blank out player graphic
        sta WSYNC   ; 3rd scanline
        sta VSYNC   ; Accumulator D1=0, turns off Vertical Sync signal
        rts
 
VerticalBlank:    

    
PositionObjects:
        sta WSYNC
       ;  ldy PlayerX   
        
WasteTime:
        nop				; that's right I'm wasting time.
        dey
        bne WasteTime
        sta RESP0,X    ; set coarse X position of object. Not doing a fine tune
 
        
VBwait:
        sta WSYNC
        bit TIMINT		; Test the timer
        bpl VBwait    ; wait for the timer to show end of Vertical Blank
        rts			; End of VerticalBlank subroutine
        
      
Kernel:    
          
        lda #0
        ldy #PF_HEIGHT   ; the height is 96
        sta WSYNC        ; Any value can be written to WSYNC but
        sta VBLANK       ; to turn on video output
        				 ; VBLANK needs to be set to zero to turn it off
        lda BG_COLOR	; Can be done in a lot of places.  It should go here
        				; if we intended to change it while drawing the display
        sta COLUBK          
        ldx #96			; I ended up putting in 96 lines of player 0 graphics
        				; so I wouldn't have to turn the player on or off
        

ArenaLoop:            
        sta WSYNC        	; storing 0, any value works
        lda HumanGfx,x		; Starts at thhe 96th line of graphics
        sta GRP0			; and stores it in player 0's register
        dex					; then the 95th, etc.
        sta WSYNC           ; Makes this a simple 2 line kernal.  Player 0
        					; will have half the vertical resolution that it
        					; could have.
        dey         		; Decrement the playfield line we're on.
        bne ArenaLoop   	; and if there are still lines to draw, repeat
        rts          		; Otherwise go back to the subroutines
    
         
OverScan:
        sta WSYNC   ; Wait for SYNC (start of next scanline)
        lda #2      ; LoaD Accumulator with 2
        sta VBLANK  ; STore Accumulator to VBLANK, D1=1 turns image output off
        lda #22		;
        sta TIM64T  ; set timer for end of Overscan
   
OSwait:
        sta WSYNC
        bit TIMINT	; Test if it's past zero
        bpl OSwait  ; Repeat if its still positive.
        rts 		; Otherwise, go back to the subroutines
       
  

HumanGfx:

 
       ; .byte %00000000
        ;.byte %00000000
        ;.byte %00000000
        
        
       ; .byte %11111110 ;D
       ; .byte %10000001
       ; .byte %10000001
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %11111110
        
        ;.byte %00000000
        
        .byte #%11000111;--n
        .byte #%11000111;--
        .byte #%11001111;--
        .byte #%11011011;--
        .byte #%11011011;--
        .byte #%11010011;--
        .byte #%11110011;--
        .byte #%11110011;--
        
        .byte %00000000
        
        ;.byte %10000010 ;R
        ;.byte %10000100
        ;.byte %10001000
        ;.byte %11111100
        ;.byte %10000010
        ;.byte %10000010
        ;.byte %11111100
        
        .byte %00000000
        
       .byte #%00011000;--i
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        
        .byte %00000000
        
        .byte #%11000011;--
        .byte #%11100111;--
        .byte #%11111111;--
        .byte #%11011011;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11000011;--
        
        .byte %00000000
        .byte %00000000
        .byte %00000000
        
        .byte #%11111111;--1
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00111000;--
        
        
        
        .byte %00000000
        .byte %00000000 ;space
        .byte %00000000
        .byte %00000000
        .byte %00000000
       ;   .byte %00000000
       ; .byte %00000000
       ; .byte %00000000
        ;.byte %00000000
       ; .byte %00000000
        
        .byte %00000000
        
         .byte #%11000111;--r
        .byte #%11001110;--
        .byte #%11011100;--
        .byte #%11111000;--
        .byte #%11111111;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11111111;--
        
          .byte %00000000
        
        .byte #%11111111;--e
        .byte #%11111111;--
        .byte #%11000000;--
        .byte #%11111111;--
        .byte #%11111111;--
        .byte #%11000000;--
        .byte #%11111111;--
        .byte #%11111111;--
        
        .byte %00000000
        
        .byte #%00011000;--y
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00111100;--
        .byte #%01100110;--
        .byte #%11000011;--
        .byte #%10000001;--
        
        .byte %00000000
        
       .byte #%11000011;--a
        .byte #%11000011;--
        .byte #%11111111;-- 
        .byte #%11111111;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11111111;--
        
        .byte %00000000
        
        .byte #%11111111;--l
        .byte #%11111111;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        
        ;.byte %11111111;E
        ;.byte %10000000
        ;.byte %10000000
        ;.byte %11111110
        ;.byte %10000000
        ;.byte %10000000
        ;.byte %11111111
        
        .byte %00000000
        
         .byte #%11000000;--p
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11111111;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11111111;--
        
        ;.byte %10000001	;H
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %11111111
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %10000001
        
        
        
        
        
        
       ; .byte #%00000000;--2
        ;.byte #%00111100;--2
        ;.byte #%00100000;--2
        ;.byte #%00111000;--2
        ;.byte #%00001100;--2
        ;.byte #%00100100;--2
        ;.byte #%00100100;--2
        ;.byte #%00011000;--2
        
        
        
        
        .byte %00000000
        .byte %00000000
        .byte %00000000
        
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PF_HEIGHT1	    = 96	; The TV screen is 192 lines high.  With a 2 line kernal
						;(sort of) we will draw 2 lines every time we decrement						; this number
LETTER_COLOR1    = $1E   ; Yellow. From chart at http://www.randomterrain. com
BG_COLOR1		= $00	; Black background

InitSystem1:

		;sei			; Set Interrupt
		;cld  		; Clear the decimal bit.
		;ldx #$FF	; Start at the top of the stack
		;txs			; Transfer to the stack
		;lda #0		
ClearMem1:
		sta 0,X		; Store zero at (0+X)
		dex			; Do all of RAM
		bne ClearMem1	; Repeat if we are not down to zero
         
        ; set player X course position    
       ; lda #15		; This will put it about 1/3rd of the way over
        ;sta PlayerX	; Store it in RAM
        
        ; set color of letters
        lda #LETTER_COLOR1
        sta COLUP0	; Player zero's color register
 
     
Main1:

; This is also a waste of time and space but its ok with such a simple program.

        jsr VerticalSync1    ; Jump to SubRoutine VerticalSync
        jsr VerticalBlank1   ; Jump to SubRoutine VerticalBlank
        jsr Kernel1          ; Jump to SubRoutine Kernel
        jsr OverScan1        ; Jump to SubRoutine OverScan
        jmp Main1           ; JuMP to Main
        
  
VerticalSync1:
        lda #2
        sta WSYNC   ; Any value halts CPU until start of next scanline
        sta VSYNC   ; D1=1, turns on Vertical Sync signal
        sta VBLANK  ; D1=1, turns on Vertical Blank signal (image output off)
        lda #47
        sta TIM64T  ; set timer for end of Vertical Blank
        sta WSYNC   ; 1st scanline
        sta WSYNC   ; 2nd scanline
        lda #0
        sta GRP0	; Blank out player graphic
        sta WSYNC   ; 3rd scanline
        sta VSYNC   ; Accumulator D1=0, turns off Vertical Sync signal
        rts
 
VerticalBlank1:    

    
PositionObjects1:
        sta WSYNC
       ;  ldy PlayerX   
        
WasteTime1:
        nop				; that's right I'm wasting time.
        dey
        bne WasteTime1
        sta RESP0,X    ; set coarse X position of object. Not doing a fine tune
 
        
VBwait1:
        sta WSYNC
        bit TIMINT		; Test the timer
        bpl VBwait1    ; wait for the timer to show end of Vertical Blank
        rts			; End of VerticalBlank subroutine
        
      
Kernel1:    
          
        lda #0
        ldy #PF_HEIGHT1   ; the height is 96
        sta WSYNC        ; Any value can be written to WSYNC but
        sta VBLANK       ; to turn on video output
        				 ; VBLANK needs to be set to zero to turn it off
        lda BG_COLOR	; Can be done in a lot of places.  It should go here
        				; if we intended to change it while drawing the display
        sta COLUBK          
        ldx #96			; I ended up putting in 96 lines of player 0 graphics
        				; so I wouldn't have to turn the player on or off
        

ArenaLoop1:            
        sta WSYNC        	; storing 0, any value works
        lda HumanGfx1,x		; Starts at thhe 96th line of graphics
        sta GRP0			; and stores it in player 0's register
        dex					; then the 95th, etc.
        sta WSYNC           ; Makes this a simple 2 line kernal.  Player 0
        					; will have half the vertical resolution that it
        					; could have.
        dey         		; Decrement the playfield line we're on.
        bne ArenaLoop1   	; and if there are still lines to draw, repeat
        rts          		; Otherwise go back to the subroutines
    
         
OverScan1:
        sta WSYNC   ; Wait for SYNC (start of next scanline)
        lda #2      ; LoaD Accumulator with 2
        sta VBLANK  ; STore Accumulator to VBLANK, D1=1 turns image output off
        lda #22		;
        sta TIM64T  ; set timer for end of Overscan
   
OSwait1:
        sta WSYNC
        bit TIMINT	; Test if it's past zero
        bpl OSwait1  ; Repeat if its still positive.
        rts 		; Otherwise, go back to the subroutines
       
  

HumanGfx1:

 
       ; .byte %00000000
        ;.byte %00000000
        ;.byte %00000000
        
        
       ; .byte %11111110 ;D
       ; .byte %10000001
       ; .byte %10000001
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %11111110
        
        ;.byte %00000000
        
        .byte #%11000111;--n
        .byte #%11000111;--
        .byte #%11001111;--
        .byte #%11011011;--
        .byte #%11011011;--
        .byte #%11010011;--
        .byte #%11110011;--
        .byte #%11110011;--
        
        .byte %00000000
        
        ;.byte %10000010 ;R
        ;.byte %10000100
        ;.byte %10001000
        ;.byte %11111100
        ;.byte %10000010
        ;.byte %10000010
        ;.byte %11111100
        
        .byte %00000000
        
       .byte #%00011000;--i
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        
        .byte %00000000
        
        .byte #%11000011;--
        .byte #%11100111;--
        .byte #%11111111;--
        .byte #%11011011;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11000011;--
        
        .byte %00000000
        .byte %00000000
        .byte %00000000
        
      .byte #%11111111;--2
        .byte #%11111111;--
        .byte #%10000000;--
        .byte #%11111111;--
        .byte #%11111111;--
        .byte #%00000001;--
        .byte #%11111111;--
        .byte #%11111111;--
          
        
        
        .byte %00000000
        .byte %00000000 ;space
        .byte %00000000
        .byte %00000000
        .byte %00000000
       ;   .byte %00000000
       ; .byte %00000000
       ; .byte %00000000
        ;.byte %00000000
       ; .byte %00000000
        
        .byte %00000000
        
         .byte #%11000111;--r
        .byte #%11001110;--
        .byte #%11011100;--
        .byte #%11111000;--
        .byte #%11111111;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11111111;--
        
          .byte %00000000
        
        .byte #%11111111;--e
        .byte #%11111111;--
        .byte #%11000000;--
        .byte #%11111111;--
        .byte #%11111111;--
        .byte #%11000000;--
        .byte #%11111111;--
        .byte #%11111111;--
        
        .byte %00000000
        
        .byte #%00011000;--y
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00011000;--
        .byte #%00111100;--
        .byte #%01100110;--
        .byte #%11000011;--
        .byte #%10000001;--
        
        .byte %00000000
        
       .byte #%11000011;--a
        .byte #%11000011;--
        .byte #%11111111;--
        .byte #%11111111;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11111111;--
        
        .byte %00000000
        
        .byte #%11111111;--l
        .byte #%11111111;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        
        ;.byte %11111111;E
        ;.byte %10000000
        ;.byte %10000000
        ;.byte %11111110
        ;.byte %10000000
        ;.byte %10000000
        ;.byte %11111111
        
        .byte %00000000
        
         .byte #%11000000;--p
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11000000;--
        .byte #%11111111;--
        .byte #%11000011;--
        .byte #%11000011;--
        .byte #%11111111;--
        
        ;.byte %10000001	;H
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %11111111
        ;.byte %10000001
        ;.byte %10000001
        ;.byte %10000001
        
        
        
        
        
        
       ; .byte #%00000000;--2
        ;.byte #%00111100;--2
        ;.byte #%00100000;--2
        ;.byte #%00111000;--2
        ;.byte #%00001100;--2
        ;.byte #%00100100;--2
        ;.byte #%00100100;--2
        ;.byte #%00011000;--2
        
        
        
        
        .byte %00000000
        .byte %00000000
        .byte %00000000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completar a ROM para 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start
