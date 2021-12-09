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
ScoreP0Sprite byte          ; Guarda o sprite do Placar do Player0
ScoreP1Sprite    byte       ; Guarda o sprite do Placar do Player1
BallXPos byte               ; Bola - eixo-x
BallYPos byte               ; Bola - eixo-y
IndoDireita byte            ; Flag para indicar se a bola está indo para a direita ou não (1=indo para direita, 0=indo para esquerda)
IndoCima byte               ; Flag para indicar se a bola está indo para cima ou não (1=indo para cima, 0=indo para baixo)
Random byte
Random1 byte
velocidadex byte
CharAnimOffset0 byte
CharAnimOffset1 byte
ContadorSom byte            ; Variavel pra percorrer a tabela de sons
Player1Venceu byte          ; Flag para indicar se o Player0 Venceu
Player2Venceu byte          ; Flag para indicar se o Player1 Venceu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definição de constantes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PLAYER0_HEIGHT = 24         ; Altura do sprite do player 0
PLAYER1_HEIGHT = 24         ; Altura do sprite do player 0
DIGITS_HEIGHT = 5           ; Altura do 'sprite' dos digitos

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
    
    lda #45
    sta P0YPos     

    lda #10
    sta P1YPos     

    lda #0
    sta ScoreP0
    lda #0
    sta ScoreP1

    lda #70
    sta BallXPos 

    lda #50
    sta BallYPos  
    
    lda #1
    sta IndoDireita
    
    lda #0
    sta IndoCima
    
    lda #%11010100
    sta Random
    
    lda #%11010100
    sta Random1
    
    lda #0
    sta ContadorSom

    lda #0
    sta Player1Venceu

    lda #0
    sta Player2Venceu

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MACRO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    MAC DRAW_BALL
        lda #0
    	cpx BallYPos
        bne .SkipBallDraw
.DrawMissile:
	lda #%00000010
.SkipBallDraw
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
    sta VSYNC                ; Desliga VSYNC

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VBLANK (37 linhas)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta COLUBK
    REPEAT 33
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setando eixo-x do Player 0, Player 1, Bola (durante VBLANK)
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
    jsr GenerateSoundGame    ; Gerando o som do jogo

    sta WSYNC
    sta HMOVE                ; Aplica a posição horizontal

    lda #0
    sta VBLANK               ; Desliga VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mostrar tela de vitoria (se houver)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ldy Player1Venceu            ; Verifica se Player0 Venceu
    cpy #1   
    beq TelaVitoriaPlayer0       ; Se sim, vai mostra a tela de vitoria do Player0
    jmp VerificarPlayer1Venceu   ; Caso contrario, verifica se Player1 venceu
TelaVitoriaPlayer0:
    lda #$1E                 ; Amarelo
    sta COLUP0
    lda #$00                 ; Preto
    sta COLUBK
    lda #0
    sta AUDV0

    ldx #95
MensagemVitoriaPlayer0:
    lda MensagemVitoriaP0,X
    sta WSYNC 
    sta WSYNC
    sta GRP0
    dex					
    bne MensagemVitoriaPlayer0 
    
    lda #0                    ; Preto
    sta COLUP0
    REPEAT 2
        sta WSYNC
    REPEND

    jmp IrParaVblank
VerificarPlayer1Venceu:
    ldy Player2Venceu
    cpy #1
    beq TelaVitoriaPlayer1
    jmp Placar                ; Se Player1 não venceu, então exibe a tela do jogo normalmente, começando pelo placar
TelaVitoriaPlayer1:
    lda #$1E                  ; Amarelo
    sta COLUP1
    lda #$00                  ; Preto
    sta COLUBK
    lda #0
    sta AUDV0

    ldx #95
MensagemVitoriaPlayer1:
    lda MensagemVitoriaP1,X
    sta WSYNC 
    sta WSYNC
    sta GRP1
    dex					
    bne MensagemVitoriaPlayer1 
    
    lda #0
    sta COLUP1
    REPEAT 2
        sta WSYNC
    REPEND

    jmp IrParaVblank
Placar:
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

    lda #$1E                    ; Amarelo
    sta COLUPF
    
    ldx #DIGITS_HEIGHT          ; X guarda o valor de 5
.ScoreDigitLoop:
    ldy TensDigitOffset         
    lda Digits,Y                
    and #$F0                    
    sta ScoreP0Sprite           

    ldy OnesDigitOffset         
    lda Digits,Y                
    and #$0F                    
    ora ScoreP0Sprite           
    sta ScoreP0Sprite           
    sta WSYNC                   
    sta PF1                     

    ldy TensDigitOffset+1       
    lda Digits,Y                
    and #$F0                    
    sta ScoreP1Sprite           

    ldy OnesDigitOffset+1       
    lda Digits,Y                
    and #$0F                    
    ora ScoreP1Sprite           
    sta ScoreP1Sprite           

    jsr Sleep12Cycles           

    sta PF1                     

    ldy ScoreP0Sprite           
    sta WSYNC                   

    sty PF1                     
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1

    jsr Sleep12Cycles           
    dex                         
    sta PF1                     
    
    bne .ScoreDigitLoop         
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
    ; Pulando 14 linhas, ou seja, tornando as preto (Background) (14 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2 
    
    lda #0
    sta AUDV0               ; Desligando o audio do canal 0
    
    ldx #$0E
    stx COLUBK              ; Cor do background (Branco)
    ldx #$00
    stx COLUPF              ; Cor do playfield (Preto)

    ldx #%00100001          ; Mudando o tamanho da bola, e colocando o playfield para repetir
    stx CTRLPF

    REPEAT 3
        sta WSYNC           ; Desenhando algumas linhas em branco
    REPEND

    ; Parte superior do playfield (Preto) (7 linhas)
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Lateral do playfield (Preto) e Players (140 linhas)
    ldx #%00100000
    stx PF0
    ldx #%00000000
    stx PF1
    ldx #%00000000
    stx PF2

    ldx #70                  ; 2-scanline kernel
.GameLineLoop:
    DRAW_BALL
.AreWeInsideP0Sprite:        ; Desenhando o sprite do Player0
    txa                      ; Transferir X para A
    sec                      ; Setando carry flag antes da subtração
    sbc P0YPos               ; Subtrai de A o eixo-y do Player 0
    cmp PLAYER0_HEIGHT       ; Verifica se já chegou na altura correta para começar a desenhar o sprite
    bcc .DesenhaSpriteP0     ; Se chegou, chama DesenhaSpriteP0
    lda #0                   ; Caso não, indice = 0, imprime tudo preto
.DesenhaSpriteP0:
    clc
    adc CharAnimOffset0	
    tay                      ; Transfere A para Y
    lda P0Bitmap,Y           ; Carrega em A o bitmap do Player 0
    sta WSYNC                ; Espera scanline
    sta GRP0                 ; Habilita gráficos do Player 0
    lda P0Color,Y            ; Carrega em A as cores do Player 0
    sta COLUP0               ; Define as cores do Player 0

.AreWeInsideP1Sprite:        ; Desenhando o sprite do Player 1	
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
    sta CharAnimOffset0
    lda #0
    sta CharAnimOffset1

    ; Parte inferior do playfield (Preto) (7 linhas)
    ldx #%11100000
    stx PF0
    ldx #%11111111
    stx PF1
    stx PF2
    REPEAT 7
        sta WSYNC
    REPEND

    ; Pulando 3 linhas, ou seja, tornando as brancas (134 linhas)
    ldx #0
    stx PF0
    stx PF1
    stx PF2
    REPEAT 3
        sta WSYNC
    REPEND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Escrevendo o nome
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
    lda #$00                    ; Amarelo
    sta COLUPF

    lda #%00000000              ; Tirar a reflexão do playfield
    sta CTRLPF
    
    ; PRIMEIRA LINHA
    REPEAT 2
        ldx #%01100000
        stx PF0
        ldx #%01001101
        stx PF1
        ldx #%00000010
        stx PF2

        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        ldx #%11110000
        stx PF0
        ldx #%00010001
        stx PF1
        ldx #%00010101
        stx PF2

        sta WSYNC
    REPEND

    ; SEGUNDA LINHA
    REPEAT 2  
        ldx #%00100000
        stx PF0
        ldx #%11101001
        stx PF1
        ldx #%00000010
        stx PF2

        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        ldx #%10100000
        stx PF0
        ldx #%00010001
        stx PF1
        ldx #%00001100
        stx PF2

        sta WSYNC
    REPEND

    ; TERCEIRA LINHA
    REPEAT 2
        ldx #%01100000
        stx PF0
        ldx #%10101100
        stx PF1
        ldx #%00000001
        stx PF2
        
        nop
        nop
        nop
        nop
        nop
        nop

        ldx #%10100000
        stx PF0
        ldx #%01111101
        stx PF1
        ldx #%00000101
        stx PF2

        sta WSYNC
    REPEND
    
    ; QUARTA LINHA
    REPEAT 2
        ldx #%00100000
        stx PF0
        ldx #%11100100
        stx PF1
        ldx #%00000001
        stx PF2

        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop

        ldx #%10100000
        stx PF0
        ldx #%00010000
        stx PF1
        ldx #%00001101
        stx PF2

        sta WSYNC
    REPEND

    ; QUINTA LINHA
    REPEAT 2
        ldx #%01100000
        stx PF0
        ldx #%10101100
        stx PF1
        ldx #%00000001
        stx PF2

        nop
        nop
        nop
        nop
        nop
        nop
        nop

        ldx #%11110000
        stx PF0
        ldx #%00010001
        stx PF1
        ldx #%00010101
        stx PF2

        sta WSYNC
    REPEND

    ldx #0
    stx PF0
    stx PF1
    stx PF2
    
    REPEAT 2
        sta WSYNC
    REPEND

IrParaVblank:
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
    cmp #47                         ; Limitando a posição Y do player 0 (superior)
    bpl CheckP0Down
    inc P0YPos
    lda #25
    sta CharAnimOffset0             ; Mudando a animação
CheckP0Down:
    lda #%00100000
    bit SWCHA
    bne CheckP0Left
    lda P0YPos
    cmp #2                          ; Limitando a posição Y do player 0 (inferior)
    bmi CheckP0Left
    dec P0YPos
    lda #50
    sta CharAnimOffset0

CheckP0Left:
    lda #%01000000
    bit SWCHA
    bne CheckP0Right
    lda P0XPos
    cmp #3                          ; Limitando a posição X do player 0 (esquerda)
    bmi CheckP0Right
    dec P0XPos
    lda #0
    sta CharAnimOffset0

CheckP0Right:
    lda #%10000000
    bit SWCHA
    bne NoInput0;
    lda P0XPos
    cmp #58                         ; Limitando a posição X do player 0 (direita)
    bpl NoInput0;
    inc P0XPos
    lda #0
    sta CharAnimOffset0
/*     
CheckP0Buttom:
	lda #%10000000
	bit INPT4
    bne NoInput0 */
    ;jmp NextFrame

NoInput0:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input para o player 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckP1Up:
    lda #%00000001
    bit SWCHA
    bne CheckP1Down
    lda P1YPos
    cmp #47
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
    cmp #130
    bpl NoInput1
    inc P1XPos
    lda #0
    sta CharAnimOffset1

NoInput1:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checando se o botão reset foi pressionado
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
/* CheckReset:
    lda #$01
    bit SWCHB
    beq SwitchPressed
    jmp NoInputReset
SwitchPressed:
    lda #0
    sta Player1Venceu
    sta Player2Venceu
    sta ScoreP0
    sta ScoreP1
    lda #70
    sta BallXPos 
    lda #50
    sta BallYPos 
    lda #0
    sta velocidadex
    sta ENABL
    jmp NADAAAAA
NoInputReset: */

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checando colisão do Player 0 com a bola
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0Ball:
    lda #%01000000
    bit CXP0FB 
    bne .CollisionPOBL
    jmp CheckCollisionP1Ball
.CollisionPOBL:
    lda #1
    sta IndoDireita
    jsr GenerateColisionSoundBP
    jsr GenerateRandomVelocity
    lda Random1
    bpl AumentaVelocidade
    lda #0
    sta velocidadex
    jmp EndCollisionCheck

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checando colisão do Player 1 com a bola
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP1Ball:
    lda #%01000000
    bit CXP1FB 
    bne .CollisionP1BL
    jmp EndCollisionCheck
.CollisionP1BL:
    lda #0
    sta IndoDireita
    jsr GenerateColisionSoundBP
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Limitando movimento da bola no eixo Y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    lda BallYPos
    cmp #70
    bpl MovimentoCima
    lda BallYPos
    cmp #5
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Limitando movimento da bola no eixo X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Verificando se alguém ganhou o jogo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
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
    lda #1
    sta Player1Venceu
Continua1:    
    lda #70
    sta BallXPos 
    lda #80
    sta BallYPos 
    
    jmp NADAAAAA
    
GameOver1:
    lda #0
    sta ScoreP0
    sta ScoreP1
    lda #1
    sta Player2Venceu

Continua2:    
    lda #70
    sta BallXPos 
    lda #80
    sta BallYPos 
    jmp NADAAAAA    
    
NADAAAA:
    
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gerando som quando ocorre colisão da bola com o player
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateColisionSoundBP subroutine
    lda #3
    sta AUDV0                

    lda #5
    sta AUDC0                

    lda BallXPos              
    lsr
    lsr
    lsr                      
    sta Temp                 
    lda #31
    sec
    sbc Temp                 
    lda #15
    sta AUDF0                

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gerando posição aleatoria da bola no eixo-y
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gerando velocidade aleatoria da bola ao ser rebatida
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subrotina para gerar o som do jogo com base no arquivo sfx.asm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
GenerateSoundGame subroutine
    lda #1
    sta AUDV1                ; Setando volume
    
    ldy ContadorSom
    lda SFX_F,Y              ; Percorrendo a tabela de som 'SFX_F'
    sta AUDF1                ; Pega o valor da table e coloca no registrador de frequência
    inc ContadorSom
    lda #5
    sta AUDC1                ; Setando o registrador de controle para alterar o tipo de som
    rts                      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Subrotina para gerar o som do gol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
GenerateSoundGol subroutine
	lda #3
	sta AUDV0                ; Volume     
	lda #13
	sta AUDC0                ; Tipo do som
    lda #4
    sta AUDF0                ; Frenquencia
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
    
    ; Pernas
   	.byte #%11110111;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    .byte #%10100100;$70
    
    ; Corpo
    .byte #%11100110;$30
    .byte #%11100100;$30
    .byte #%11101100;$30
    .byte #%11101000;$30
    .byte #%11101000;$30
    .byte #%11111000;$30
    .byte #%11111000;$30
    .byte #%11111000;$30
    
    ; Cabeça
    .byte #%11111000;$36
    .byte #%11001000;$36
    .byte #%11011000;$36
    .byte #%11111000;$36
    .byte #%11101000;$36
    .byte #%11110000;$36
    .byte #%11111100;$30
    .byte #%11110000;$30

P0Bitmapup:

	.byte #%00000000;

    ; Pernas
    .byte #%00111111;--
    .byte #%01111110;--
    .byte #%01011010;--
    .byte #%01111110;--
    .byte #%01111110;--
    .byte #%01111010;--
    .byte #%01100110;--
    .byte #%01111110;--
        
    ; Corpo
    .byte #%11111111;--
    .byte #%11110001;--
    .byte #%11110001;--
    .byte #%11110001;--
    .byte #%11111001;--
    .byte #%11110101;--
    .byte #%11110011;--
    .byte #%11110001;--
	
    ; Cabeça
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
        
    ; Pernas
    .byte #%10101111;$0E
    .byte #%10100001;$0E
    .byte #%10100001;$0E
    .byte #%10100001;$0E
    .byte #%10100001;$0E
    .byte #%10100001;$0E
    .byte #%10100001;$0E
    .byte #%10100001;$0E
    
    ; Corpo
    .byte #%11110001;$0E
    .byte #%11110011;$0E
    .byte #%11110101;$0E
    .byte #%11111001;$0E
    .byte #%11110001;$0E
    .byte #%11110001;$0E
    .byte #%11110001;$0E
    .byte #%11111111;$0E
	
    ; Cabeça
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
    
    ; Pernas
    .byte #%11101111;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    .byte #%00100101;$80
    
    ; Corpo
    .byte #%01100111;$32
    .byte #%00100111;$32
    .byte #%00100111;$32
    .byte #%00110111;$32
    .byte #%00010111;$32
    .byte #%00010111;$32
    .byte #%00011111;$32
    .byte #%00011111;$32
    
    ; Cabeça
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
        
    ; Pernas
    .byte #%01111111;--
    .byte #%00111111;--
    .byte #%00101101;--
    .byte #%00111111;--
    .byte #%00111111;--
    .byte #%00111101;--
    .byte #%00110011;--
    .byte #%00111111;--

    ; Corpo
    .byte #%11111111;--
    .byte #%10001111;--
    .byte #%10001111;--
    .byte #%10001111;--
    .byte #%10011111;--
    .byte #%10101111;--
    .byte #%11001111;--
    .byte #%10001111;--

    ; Cabeça
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
        
    ; Pernas
    .byte #%11110101;--
    .byte #%10000101;--
    .byte #%10000101;--
    .byte #%10000101;--
    .byte #%10000101;--
    .byte #%10000101;--
    .byte #%10000101;--
    .byte #%10000101;--
        

    ; Corpo
    .byte #%10001111;--
    .byte #%11001111;--
    .byte #%10101111;--
    .byte #%10011111;--
    .byte #%10001111;--
    .byte #%10001111;--
    .byte #%10001111;--
    .byte #%11111111;--

    ; Cabeça
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

    ; Cor das pernas
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    
    ; Cor do corpo
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    .byte #$30;
    
    ; Cor da cabeça
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

    ; Cor das Pernas
    .byte #$30;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;    

    ; Cor do Corpo
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;

    ; Cor da Cabeça
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
        
    ; Cor das Pernas
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
        
    ; Cor do Corpo
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;
    .byte #$40;

    ; Cor da Cabeça
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
    
    ; Cor das pernas
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    
    ; Cor do corpo
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    
    ; Cor da cabeça
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$a4;
    .byte #$a4;
    
P1Colorup:   
	.byte #$00;

    ; Cor das pernas
    .byte #$a4;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$a4;
    .byte #$a4;  
        
    ; Cor do corpo
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;

    ; Cor da cabeça
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

    ; Cor das pernas
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
    .byte #$70;
        
    ; Cor do corpo
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;
    .byte #$a4;

    ; Cor da cabeça
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$36;
    .byte #$a4;
    .byte #$a4;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lookup table do som
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    include "sfx.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lookup table  das mensagens de vitoria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
MensagemVitoriaP0:
    .byte #%11000111;--n
    .byte #%11000111;--
    .byte #%11001111;--
    .byte #%11011011;--
    .byte #%11011011;--
    .byte #%11010011;--
    .byte #%11110011;--
    .byte #%11110011;--

    .byte %00000000
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
    .byte %00000000 
    .byte %00000000
    .byte %00000000
    .byte %00000000
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

    .byte %00000000

    .byte #%11000000;--p
    .byte #%11000000;--
    .byte #%11000000;--
    .byte #%11000000;--
    .byte #%11111111;--
    .byte #%11000011;--
    .byte #%11000011;--
    .byte #%11111111;--

    .byte %00000000
    .byte %00000000
    .byte %00000000
MensagemVitoriaP1:
    .byte #%11000111;--n
    .byte #%11000111;--
    .byte #%11001111;--
    .byte #%11011011;--
    .byte #%11011011;--
    .byte #%11010011;--
    .byte #%11110011;--
    .byte #%11110011;--
    
    .byte %00000000
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
    .byte %00000000 
    .byte %00000000
    .byte %00000000
    .byte %00000000
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
    
    .byte %00000000
    
    .byte #%11000000;--p
    .byte #%11000000;--
    .byte #%11000000;--
    .byte #%11000000;--
    .byte #%11111111;--
    .byte #%11000011;--
    .byte #%11000011;--
    .byte #%11111111;--
    
    .byte %00000000
    .byte %00000000
    .byte %00000000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completar a ROM para 4KB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    org $FFFC
    .word Start
    .word Start
