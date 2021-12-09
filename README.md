# Projeto - [Easy D-i-sk]
Jogo feito em assembly para a disciplina de Interface Hardware-Software do curso de Engenharia da Computação da Universidade Federal de Sergipe.

# Imagens
!(imagem_principal.png)
# Funcionamento
O jogo é composto por uma arena (preta) onde os dois players (Player 0 e Player 1) podem se mover até metade da arena. Cada Player pode
rebater o disco. O objetivo do jogo consiste em conseguir fazer 5 gols primeiro. Há algumas características importantes:
1. Quando ocorre um gol, a posição que bola nasce no eixo-y é aleatória, porém sempre no mesmo valor do eixo-x (meio do jogo).
1. Quando a bola é rebatida por um Player, ela pode ser rebatida em uma velocidade normal, ou duas vezes mais rápido. Isso também ocorre
de maneira aleatória.
1. Quando há um game over, uma tela aparece indicando quem ganhou e para jogar novamente, basta reiniciar o jogo.
1. Há animação dos dois players
1. Há som quando a bola bate em um Player ou ocorre um gol.
1. Há um som de fundo, cujo arquivo tem o nome 'sfx.asm'.
1. Há dois placares e o nome do jogo

# Vídeo de Apresentação

# Ferramentas Utilizadas
* Linguagem Assembly do Atari
* Editor de código - Visual Studio Code
* Emulador - Stella
* Assembler - DASM
* Ferramenta Online para debug e emulação - [8bitworkshop](https://8bitworkshop.com/)

# Controles do Jogo
* Player 0 - Basta usar as teclas direcionais do teclado 
* Player 1 - No emulador Stella y(cima), h(baixo), g(esquerda), j(direita)

# Como jogar

## Primeiro Caminho - Emulador Online
1. Acesse o site do [8bitworkshop](https://8bitworkshop.com/), e clique no botão 'Open 8bitworkshop IDE'.
1. Logo após, acesse o arquivo *disk_final.asm* e copie o seu conteúdo colando-o em seguida na página aberta pelo site 8bitworkshop.
1. Por fim, basta obter o arquivo *sfx.asm* e colocá-lo no site do 8bitworkshop. Para isso, vá até icone em hamburguer que fica do lado
esquerdo superior, e escolha a opção upload realizando assim o uploado do arquivo requerido. Feito isso, o jogo já estará rodando.

## Segundo Caminho - Emulado Offline
1. Instalar o Stella no estilo 'next next fisish'.
1. Instalar o dasm, colocando-o como uma variável de ambiente do sistema.
1. Após isso, clone o repositório, entre na pasta e execute o comando 'dasm disk_final.asm -f3 -v0 -ocart'.
1. Feito isso, será gerado um arquivo 'cart'. Renomeie-o para 'cart.bin' e abra o arquivo com o Stella