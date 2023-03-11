# Projeto : Fase 1 
## Inteligência Artificial 22/23 
Prof. Joaquim Filipe 

Eng. Filipe Mariano 

# Dots and Boxes
**Dots and Boxes** é um [jogo de lápis e papel](https://en.wikipedia.org/wiki/Pencil_and_paper_game "jogo de lápis e papel") para dois jogadores. 
Este tem como objetivo fechar o maior numero de caixas possíveis. Aplicando apenas arcos ligando os pontos apresentados no tabuleiro.

## Manual de Utilizador
Realizador por: 

Dinis Pimpão -> 201901055 

Pedro Peralta -> 202002153

## Índice
 1. Introdução
 2. Instalação
 3. Interface



## 1 - Introdução
O manual de utilizador tem como objetivo facilitar a instalação e a utilização de todo o projeto, nomeadamente dos menús e definições disponiveis e do modo de jogo do mesmo.



## 2 - Instalação
Para a utilização do projeto é necessário o IDE LispWorks ou qualquer outro que seja possível compilar e executar o código Lisp.

O projeto é composto por 3 ficheiros de código e 1 auxiliar de "logs" das jogadas:

 - **jogo.lisp** ->  Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador. 
 - **puzzle.lisp** -> Código relacionado com o problema. 
 - **algoritmo.lisp** -> Contem a implementação do algoritmo minimax com cortes alfabeta.
 - **log.dat** - ficheiro do historico das execuções do algoritmo alfa-beta cada vez que o computador joga. Guarda as jogadas realizadas, nomeadamente o novo estado, o número de nós analisados, o número de cortes alfa e beta e o tempo de execução.

Para iniciar o programa, deve-se carregar o ficheiro `jogo.lisp` e este irá carregar os outros 2 ficheiros auxiliares.

**NOTA: Para garantir o funcionamento do programa, é necessário alterar o diretório onde será escrito o ficheiro de logs, caso contrário, o programa dará erro. O caminho pode ser alterado no ficheiro `jogo.lisp`, logo no topo do mesmo, dentro da função `ficheiro-solucao`. O caminho deve seguir o exemplo que já se encontra lá, sendo necessário o uso das 2 barras `\\`.**



## 3 - Interface
O programa inicia-se chamando a função iniciar. Esta irá realizar apresentar o menú inicial do programa, onde o utilizador poderá visualizar os valores por default do jogador inicial e tempo máximo de jogada (ou atuais caso tenha alterado durante a execução do programa). Neste menú são apresentadas 3 opções, a opção de jogar, de editar as definições de jogo e a de sair (terminar o programa).

```lisp
CL-USER 1 > (iniciar)
```

Menú inicial:
```lisp
 ______________________________________________________ 
|                                                      |
|                    DOTS AND BOXES                    |
|                                                      |
|                     1 - Play                         |
|                     2 - Settings                     |
|                     0 - Quit                         |
|                                                      |
|                    Settings Value                    |
|                   Initial Player: 1                  |
|                   Max Time: 5000 ms                  |
|______________________________________________________|
```

Caso seja escolhida a opção das definições, será apresentado um menú onde o utilizador poderá indicar o tempo máximo que uma jogada do computador poderá demorar e quem será o primeiro utilizador a jogar (este 2º valor apenas terá utilidade na modalidade User vs Pc).

```lisp
 ______________________________________________________ 
|                                                      |
|                       SETTINGS                       |
|                                                      |
|                 1 - Execution Time                   |
|                 2 - Initial Player                   |
|                 0 - Home                             |
|                                                      |
|______________________________________________________|

 ______________________________________________________ 
|                                                      |
|                    EXECUTION TIME                    |
|                                                      |
|                     [1000 - 5000]                    |
|                       0 - Home                       |
|                                                      |
|______________________________________________________|
 ______________________________________________________ 
|                                                      |
|                    Initial Player                    |
|                                                      |
|                     1 - (User)                       |
|                     2 - (PC)                         |
|                     0 - Home Menu                    |
|                                                      |
|______________________________________________________|

```

Caso no primeiro menú, o utilizador escolha a opção de jogar, será apresentado um novo menú para que este escolha qual o modo de jogo que quer (Utilizador vs PC ou PC vs PC).

Menú de escolha de modo de jogo:
```lisp
 ______________________________________________________ 
|                                                      |
|                      GAME MODES                      |
|                                                      |
|                   1 - Human vs PC                    |
|                   2 - PC vs PC                       |
|                   0 - Home                           |
|                                                      |
|                       DEFAULT                        |
|                   Initial Player: 1                  |
|                   Max Time: 5000 ms                  |
|______________________________________________________|
```

Relativamente aos modos de jogo, no modo PC vs PC, este jogará tanto como jogador 1 e 2 e cada uma das suas jogadas, será limitada pelo tempo definido pelo utilizador ou por default (5000ms).

Caso seja escolhido o modo Human vs Pc, conforme o jogador está definido para ser o primeiro (por default é o humano), este deverá jogar. Caso seja o computador a jogar, este fará automáticamente, dentro do tempo definido, caso seja o humano, este deverá indicar onde pretende jogar, indicando a direção do arco (vertical ou horizontal) e a respetiva posição (linha e coluna).

```
Estado anterior:
+--+  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +

======================
(jogada do utilizador)

Direction (v/h): v
Column: 1
Line: 1
======================

Estado após jogada:
+--+  +  +  +  +  +
|                    
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
                     
+  +  +  +  +  +  +
```

Caso a jogada seja inválida (fora do tabuleiro ou direção inválida), este será indicado para jogar de novo até concluir uma jogada corretamente. Caso feche uma caixa, será indicado para jogar novamente.

No final de cada jogada do computador, será apresentada uma tabela com as estatisticas dessa jogada, nomeadamente apresentará o número de nós analisados, expandidos, o número de cortes alfa e beta e por fim, o tempo que essa jogada demorou, em milisegundos.
Fora dessa tabela, será apresentado o estado de jogo, representado por uma lista composta por 2 listas, sendo a 1ª o estado de jogo (composta por mais 2 listas, lista de arcos horizontais e lista de arcos verticais) e 2ª uma lista com 2 elementos, representado o número de caixas fechadas pelo jogador 1 e 2, respetivamente.

```lisp
 ______________________________________________________ 
|                      STATS                           |
|                  Player: 2                           |
|                  Analysed Nodes:    21               |
|                  Expanded Nodes:     93              |
|                  Alfa Cuts:    0                     |
|                  Beta Cuts:   11                     |
|                  Time:     2 ms                      |
|______________________________________________________|

Estado: ((((1 2 1 2 1 2) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1) (1 1 1 1 1 1)) ((2 1 2 1 2) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1) (1 1 1 1 1))) (12 18))
```

Ao mesmo tempo, será produzido um ficheiro de texto `log.dat` onde ficará registada uma cópia destes dados apresentados na tabela, para ser possivel realizar uma análise posteriormente.