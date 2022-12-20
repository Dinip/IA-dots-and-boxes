# Projeto : Fase 1 
## Inteligência Artificial 22/23 
Prof. Joaquim Filipe 

Eng. Filipe Mariano 

# Dots and Boxes
**Dots and Boxes** é um [jogo de lápis e papel](https://en.wikipedia.org/wiki/Pencil_and_paper_game "jogo de lápis e papel") para dois jogadores. 
Este tem como objetivo fechar o maior numero de caixas possíveis. Aplicando apenas riscos ligando os pontos apresentados no tabuleiro.

## Manual de Utilizador
Realizador por: 

Dinis Pimpão -> 201901055 

Pedro Peralta -> 202002153

## Índice
 1. Introdução
 2. Instalação
 3. Interface


## 1 - Introdução
O manual de utilizador tem como objetivo facilitar a instalação e a utilização de todo o projeto.


## 2 - Instalação
Para a utilização do projeto é necessário o IDE LispWorks ou qualquer outro que seja possível compilar e executar o código Lisp.

O projeto foi dividido em 3 ficheiros:
- projeto.lisp -> Carrega os outros ficheiros de código, escreve e lê ficheiros, e trata da interação com o utilizador.
- puzzle.lisp -> Código relacionado com o problema. 
- procura.lisp -> Contem a implementação de: 
    - 1. Algoritmo de Procura de Largura Primeiro (BFS) 
    - 2. Algoritmo de Procura do Profundidade Primeiro (DFS) 
    - 3. Algoritmo de Procura do Melhor Primeiro (A) 
    - 4. Os algoritmos SMA, IDA* e/ou RBFS (caso optem por implementar o bónus)


## 3 - Interface

O programa inicia-se chamando a função iniciar. Esta irá realizar várias questões ao utilizador, nomeadamente para importar o ficheiro de problemas, escolher qual quer resolver, com que algoritmo, com que depth ou heuristica e no fim apresenta os resultados encontrados, perguntado ao user se pretende guardar estes valores num ficheiro.

```lisp
CL-USER 1 > (iniciar)
```


Assim que se inicia o programa, é pedido ao utilizador o caminho desde a diretoria raiz até ao ficheiro onde se encontram, "problemas.dat".
Para garantir que o ficheiro é inserido no formato correto e que este existe, é utilizada a seguinte função que apresenta um exemplo de como deve ser o caminho introduzido, seguindo de uma verficação se o mesmo existe no sistema.
```lisp
(defun ler-caminho-ficheiro (&optional (verify nil))  
  (format t "Qual o caminho do ficheiro? (sem aspas, no formato C:/Users/User/Documents/IA/ficheiro.dat) -> ")
  (let ((ficheiro (read-line)))
    (cond (verify 
      (cond ((probe-file ficheiro) ficheiro)
            (t (progn (format t "Ficheiro nao encontrado. ~%") (ler-caminho-ficheiro)))
      ))
      (T ficheiro)
    )
  )
)
```

```lisp
Qual o caminho do ficheiro? (sem aspas, no formato C:/Users/User/Documents/IA/ficheiro.dat) ->
```

Seguidamente são efetuadas as questões ao utilizador para este selecionar o problema que pretende resolver, respetivo algoritmo e possíveis opções dentro deste. Pode-se observar esse fluxo abaixo:

```lisp
1 - Problema a 
2 - Problema b 
3 - Problema c 
4 - Problema d 
5 - Problema e 
6 - Problema f
Qual o problema que quer resolver? -> 1
```

Seguidamente são apresentadas as informações relativas ao problema escolhido
```lisp
Representacao do nivel escolhido: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 
Caixas a fechar do nivel escolhido: 3 

Representacao grafica do nivel escolhido: 
+  +  +  +
            
+  +  +--+
   |     |  
+  +--+--+
      |  |  
+  +  +--+
```

São iniciadas as questões sobre o método de procura a ser utilizado:

```lisp
1 - Procura na largura 
2 - Procura na profundidade 
3 - Procura A* 
Que algoritmo de procura quer utilizar? -> 2
```

(Caso seja o algoritmo DFS)
```lisp
Qual a profundidade limite? -> 10
```


(Caso seja o algoritmo Astar)
```
1 - Euristica dada h(x) = objetivo_caixas - caixas_fechadas

2 - Euristica propria v1 h(x) = (objetivo_caixas - caixas_fechadas) * (|numero_arcos_horizontais_colocados + numero_arcos_verticais_nao_colocados| + 1)

3 - Euristica propria v2 h(x) = (objetivo_caixas - caixas_fechadas) * (numero_arcos_horizontais_nao_colocados + numero_arcos_verticais_nao_colocados)

Que heuristica quer usar? -> 1
```

Por fim, são apresentados os resultados do problema, se encontrados, ou uma mensagem de erro a indicar que não foi possivel encontrar solução. É apresentado o progresso dos estados até chegar ao nó final e respetivas estatisticas.

```lisp
1 estado 
No estado: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 
+  +  +  +
            
+  +  +--+
   |     |  
+  +--+--+
      |  |  
+  +  +--+
 

2 estado 
No estado: (((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 
+  +  +  +
            
+  +  +--+
   |  |  |  
+  +--+--+
      |  |  
+  +  +--+
 

3 estado 
No estado: (((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 
+  +  +  +
            
+  +--+--+
   |  |  |  
+  +--+--+
      |  |  
+  +  +--+
 
No solucao ((((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 2 0 ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 1 0 ((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 0 2 NIL))) 
Numero de nos gerados: 46 
Numero de nos expandidos: 3 
Profundidade da solucao: 2 
Penetrancia: 0.04347826 
Fator Ramificacao Media: 6.3010926 
Tempo de execucao: 0.0 s 
```

Por fim, é dada a opção ao utilizador se este quer guardar estes resultados num ficheiro.

```lisp
1 - Sim 
2 - Nao 
Deseja guardar o resultado num ficheiro? -> 1

Qual o caminho do ficheiro? (sem aspas, no formato C:/Users/User/Documents/IA/ficheiro.dat) ->
```