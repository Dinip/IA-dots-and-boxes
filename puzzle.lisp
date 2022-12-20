;;Seletores

;; 1. get-arcos-horizontais: Retorna a lista dos arcos horizontais de um tabuleiro
;; Exemplo: (get-arcos-horizontais (tabuleiro-teste)) => ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
(defun get-arcos-horizontais (tabuleiro)
	(first tabuleiro)
)

;; 2. get-arcos-verticais: Retorna a lista dos arcos verticiais de um tabuleiro.
;; Exemplo: (get-arcos-verticais (tabuleiro-teste)) => ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
(defun get-arcos-verticais (tabuleiro)
	(second tabuleiro)
)

;; 3. get-arco-na-posicao: Função que retorna o arco que se encontra numa posicao da lista de arcos horizontais ou verticais.
;; Exemplo: devolve o arco existente na 2ª lista dos arcos horizontais na posição 3
;; (get-arco-na-posicao 2 3 (get-arcos-horizontais (tabuleiro-teste))) => (1)
(defun get-arco-na-posicao (posicao-arco posicao-elemento lista-arcos)
	(cond ((null lista-arcos) nil)
        (t (nth (- posicao-elemento 1) (nth (- posicao-arco 1) lista-arcos)))
  )
)

;; cria um no com o estado, a profundidade, a heuristica e o pai
(defun cria-no (tabuleiro &optional (g 0) heuristica (pai nil))
  (list tabuleiro g heuristica pai)
)

;; retorna o estado do no
(defun no-estado (no)
  (car no)
)

;; retorna a profundidade do no
(defun no-profundidade (no)
  (second no)
)

;; retorna o valor da heuristica do no
(defun no-heuristica (no)
  (third no)
)

;; retorna o pai do no
(defun no-pai (no)
  (fourth no)
)

;; operadores do problema
(defun operators ()
 (list 'arco-horizontal 'arco-vertical)
)

;;Funções auxiliares

;; 4. substituir: Função que recebe um índice, uma lista e valor x e deverá substituir o elemento nessa
;; posição pelo valor x, que deve ser definido com o valor de default a 1.
;; Exemplo: (substituir 1 (car (get-arcos-horizontais (tabuleiro-teste)))) => (1 0 0)
;; Exemplo: (substituir 2 (car (get-arcos-verticais (tabuleiro-teste))) 2) => (0 2 0)
(defun substituir (indice lista-arcos &optional (x 1)) 
  (cond ((null lista-arcos) nil)
        ((= indice 1) (cons x (cdr lista-arcos)))
        (t (cons (car lista-arcos) (substituir (- indice 1) (cdr lista-arcos) x)))
  )
)

;; 5. arco-na-posicao: Insere um arco (representado pelo valor 1) numa lista que representa o conjunto de
;; arcos horizontais ou verticais de um tabuleiro. A posição do arco será indicada através de dois índices
;; recebidos por parâmetro, em que o primeiro indica a posição da lista de arcos e o segundo a qual o
;; arco dentro dessa lista.
;; Exemplo: (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste))) => ((0 0 0) (0 1 1) (0 1 1) (0 0 1))
;; Exemplo: (arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste))) => ((0 0 0) (0 1 1) (1 0 1) (1 1 1))
(defun arco-na-posicao (posicao-arco posicao-elemento lista-arcos)
  (cond ((null lista-arcos) nil)
    		((= posicao-arco 1) (cons (substituir posicao-elemento (car lista-arcos)) (cdr lista-arcos)))
    		(t (cons (car lista-arcos) (arco-na-posicao (- posicao-arco 1) posicao-elemento (cdr lista-arcos))))
  )
)


;;Operadores

;; 6. arco-horizontal: Função que recebe dois índices e o tabuleiro e coloca um arco horizontal nessa
;; posição. A função deverá retornar NIL caso já exista um arco colocado nessa posição ou caso a posição
;; indicada seja fora dos limites do tabuleiro.
;; Exemplo: (arco-horizontal 3 1 (tabuleiro-teste)) =>
;; (
;; ((0 0 0) (0 0 1) (1 1 1) (0 0 1))
;; ((0 0 0) (0 1 1) (1 0 1) (0 1 1))
;; )
;; Exemplo: (arco-horizontal 3 2 (tabuleiro-teste)) => NIL
;; Exemplo: (arco-horizontal 7 2 (tabuleiro-teste)) => NIL
(defun arco-horizontal (indice1 indice2 tabuleiro) 
    (cond ((null tabuleiro) nil)
        	((or (< (length (car tabuleiro)) indice1) (>= 0 indice1) ) nil)
        	((or (< (length (caar tabuleiro)) indice2) (>= 0 indice2) ) nil)
        	((= (get-arco-na-posicao indice1 indice2 (get-arcos-horizontais tabuleiro)) 1) nil)
        	(t (cons (arco-na-posicao indice1 indice2 (get-arcos-horizontais tabuleiro)) (cdr tabuleiro)))
    )
)

;; 7. arco-vertical: Função que recebe dois índices e o tabuleiro e coloca um arco vertical nessa posição.
;; A função deverá retornar NIL caso já exista um arco colocado nessa posição ou caso a posição indicada
;; seja fora dos limites do tabuleiro.
;; 

;; (arco-vertical 1 2 (tabuleiro-teste)) =>
;; (
;; 	((0 0 0) (0 0 1) (0 1 1) (0 0 1))
;; 	((0 0 0) (1 1 1) (1 0 1) (0 1 1))
;; )
(defun arco-vertical (indice1 indice2 tabuleiro) 
		(cond ((null tabuleiro) nil)
					((or (< (length (second tabuleiro)) indice1) (>= 0 indice1) ) nil)
					((or (< (length (cadr tabuleiro)) indice2) (>= 0 indice2) ) nil)
					((= (get-arco-na-posicao indice1 indice2 (get-arcos-verticais tabuleiro)) 1) nil)
					(t (list (car tabuleiro) (arco-na-posicao indice1 indice2 (get-arcos-verticais tabuleiro))))
		)
)

;; funcoes auxiliares para os operadores
;; (get-arcos-horizontais (tabuleiro-teste)) => ((0 0 0) (0 0 1) (0 1 1) (0 0 1))
;; (get-indices-arcos-vazios (get-arcos-horizontais (tabuleiro-teste))) => ((1 1) (1 2) (1 3) (2 1) (2 2) (3 1) (4 1) (4 2))
(defun get-indices-arcos-vazios (lista-arcos &optional (indice 1))
	(cond ((null lista-arcos) nil)
				(t (append (mapcar #'(lambda (x) (list indice x)) (get-indices-arcos-vazios-aux (car lista-arcos) 1)) (get-indices-arcos-vazios (cdr lista-arcos) (+ indice 1))))
	)
)

(defun get-indices-arcos-vazios-aux (lista-arcos indice)
	(cond ((null lista-arcos) nil)
				((= (car lista-arcos) 1) (get-indices-arcos-vazios-aux (cdr lista-arcos) (+ indice 1)))
				(t (cons indice (get-indices-arcos-vazios-aux (cdr lista-arcos) (+ indice 1))))
	)
)

;; aplica o operador ao tabuleiro e retorna o novo tabuleiro com o arco colocado na posicao indicada
;; incrementando a profundidade deste novo tabuleiro em 1, calculando a sua heuristica e definindo o pai
;; como o tabuleiro recebido inicialmente
(defun novo-sucessor (tabuleiro operador op-params &optional (fn-heuristica nil) (valor-objetivo nil))
  (let ((estado-sucessor (funcall operador (first op-params) (second op-params) (no-estado tabuleiro))))
    (cond ((null fn-heuristica) (cria-no estado-sucessor (+ (no-profundidade tabuleiro) 1) 0 tabuleiro))
          (T (cria-no estado-sucessor (+ (no-profundidade tabuleiro) 1) (funcall fn-heuristica estado-sucessor valor-objetivo) tabuleiro))
    )
  )
)

;; recebe um tabuleiro, os operadores que podem ser aplicados ao mesmo e o algoritmo a ser utilizado
;; e retorna uma lista com os sucessores do tabuleiro recebido conforme o algoritmo escolhido
;; no caso do dfs, caso o no do tabuleiro recebido tenha profundidade igual ao depth maximo
;; retorna uma lista vazia de sucessores
(defun sucessores (tabuleiro operators algorithm &optional depth-ou-objetivo (fn-heuristica nil))
  (cond ((eq algorithm 'bfs) 
					(append
						(mapcar (lambda (xycoords) (novo-sucessor tabuleiro (first (operators)) xycoords)) (get-indices-arcos-vazios (get-arcos-horizontais (no-estado tabuleiro))))
						(mapcar (lambda (xycoords) (novo-sucessor tabuleiro (second (operators)) xycoords)) (get-indices-arcos-vazios (get-arcos-verticais (no-estado tabuleiro))))
					)
				)
        ((eq algorithm 'dfs) 
          (cond ((< (no-profundidade tabuleiro) depth-ou-objetivo) 
                 (append
					       	(mapcar (lambda (xycoords) (novo-sucessor tabuleiro (first (operators)) xycoords)) (get-indices-arcos-vazios (get-arcos-horizontais (no-estado tabuleiro))))
					       	(mapcar (lambda (xycoords) (novo-sucessor tabuleiro (second (operators)) xycoords)) (get-indices-arcos-vazios (get-arcos-verticais (no-estado tabuleiro))))
					       ))
                (T nil)
          )
        )
        ((eq algorithm 'astar) 
          (append
						(mapcar (lambda (xycoords) (novo-sucessor tabuleiro (first (operators)) xycoords fn-heuristica depth-ou-objetivo)) (get-indices-arcos-vazios (get-arcos-horizontais (no-estado tabuleiro))))
						(mapcar (lambda (xycoords) (novo-sucessor tabuleiro (second (operators)) xycoords fn-heuristica depth-ou-objetivo)) (get-indices-arcos-vazios (get-arcos-verticais (no-estado tabuleiro))))
					)
        )
  )
)


;; funcao que verifica se o no ja existe na lista
;; conforme o algorimo de busca, esta funcao pode precisar
;; de ter em conta a profundidade do no ou o custo do no
;; NOTA: esta funcao nao esta a ser usada pois foi substituida
;; por funcoes mais eficientes (nao recursivas) de forma a tentar
;; executar alguns algoritmos que davam stack overflow, mas
;; retirar a recursividade destas funcoes nao foi suficiente
(defun no-existep (no lista algoritmo)
  (cond ((null lista) nil)
        ((equal (no-estado no) (no-estado (car lista))) 
         (cond ((equal algoritmo 'bfs) T)
               ((equal algoritmo 'dfs)
                 (cond ((>= (no-profundidade no) (no-profundidade (first lista))) T)
                       (T nil)
                 )
                )
                ((equal algoritmo 'astar)
                 (cond ((>= (calcular-custo-no-astar no) (calcular-custo-no-astar (first lista))) T)
                       (T nil)
                 )
                )
         )
        )
        (T (no-existep no (cdr lista) algoritmo))
  )
)

;; junta os sucessores ao fim da lista de abertos para o bfs
(defun abertos-bfs (listaAbertos listaSucessores)
  (append listaAbertos listaSucessores)
)

;; junta os sucessores ao inicio da lista de abertos para o dfs
(defun abertos-dfs (listaAbertos listaSucessores)
  (append listaSucessores listaAbertos)
)

;; devolve os nos da primeira lista (sucessores) que nao existem na segunda lista (fechados)
;; comparando apenas os estados
(defun nos-unicos-bfs (fechados sucessores)
	(set-difference sucessores fechados :test 'equal-states)
)

(defun nos-unicos-dfs (abertos fechados sucessores)
	(set-difference sucessores (append abertos fechados) :test 'equal-states-depth)
)

;; comprar estados de 2 nos para o set-difference
(defun equal-states (no1 no2)
  (equal (no-estado no1) (no-estado no2))
)

;;remove sucessores que ja estao em fechados, funcao auxiliar
(defun remove-sucessores-from-fechados (fechados sucessores)
  (set-difference fechados sucessores :test 'equal-states)
)


;; comprar estados de 2 nos, se forem iguais, verifica se a profundidade do no1 e maior que a do no2
;; se for, devolve nil, senao T o que significa que o no ja existe
;;(set-difference '(((((0 0 0)(0 0 1)(0 1 1)(0 0 1))((0 0 0)(0 1 0)(0 0 1)(0 1 1))) 2 1 nil)) '(((((0 0 0)(0 0 1)(0 1 1)(0 0 1))((0 0 0)(0 1 0)(0 0 1)(0 1 1))) 4 1 nil)) :test 'equal-states-depth)
;;result: (((((0 0 0) (0 0 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 0 1) (0 1 1))) 2 1 NIL)) (mesmo no nos sucessores, nos fechados tinha depth 4 e passou a ter 2)
(defun equal-states-depth (no1 no2)
  (cond ((and (equal (no-estado no1) (no-estado no2)) (>= (no-profundidade no1) (no-profundidade no2))) T)
         (T nil)
  )
)

;;remove da lista todos os nos que satisfazem a funcao objetivo, caso nenhum satisfaça, devolve nil
;;exemplo: (no-objetivo '(((((0 0 0)(0 1 1)(0 1 1)(0 0 1))((0 0 0)(0 1 0)(0 1 1)(0 1 1))) 2 1 nil) ((((0 0 0)(0 0 1)(0 1 1)(0 0 1))((0 0 0)(0 1 0)(0 0 1)(0 1 1))) 0 1 nil)) 'no-solucaop 3)
;; (((((0 0 0) (0 1 1) (0 1 1) (0 0 1)) ((0 0 0) (0 1 0) (0 1 1) (0 1 1))) 2 1 NIL))
(defun no-objetivo (lista-nos funObj objetivo)
	(remove-if-not (lambda (no) (funcall funObj no objetivo)) lista-nos)
)

;; recebe um tabuleiro e o numero de caixas fechadas
(defun closed-boxes (table &optional (row 1) (col 1))
  (cond ((null table) nil)
        ((>= row (length (get-arcos-horizontais table))) 0)
        ((>= col (length (get-arcos-verticais table))) (closed-boxes table (+ row 1) 1))
        ((and (= (get-arco-na-posicao row col (get-arcos-horizontais table)) 1)
              (= (get-arco-na-posicao (+ row 1) col (get-arcos-horizontais table)) 1)
              (= (get-arco-na-posicao col row (get-arcos-verticais table)) 1)
              (= (get-arco-na-posicao (+ col 1) row (get-arcos-verticais table)) 1)
          )
         (+ 1 (closed-boxes table row (+ col 1)))
        )
        (t (closed-boxes table row (+ col 1)))
  )
)

;;verifica se o no é solução para o problema do bfs e dfs
(defun no-solucaop (no objetivo)
  (cond ((= (closed-boxes (no-estado no)) objetivo) T)
        (T nil)
  )
)

;;verifica se o no é solução para o problema do astar
(defun no-solucaop-astar (no)
  (cond ((= (no-heuristica no) 0) T)
        (T nil)
  )
)

;;sucessores que ainda não existem em ABERTOS nem FECHADOS
;;devolve apenas nos que sejam diferentes aos nos em fechados
(defun nos-unicos-astar (abertos fechados sucessores)
  (set-difference sucessores (append abertos fechados) :test 'equal-states-custo)
)

(defun equal-states-custo (no1 no2)
  (cond ((and (equal (no-estado no1) (no-estado no2)) (>= (calcular-custo-no-astar no1) (calcular-custo-no-astar no2))) T)
         (T nil)
  )
)

;; funcao auxiliar para calcular o custo de um no (profundidade + heuristica)
(defun calcular-custo-no-astar (no)
  (+ (no-profundidade no) (no-heuristica no))
)

;; funcao auxiliar para ordenar uma lista de nos por custo ascendente (profundidade + heuristica)
(defun ordenar-nos (lista)
  (sort lista #'< :key #'calcular-custo-no-astar)
)

;; funcao auxiliar para juntar uma lista de sucessores e de abertos para o algorithmo A*
(defun colocar-sucessores-em-abertos (sucessores abertos)
  (ordenar-nos (append sucessores abertos))
)

;; funcao que calcula a heuristica para o algoritmo A* (numero de caixas a fechar (objetivo) - numero de caixas fechadas)
(defun calcular-heuristica-dada (no-estado objetivo)
  (- objetivo (closed-boxes no-estado))
)

;; passando um conjunto de arcos (horizontal ou vertical) devolve a quantidade de arcos nao colocados (a 0)
(defun get-arcos-nao-colocados (h-ou-v)
  (reduce #'+ (mapcar #'(lambda (x) (reduce #'+ (mapcar #'(lambda (y) (if (= y 0) 1 0)) x))) h-ou-v))
)

;; passando um conjunto de arcos (horizontal ou vertical) devolve a quantidade de arcos colocados (a 1)
(defun get-arcos-colocados (h-ou-v)
  (reduce #'+ (mapcar #'(lambda (x) (reduce #'+ x)) h-ou-v))
)

;; funcao que calcula a heuristica para o algoritmo A* (implementacao de uma heuristica propria)
;; (objetivo_caixas - caixas_fechadas) * (|numero_arcos_horizontais_colocados + numero_arcos_verticais_nao_colocados| + 1)
(defun calcular-heuristica-propria (no-estado objetivo)
  (* (- objetivo (closed-boxes no-estado)) (+ 1 (abs (- (get-arcos-nao-colocados (get-arcos-horizontais no-estado)) (get-arcos-nao-colocados (get-arcos-verticais no-estado))))))
)

;; funcao que calcula a heuristica para o algoritmo A* (implementacao de uma heuristica propria v2)
;; (objetivo_caixas - caixas_fechadas) * (numero_arcos_horizontais_nao_colocados + numero_arcos_verticais_nao_colocados)
(defun calcular-heuristica-propria-v2 (no-estado objetivo)
  (* (- objetivo (closed-boxes no-estado)) (+ (get-arcos-nao-colocados (get-arcos-horizontais no-estado)) (get-arcos-nao-colocados (get-arcos-verticais no-estado))))
)

;; calcula a penetrancia da árvore de procura (comprimento do caminho ate ao objetivo / numero de nos na solucao)
(defun penetrancia (l-value t-value)
  (float (/ l-value t-value))
)

;; funcao auxiliar para calcular o factor de ramificacao medio
(defun polinomial (b-value l-value t-value)
  "B + B^2 + ... + B^L=T"
  (cond ((= 1 l-value) (- b-value t-value))
    (T (+ (expt b-value l-value) (polinomial b-value (- l-value 1) t-value)))
  )
)

;; Devolve o factor de ramificacao, executando o metodo da bisseccao
(defun branching-factor (l-value t-value &optional (erro 0.001) (bmin 1) (bmax 10e11))
  (let ((bmedio (/ (+ bmin bmax) 2)))
    (cond ((< (- bmax bmin) erro) (/ (+ bmax bmin) 2))
          ((< (polinomial bmedio l-value t-value) 0) (branching-factor l-value t-value erro bmedio bmax))
          (t (branching-factor l-value t-value erro bmin bmedio))
    )
  )
)