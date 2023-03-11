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

;; cria um no com o estado e a profundidade
(defun cria-no (estado &optional (depth 0) caixas-anterior)
  (list estado depth caixas-anterior)
)

;; retorna o estado do no
(defun no-estado (no)
  (car no)
)

(defun no-tabuleiro (no)
  (car (car no))
)

;; retorna o numero de caixas fechadas
(defun no-caixas-fechadas (no)
  (second (car no))
)

;; retorna a profundidade do no
(defun no-profundidade (no)
  (second no)
)

(defun no-caixas-fechadas-antes (no)
  (third no)
)

;; operadores do problema
(defun operators ()
 (list 'arco-horizontal 'arco-vertical)
)

;; cria um novo estado, constituido pelo tabuleiro
;; e pelo numero de caixas fechadas por cada jogador
;; nesse tabuleiro
(defun criar-estado (tabuleiro caixas-fechadas)
  (list tabuleiro caixas-fechadas)
)

;; player = 1 ou 2, valor caixas antigo = (5, 8)
(defun incrementar-caixas-player (player dif-novas-caixas valor-caixas-antigo)
  (cond ((= player *jogador1*) (list (+ dif-novas-caixas (first valor-caixas-antigo)) (second valor-caixas-antigo)))
        ((= player *jogador2*) (list (first valor-caixas-antigo) (+ dif-novas-caixas (second valor-caixas-antigo))))
  )
)

;;Funções auxiliares

;; 4. substituir: Função que recebe um índice, uma lista e valor x e deverá substituir o elemento nessa
;; posição pelo valor x, que deve ser definido com o valor de default a 1.
;; Exemplo: (substituir 1 (car (get-arcos-horizontais (tabuleiro-teste)))) => (1 0 0)
;; Exemplo: (substituir 2 (car (get-arcos-verticais (tabuleiro-teste))) 2) => (0 2 0)
(defun substituir (indice lista-arcos &optional (player 1)) 
  (cond ((null lista-arcos) nil)
        ((= indice 1) (cons player (cdr lista-arcos)))
        (t (cons (car lista-arcos) (substituir (- indice 1) (cdr lista-arcos) player)))
  )
)

;; 5. arco-na-posicao: Insere um arco (representado pelo valor 1) numa lista que representa o conjunto de
;; arcos horizontais ou verticais de um tabuleiro. A posição do arco será indicada através de dois índices
;; recebidos por parâmetro, em que o primeiro indica a posição da lista de arcos e o segundo a qual o
;; arco dentro dessa lista.
;; Exemplo: (arco-na-posicao 2 2 (get-arcos-horizontais (tabuleiro-teste))) => ((0 0 0) (0 1 1) (0 1 1) (0 0 1))
;; Exemplo: (arco-na-posicao 4 1 (get-arcos-verticais (tabuleiro-teste))) => ((0 0 0) (0 1 1) (1 0 1) (1 1 1))
(defun arco-na-posicao (posicao-arco posicao-elemento lista-arcos &optional (player 1))
  (cond ((null lista-arcos) nil)
    		((= posicao-arco 1) (cons (substituir posicao-elemento (car lista-arcos) player) (cdr lista-arcos)))
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
(defun arco-horizontal (indice1 indice2 tabuleiro &optional (player 1)) 
    (cond ((null tabuleiro) nil)
        	((or (< (length (car tabuleiro)) indice1) (>= 0 indice1) ) nil)
        	((or (< (length (caar tabuleiro)) indice2) (>= 0 indice2) ) nil)
        	((= (get-arco-na-posicao indice1 indice2 (get-arcos-horizontais tabuleiro)) 1) nil)
        	(t (cons (arco-na-posicao indice1 indice2 (get-arcos-horizontais tabuleiro) player) (cdr tabuleiro)))
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
(defun arco-vertical (indice1 indice2 tabuleiro &optional (player 1)) 
		(cond ((null tabuleiro) nil)
					((or (< (length (second tabuleiro)) indice1) (>= 0 indice1) ) nil)
					((or (< (length (cadr tabuleiro)) indice2) (>= 0 indice2) ) nil)
					((= (get-arco-na-posicao indice1 indice2 (get-arcos-verticais tabuleiro)) 1) nil)
					(t (list (car tabuleiro) (arco-na-posicao indice1 indice2 (get-arcos-verticais tabuleiro) player)))
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
				((not (= (car lista-arcos) 0)) (get-indices-arcos-vazios-aux (cdr lista-arcos) (+ indice 1)))
				(t (cons indice (get-indices-arcos-vazios-aux (cdr lista-arcos) (+ indice 1))))
	)
)

;; verifica se o tabuleiro está preenchido (nao existe)
;; possibilidade de por mais nenhum arco ou entao se
;; o numero de caixas corresponde a 30
(defun tabuleiro-preenchidop (no)
  ;; verificar se ha arcos vazios no tabuleiro
  ; (and 
  ;   (null (get-indices-arcos-vazios (get-arcos-horizontais (no-tabuleiro no)))) 
  ;   (null (get-indices-arcos-vazios (get-arcos-verticais (no-tabuleiro no))))
  ; )

  ;; verificar se o numero de caixas fechadas = 30
  (= (apply #'+ (no-caixas-fechadas no)) 30)
)

;; aplica o operador ao tabuleiro e retorna o novo tabuleiro com o arco colocado na posicao indicada
;; incrementando a profundidade deste novo tabuleiro em 1
(defun novo-sucessor (no operador op-params jogador caixas-antes &optional (inc-depth 1))
  (let* ((estado-sucessor (funcall operador (first op-params) (second op-params) (no-tabuleiro no) jogador)) (caixas-dif (- (closed-boxes estado-sucessor) caixas-antes)))
    (cria-no (criar-estado estado-sucessor (incrementar-caixas-player jogador caixas-dif (no-caixas-fechadas no))) (+ (no-profundidade no) inc-depth) (no-caixas-fechadas-antes no))
  )
)

;; recebe um tabuleiro, os operadores que podem ser aplicados ao mesmo e respetivo player
;; retorna uma lista com os sucessores do tabuleiro recebido
(defun sucessores (no jogador &optional (inc-depth 1))
  (if (tabuleiro-preenchidop no)
    (list no)
    (let* ((numero-caixas-no (apply #'+ (no-caixas-fechadas no)))
           (nos-sucessores 
            (append
    	        (mapcar (lambda (xycoords) (novo-sucessor no (first (operators)) xycoords jogador numero-caixas-no inc-depth))
                (get-indices-arcos-vazios (get-arcos-horizontais (no-tabuleiro no))) ;;lista de indices de arcos horizontais vazios
              )
    	        (mapcar (lambda (xycoords) (novo-sucessor no (second (operators)) xycoords jogador numero-caixas-no inc-depth)) 
                (get-indices-arcos-vazios (get-arcos-verticais (no-tabuleiro no))) ;;lista de indices de arcos verticais vazios
              )
            )
           )
          )
          (remove-duplicates (play-again numero-caixas-no nos-sucessores jogador) :test #'equal)
    )
  )
)

;; verifica se o jogador que estava a jogar, fechou uma caixa e no caso
;; de ter fechado, esse jogador fara uma nova jogada, ate a jogada nao
;; fechar pelo menos 1 caixa
(defun play-again (caixas-atual nos-sucessores jogador)
  (cond ((null nos-sucessores) nil)
        ((> (apply #'+ (no-caixas-fechadas (car nos-sucessores))) caixas-atual) 
          (append (sucessores (car nos-sucessores) jogador 0) 
                  (play-again caixas-atual (cdr nos-sucessores) jogador)
          )
        )
        (t (cons (car nos-sucessores) (play-again caixas-atual (cdr nos-sucessores) jogador)))
  )
)

;; recebe um tabuleiro e o numero de caixas fechadas
(defun closed-boxes (table &optional (row 1) (col 1))
  (cond ((null table) nil)
        ((>= row (length (get-arcos-horizontais table))) 0)
        ((>= col (length (get-arcos-verticais table))) (closed-boxes table (+ row 1) 1))
        ((and (not (= (get-arco-na-posicao row col (get-arcos-horizontais table)) 0))
              (not (= (get-arco-na-posicao (+ row 1) col (get-arcos-horizontais table)) 0))
              (not (= (get-arco-na-posicao col row (get-arcos-verticais table)) 0))
              (not (= (get-arco-na-posicao (+ col 1) row (get-arcos-verticais table)) 0))
          )
         (+ 1 (closed-boxes table row (+ col 1)))
        )
        (t (closed-boxes table row (+ col 1)))
  )
)

;; contar quantas "caixas" estao a 1 arco de serem fechadas
;; 3 lados completos e ignora as que ja estao fechadas
;; que tecnicamente tambem tem 3 lados completos
(defun 3-side-boxes (table &optional (row 1) (col 1))
  (cond ((null table) nil)
        ((>= row (length (get-arcos-horizontais table))) 0)
        ((>= col (length (get-arcos-verticais table))) (3-side-boxes table (+ row 1) 1))
        (
          (and
            (or
              (and (not (= (get-arco-na-posicao row col (get-arcos-horizontais table)) 0))
                   (not (= (get-arco-na-posicao (+ row 1) col (get-arcos-horizontais table)) 0))
                   (not (= (get-arco-na-posicao col row (get-arcos-verticais table)) 0))
              )
              (and (not (= (get-arco-na-posicao row col (get-arcos-horizontais table)) 0))
                   (not (= (get-arco-na-posicao (+ row 1) col (get-arcos-horizontais table)) 0))
                   (not (= (get-arco-na-posicao (+ col 1) row (get-arcos-verticais table)) 0))
              )
              (and (not (= (get-arco-na-posicao row col (get-arcos-horizontais table)) 0))
                   (not (= (get-arco-na-posicao col row (get-arcos-verticais table)) 0))
                   (not (= (get-arco-na-posicao (+ col 1) row (get-arcos-verticais table)) 0))
              )
              (and (not (= (get-arco-na-posicao (+ row 1) col (get-arcos-horizontais table)) 0))
                   (not (= (get-arco-na-posicao col row (get-arcos-verticais table)) 0))
                   (not (= (get-arco-na-posicao (+ col 1) row (get-arcos-verticais table)) 0))
              )
            )
            (not 
              (and 
                (not (= (get-arco-na-posicao row col (get-arcos-horizontais table)) 0))
                (not (= (get-arco-na-posicao (+ row 1) col (get-arcos-horizontais table)) 0))
                (not (= (get-arco-na-posicao col row (get-arcos-verticais table)) 0))
                (not (= (get-arco-na-posicao (+ col 1) row (get-arcos-verticais table)) 0))
              )
            )
          )
         (+ 1 (3-side-boxes table row (+ col 1)))
        )
        (t (3-side-boxes table row (+ col 1)))
  )
)

;;troca o jogador
(defun trocar-jogador (jogador)
  "Realiza a troca entre jogadores"
  (cond ((= jogador *jogador1*) *jogador2*)
        (t *jogador1*)
  )
)

;; retorna o numero de caixas fechadas pelo jogador
(defun caixas-fechadas-jogador (jogador no)
  "Retorna o numero de caixas fechadas pelo jogador"
  (cond ((= jogador *jogador1*) (first (no-caixas-fechadas no)))
        (t (second (no-caixas-fechadas no)))
  )
)

;; avalia um no em relacao às caixas fechadas de um jogador
;; em relaçao ao outro
(defun avaliar (no)
  (cond ((= *initial-player* *jogador1*) (- (caixas-fechadas-jogador *jogador1* no) (caixas-fechadas-jogador *jogador2* no)))
        (t (- (caixas-fechadas-jogador *jogador2* no) (caixas-fechadas-jogador *jogador1* no)))
  )
)

;; avalia um no em relacao às caixas fechadas de um jogador
;; em relaçao ao outro e ao numero de caixas que deixa por
;; fechar, podendo ser fechadas pelo outro jogador, o que
;; é mau para o que jogou
(defun avaliar-v2 (no)
  (cond ((= *initial-player* *jogador1*) (- (- (* (caixas-fechadas-jogador *jogador1* no) 10) (* (3-side-boxes (no-tabuleiro no)) 5)) (* (caixas-fechadas-jogador *jogador2* no) 10)))
        (t (- (- (* (caixas-fechadas-jogador *jogador2* no) 10) (* (3-side-boxes (no-tabuleiro no)) 5)) (* (caixas-fechadas-jogador *jogador1* no) 10)))
  )
)

;; guarda os valores de alfa e o estado em variaveis
;; globais
(defun guardar-solucao (no value)
  (if (> value *best-alfa-value*)
    (progn 
      (setq *best-alfa-value* value)
      (setq *estado* no)
    )
  )
)