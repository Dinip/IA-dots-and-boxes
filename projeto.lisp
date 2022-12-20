; (load (current-pathname "puzzle.lisp"))
; (load (current-pathname "procura.lisp"))

;; funcao para ler o ficheiro passado como argumento e devolver uma lista com os dados do ficheiro
;; o output consiste numa lista com listas de 2 elementos ((problema, caixas-a-fechar) (problema, caixas-a-fechar))
;; exemplo: (((((0 0 0)(0 0 1)(0 1 1)(0 0 1)) ((0 0 0)(0 1 0)(0 0 1)(0 1 1))) 3) ((((0 0 1 0)(1 1 1 1)(0 0 1 1)(0 0 1 1)(0 0 1 1))((0 0 1 1)(0 0 1 1)(1 1 1 1)(1 0 1 1)(0 1 1 1))) 7))
(defun fileread (path)
  (let ((in (open path :if-does-not-exist nil)) (out nil))
    (when in
      (setf out (mapcar-string-to-value (mapcar-split-string-plus (split-string (read-line in) #\/))))
      (close in)
    )
    out
  )
)

;; divide uma string em duas partes, a primeira ate ao char e a segunda a partir do char
;; chama a funcao recursivamente com a segunda parte da string
(defun split-string (string char)
  (let ((start 0) (end (position char string :start 1)) )
    (cond (end (cons (subseq string start end) (split-string (subseq string (1+ end)) char)))
          (t (list string))
    )
  )
)

;; aplica o split-string a cada elemento da lista
(defun mapcar-split-string-plus (lista)
  (mapcar #'(lambda (x) (split-string x #\+)) lista)
)

;; converte uma string para o seu valor efetivo
(defun mapcar-string-to-value (lista)
  (mapcar #'(lambda (x) (mapcar #'read-from-string x)) lista)
)

;; "desenha" o puzzle
;; (
;;  ((0 0 0)(0 0 1)(0 1 1)(0 0 1))
;;  ((0 0 0)(0 1 0)(0 0 1)(0 1 1))
;; )
;; +  +  +  +
;;   
;; +  +  +--+
;;    |     |
;; +  +--+--+
;;       |  |
;; +  +  +--+
(defun print-table (table)
  (let ((nlines (length (first table))) (ncolumns (length (second table))))
    (loop for lin from 0 to (- nlines 1)
      do (loop for col from 0 to (- ncolumns 2)
        do
          (format t "+")
          (cond ((= (nth col (nth lin (first table))) 1) (format t "--"))
                ((= (nth col (nth lin (first table))) 0) (format t "  "))
          )
      )
      (format t "+")
      (terpri)
      (cond ((< lin (- nlines 1))
            (loop for col2 from 0 to (- ncolumns 1)
              do
                (cond ((= (nth lin (nth col2 (second table))) 1) (format T "|"))
                      ((= (nth lin (nth col2 (second table))) 0) (format t " "))
                )
                (format t "  ")
            ))
            (t (format t " "))
      )
      (terpri)
    )
  )
)

;; pergunta ao utilizador o caminho para o ficheiro com os problemas
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

;; inicia o programa
(defun iniciar()
  (let* (
      (ficheiro (ler-caminho-ficheiro t))
      ; (ficheiro "C:/Users/Dinip/Downloads/problemas.dat")
      (problema (fileread ficheiro)) (nivel (escolher-nivel problema))
    )
      (print-infos-nivel nivel)
      (let* (
          (algoritmo (ler-algoritmo))
          (profundidade (cond ((eql algoritmo 'dfs) (ler-profundidade)) (T 0)))
          (heuristica (cond ((eql algoritmo 'astar) (escolher-euristica)) (T 'calcular-heuristica-dada)))
          (no (cria-no (first nivel) 0 (funcall heuristica (first nivel) (second nivel))))
          (starttime (get-internal-run-time))
      	  (resultado (cond 
		        ((equal algoritmo 'bfs) (bfs no 'no-solucaop 'sucessores (operators) (second nivel)))
		        ((equal algoritmo 'dfs) (dfs no 'no-solucaop 'sucessores (operators) profundidade (second nivel)))
		        ((equal algoritmo 'astar) (astar no 'no-solucaop-astar 'sucessores (operators) (second nivel) heuristica))
          ))
          (endtime (float(/ (- (get-internal-run-time) starttime) internal-time-units-per-second)))
        )
        (cond ((null (first resultado)) (format t "Solucao nao encontrada! ~%"))
              (t (mostrar-solucao resultado))
        )
        (format t "~%Tempo de execucao: ~a s ~%" endtime)
        (cond ((escolher-se-quer-output-file) (guardar-resultado-em-ficheiro (ler-caminho-ficheiro) nivel algoritmo profundidade heuristica resultado endtime)))
	    )
  )
)


;;apresenta as informacoes do resultado, nomeadamente faz um print dos tabuleiros,
;;numero de nos gerados, expandidos e a penetrancia e fator de ramificacao
(defun mostrar-solucao (resultado)
  (apresentar-progresso-estados (reverse (transform-result-to-list (first resultado))))
  (format t "No solucao ~a ~%" (first resultado))
  (format t "Numero de nos gerados: ~a ~%" (first (second resultado)))
  (format t "Numero de nos expandidos: ~a ~%" (second (second resultado)))
  (format t "Profundidade da solucao: ~a ~%" (no-profundidade (first resultado)))
  (format t "Penetrancia: ~a ~%" (penetrancia (no-profundidade (first resultado)) (first (second resultado))))
  (format t "Fator Ramificacao Media: ~a ~%" (branching-factor (no-profundidade (first resultado)) (first (second resultado))))
)

;; apresenta os problemas disponiveis (lido do ficheiro)
;; e trata da interacao com o utilizador para escolher 1
(defun escolher-nivel (problema)
  (let ((niveis (length problema)))
    (loop for i from 1 to niveis
      do (format t "~a - Problema ~a ~%" i (code-char (+ i 96)))
    )
    (format t "Qual o problema que quer resolver? -> ")
    (let ((resposta (read)))
      (cond ((and (<= resposta niveis) (> resposta 0)) (nth (- resposta 1) problema))
            (t (progn (format t "Problema invalido. ~%") (escolher-nivel problema)))
      )
    )
  )
)

;; no caso de ser escolhido o algoritmo de procura A*,
;; o utilizador tem de escolher a heuristica a usar
(defun escolher-euristica()
  (format t "~%1 - Euristica dada h(x) = objetivo_caixas - caixas_fechadas ~%")
  (format t "2 - Euristica propria h(x) = (objetivo_caixas - caixas_fechadas) * (|numero_arcos_horizontais_colocados + numero_arcos_verticais_nao_colocados| + 1) ~%")
  (format t "3 - Euristica propria v2 h(x) = (objetivo_caixas - caixas_fechadas) * (numero_arcos_horizontais_nao_colocados + numero_arcos_verticais_nao_colocados) ~%")
  (format t "Que heuristica quer usar? -> ")
  (let ((resposta (read)))
    (cond ((= resposta 1) 'calcular-heuristica-dada)
          ((= resposta 2) 'calcular-heuristica-propria)
          ((= resposta 3) 'calcular-heuristica-propria-v2)
          (t (progn (format t "Heuristica invalida. ~%") (escolher-euristica)))
    )
  )
)

(defun print-infos-nivel (nivel)
  (format t "~%Representacao do nivel escolhido: ~a ~%" (first nivel))
  (format t "Caixas a fechar do nivel escolhido: ~a ~%~%" (second nivel))
  (format t "Representacao grafica do nivel escolhido: ~%")
  (print-table (first nivel))
)


;; trata da interacao com o utilizador para escolher o algoritmo de procura
(defun ler-algoritmo ()
"Permite fazer a leitura do algoritmo a utilizar."
  (progn
    (format t "1 - Procura na largura ~%")
    (format t "2 - Procura na profundidade ~%")
    (format t "3 - Procura A* ~%")
    (format t "Que algoritmo de procura quer utilizar? -> ")
    (let ((resposta (read)))
      (cond ((= resposta 1) 'bfs)
            ((= resposta 2) 'dfs)
            ((= resposta 3) 'astar)
            (t (progn (format t "Algoritmo invalido. ~%") (ler-algoritmo)))
      )
    )
  )
)

;; no caso de ser escolhido o algoritmo de procura na profundidade,
;; o utilizador tem de escolher a profundidade limite
(defun ler-profundidade()
"Permite fazer a leitura da profundidade limite para o algoritmo dfs."
  (progn
    (format t "Qual a profundidade limite? -> ")
    (read)
  )
)

;; apresenta o resultado da procura no tabuleiro
(defun apresentar-progresso-estados (result &optional (index 1))
	(cond ((null result) nil)
		(T (progn
			(terpri)
      (format t "~a estado ~%" index)
      (format t "No estado: ~a ~%" (car result))
			(print-table (car result))
			(apresentar-progresso-estados (cdr result) (+ index 1))
		))
	)
)

;; transforma o resultado da procura numa lista de tabuleiros
(defun transform-result-to-list (result)
  (cond ((null result) nil)
        (T (cons (first result) (transform-result-to-list (no-pai result))))
  )
)


(defun escolher-se-quer-output-file ()
  (format t "1 - Sim ~%")
  (format t "2 - Nao ~%")
  (format t "Deseja guardar o resultado num ficheiro? -> ")
  (let ((resposta (read)))
    (cond ((= resposta 1) T)
          ((= resposta 2) nil)
          (t (progn (format t "Opcao invalida. ~%") (escolher-se-quer-output-file)))
    )
  )
)

(defun guardar-resultado-em-ficheiro (path nivel algoritmo profundidade heuristica resultado endtime)
  (with-open-file (output path :direction :output :if-exists :append :if-does-not-exist :create)
    (format output "====================================================================================== ~%" )
    (format output "Representacao do nivel escolhido: ~a ~%" (first nivel))
    (format output "Caixas a fechar do nivel escolhido: ~a ~%" (second nivel))
    (format output "Algoritmo utilizado: ~a ~%" algoritmo)
    (cond 
      ((equal algoritmo 'dfs) (format output "Profundidade limite: ~a ~%" profundidade))
      ((equal algoritmo 'astar)(format output "Heuristica utilizada: ~a ~%" heuristica))
    )
    (format output "Tempo de execucao: ~as ~%" endtime)
    (cond 
      ((null (first resultado)) (format output "Solucao nao encontrada! ~%~%~%~%"))
      (t 
        (format output "No solucao ~a ~%" (first resultado))
        (format output "Numero de nos gerados: ~a ~%" (first (second resultado)))
        (format output "Numero de nos expandidos: ~a ~%" (second (second resultado)))
        (format output "Profundidade da solucao: ~a ~%" (no-profundidade (first resultado)))
        (format output "Penetrancia: ~a ~%" (penetrancia (no-profundidade (first resultado)) (first (second resultado))))
        (format output "Fator Ramificacao Media: ~a ~%" (branching-factor (no-profundidade (first resultado)) (first (second resultado))))
      )
    )
    (format output "====================================================================================== ~%~%~%~%" )
  )
)