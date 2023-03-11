(defvar *jogador1* 1)
(defvar *jogador2* 2)
(defvar *initial-player* 1)
(defvar *best-alfa-value* most-negative-fixnum)
(defvar *estado* nil)
(defvar *cortes-alfa* 0)
(defvar *cortes-beta* 0)
(defvar *expanded-nodes* 0)
(defvar *analised-nodes* 0)
(defvar *max-time* 5000)
(defvar *first-play* nil)

;; caminho para o ficheiro log.dat com estatisticas (DEVE SER ALTERADO)
(defun ficheiro-solucao ()
  (let ((path "D:\\Clouds\\OneDriveIPS\\LEI\\IA\\projeto_part2\\log.dat"))
    path
  )
)

;; carregar os ficheiros necessarios para o funcionamento do programa
;; implica que os ficheiros estejam na mesma pasta que o ficheiro jogo.lisp

(load (compile-file (current-pathname "puzzle.lisp")))
(load (compile-file (current-pathname "algoritmo.lisp")))

;; no inicial, com o estado inicial a zero e 0 caixas fechadas
(defun no-inicial ()
  (list (list (tabuleiro-inicial) (list 0 0)) 0 (list 0 0))
)

;; estado inicial de jogo, tudo a 0
(defun tabuleiro-inicial ()
  '(
    ((0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0)(0 0 0 0 0 0))
    ((0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0)(0 0 0 0 0))
  )
)

;; imprime o tabuleiro no ecra
(defun print-table (table)
  (let ((nlines (length (first table))) (ncolumns (length (second table))))
    (loop for lin from 0 to (- nlines 1)
      do (loop for col from 0 to (- ncolumns 2)
        do
          (format t "+")
          (cond ((not (= (nth col (nth lin (first table))) 0)) (format t "--"))
                ((= (nth col (nth lin (first table))) 0) (format t "  "))
          )
      )
      (format t "+")
      (terpri)
      (cond ((< lin (- nlines 1))
            (loop for col2 from 0 to (- ncolumns 1)
              do
                (cond ((not (= (nth lin (nth col2 (second table))) 0)) (format T "|"))
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

;; reseta as variaveis globais utilizadas em cada iteração
;; do computador, para os seus valores originais
(defun reset-values ()
  (setq *best-alfa-value* most-negative-fixnum)
  (setq *estado* nil)
  (setq *cortes-alfa* 0)
  (setq *cortes-beta* 0)
  (setq *expanded-nodes* 0)
  (setq *analised-nodes* 0)
)

;; funcao para jogar computador contra computador
(defun pc-vs-pc (no &optional (jogador *jogador1*))
  (cond ((tabuleiro-preenchidop no) (print-table (no-tabuleiro no)) (winner no))
        (t (print-table (no-tabuleiro no))
           (reset-values)
           (setq *initial-player* jogador)
           (let ((start-time (get-internal-real-time)))
            (alfabeta no 4 jogador *max-time* most-negative-fixnum most-positive-fixnum start-time) ;; reminder: dentro do alfabeta o estado é alterado
            (print-stats (- (get-internal-real-time) start-time) jogador)
            (guardar-resultado-em-ficheiro (- (get-internal-real-time) start-time) jogador)
            (setq *first-play* nil)
           )
           (pc-vs-pc (cria-no (no-estado *estado*) 0 (no-caixas-fechadas *estado*)) (trocar-jogador jogador))
        )
  ) 
)

;; funcao para tratar da interacao com o user para poder jogar
;; colocando arcos e caso feche uma caixa nessa jogada, podera
;; voltar a jogar
(defun user-vs-pc (no &optional (jogador *jogador1*))
  (cond ((tabuleiro-preenchidop no) (print-table (no-tabuleiro no)) (winner no))
        (t (print-table (no-tabuleiro no))
           (cond ((equal jogador *jogador1*)
                  (let (
                        (linha nil)
                        (coluna nil)
                        (direcao nil)
                        (novo-estado nil)
                        (caixas-antes (apply #'+ (no-caixas-fechadas no)))
                      )
                      (format t "Direction (v/h): ")
                      (setq direcao (read-line))
                      (cond ((equal direcao "v") 
                              (format t "Column: ")
                              (setq coluna (read-line))
                              (format t "Line: ")
                              (setq linha (read-line))
                              (setq novo-estado (arco-vertical (parse-integer coluna) (parse-integer linha) (no-tabuleiro no)))
                            )
                            ((equal direcao "h") 
                              (format t "Line: ")
                              (setq linha (read-line))
                              (format t "Column: ")
                              (setq coluna (read-line))
                              (setq novo-estado (arco-horizontal (parse-integer linha) (parse-integer coluna) (no-tabuleiro no)))
                            )
                            (t (format t "Invalid direction, try again...")
                              (user-vs-pc no jogador)
                            )
                      )
                      (cond ((null novo-estado)
                              (format t "Invalid move or already filled, try again...")
                              (user-vs-pc no jogador)
                            )
                            (t (setq no (cria-no (criar-estado novo-estado (incrementar-caixas-player jogador (- (closed-boxes novo-estado) caixas-antes) (no-caixas-fechadas no))) 0 (no-caixas-fechadas no))))
                      )
                      (cond ((> (closed-boxes novo-estado) caixas-antes) 
                              (format t "You closed a box, please play again ~%~%")
                              (user-vs-pc no jogador)
                            )
                            (t (user-vs-pc no (trocar-jogador jogador)))                        
                      )
                  )
                 )    
                 (t
                  (setq *best-alfa-value* most-negative-fixnum)
                  (setq *estado* nil)
                  (setq *initial-player* jogador)

                  (let ((start-time (get-internal-real-time)))
                    (alfabeta no 4 jogador *max-time* most-negative-fixnum most-positive-fixnum) ;; reminder: dentro do alfabeta o estado é alterado
                    (print-stats (- (get-internal-real-time) start-time) jogador)
                    (guardar-resultado-em-ficheiro (- (get-internal-real-time) start-time) jogador)
                    (setq *first-play* nil)
                  )
                  (user-vs-pc (cria-no (no-estado *estado*) 0 (no-caixas-fechadas *estado*)) (trocar-jogador jogador))
                 )
          )
        )
  )
)

;; recebe um no e indica quem foi o jogador vencedor, analisando
;; o numero de caixas que cada um fechou
(defun winner (no)
  (cond ((> (caixas-fechadas-jogador *jogador1* no) (caixas-fechadas-jogador *jogador2* no)) (format t "Player ~a won" *jogador1*))
        ((= (caixas-fechadas-jogador *jogador1* no) (caixas-fechadas-jogador *jogador2* no)) (format t "Draw..."))
        (t (format t "Player ~a won" *jogador2*))
  )
)

(defun iniciar ()
  (start-menu)
)


;; apresentacao e tratamento da escolha do jogador no menu principal
;; a aplicacao, onde permite escolher se quer jogar ou alterar definicoes
;; e podera ver as definicoes atuais
(defun start-menu ()
  (loop
    (format t "~%  ______________________________________________________ ")
    (format t "~% |                                                      |")
    (format t "~% |                    DOTS AND BOXES                    |")
    (format t "~% |                                                      |")
    (format t "~% |                     1 - Play                         |")
    (format t "~% |                     2 - Settings                     |")
    (format t "~% |                     0 - Quit                         |")
    (format t "~% |                                                      |")
    (format t "~% |                    Settings Value                    |")
    (format t "~% |                   Initial Player: ~a                  |" *initial-player*) ;;nao mexer
    (format t "~% |                   Max Time: ~a ms                  |" *max-time*) ;;nao mexer
    (format t "~% |______________________________________________________|")
    (format t "~%~% Choose an option => ")
    (let ((choice (read)))
      (cond ((and (numberp choice) (> choice -1) (< choice 3))
              (cond ((= choice 1) (progn (game-modes-menu) t))
                    ((= choice 2) (progn (settings-menu) t))
                    ((= choice 0) (progn (format t "~%         Bye Bye") (return)))
              )
            )
            (t (progn 
                (format t "~%         Invalid Choice~%~%         Choose an option => ")
                (setf choice (read))
              )
            )
      )
    )
  )
)

;; apresentacao e tratamento da escolha do jogador no menu de
;; novo de jogo (pc-vs-pc ou user-vs-pc)
(defun game-modes-menu ()
  (loop
    (format t "~%  ______________________________________________________ ")
    (format t "~% |                                                      |")
    (format t "~% |                      GAME MODES                      |")
    (format t "~% |                                                      |")
    (format t "~% |                   1 - Human vs PC                    |")
    (format t "~% |                   2 - PC vs PC                       |")
    (format t "~% |                   0 - Home                           |")
    (format t "~% |                                                      |")
    (format t "~% |                       DEFAULT                        |")
    (format t "~% |                   Initial Player: ~a                  |" *initial-player*) ;;nao mexer
    (format t "~% |                   Max Time: ~a ms                  |" *max-time*) ;;nao mexer
    (format t "~% |______________________________________________________|")
    (format t "~%~% Choose an option => ")

    (let ((choice (read)) (initial-player *initial-player*))
      (cond ((and (numberp choice) (> choice -1) (< choice 3))
              (cond ((= choice 1) (progn (setq *first-play* t) (user-vs-pc (no-inicial) *initial-player*) (setq *initial-player* initial-player)))
                    ((= choice 2) (progn (setq *first-play* t) (pc-vs-pc (no-inicial)) (setq *initial-player* initial-player)))
                    ((= choice 0) (return))
              )
            )
            (t (progn 
                (format t "~%         Invalid Choice~%~%         Choose an option => ")
                (setf choice (read))
              )
            )
      )
    )
  )
)

;; apresentacao e tratamento da escolha do jogador no menu de defincoes
;; onde podera ser definido o tempo de execucao maximo por parte do computador
;; e o jogador inicial (no caso de user-vs-pc)
(defun settings-menu ()
  (loop
    (format t "~%  ______________________________________________________ ")
    (format t "~% |                                                      |")
    (format t "~% |                       SETTINGS                       |")
    (format t "~% |                                                      |")
    (format t "~% |                 1 - Execution Time                   |")
    (format t "~% |                 2 - Initial Player                   |")
    (format t "~% |                 0 - Home                             |")
    (format t "~% |                                                      |")
    (format t "~% |______________________________________________________|") 
    (format t "~%~% Choose an option => ")

    (let ((choice (read)))
      (cond ((and (numberp choice) (> choice -1) (< choice 3))
              (cond ((= choice 1) (execution-time-menu))
                    ((= choice 2) (first-player-menu))
                    ((= choice 0)  nil)
              )
            )
            (t (progn 
                (format t "~%          Invalid Choice~%~%         Choose an option => ")
                (setf choice (read))
              )
            )
      )
      (return)
    )
  )
)

;; apresentacao e tratamento da escolha do jogador no menu definicao
;; de tempo de execucao maximo
(defun execution-time-menu ()
  (loop
    (format t "~%  ______________________________________________________ ")
    (format t "~% |                                                      |")
    (format t "~% |                    EXECUTION TIME                    |")
    (format t "~% |                                                      |")
    (format t "~% |                     [1000 - 5000]                    |")
    (format t "~% |                       0 - Home                       |")
    (format t "~% |                                                      |")
    (format t "~% |______________________________________________________|") 
    (format t "~%~% Choose an option => ")

    (let ((choice (read)))
      (cond ((and (numberp choice) (> choice 999) (< choice 5001)) (setf *max-time* choice))
            ((and (numberp choice) (= choice 0) (return)))
            (t (progn 
                (format t "~%          Invalid Choice~%~%         Choose an option => ")
                (setf choice (read))
              )
            )
      )
      (return)
    )
  )
)

;; apresentacao e tratamento da escolha do jogador no menu definicao
;; de primeiro jogador a jogar
(defun first-player-menu ()
  (loop
    (format t "~%  ______________________________________________________ ")
    (format t "~% |                                                      |")
    (format t "~% |                    Initial Player                    |")
    (format t "~% |                                                      |")
    (format t "~% |                     1 - (User)                       |")
    (format t "~% |                     2 - (PC)                         |")
    (format t "~% |                     0 - Home Menu                    |")
    (format t "~% |                                                      |")
    (format t "~% |______________________________________________________|") 
    (format t "~%~%  Choose an option => ")

    (let ((choice (read)))
      (cond ((and (numberp choice) (> choice -1) (< choice 3))
              (cond ((= choice 1) (setq *initial-player* 1))
                    ((= choice 2) (setq *initial-player* 2))
                    ((= choice 0) nil)
              )
            )
            (t (progn 
                (format t "~%          Invalid Choice~%~%         Choose an option => ")
                (setf choice (read))
              )
            )
      )
      (return)
    )
  )
)

;; a jogada realizada, o novo estado, o número de nós analisados
;; o número de cortes efetuados (de cada tipo) e o tempo gasto
(defun print-stats (time player)
  (format t "~%  ______________________________________________________ ")
  (format t "~% |                      STATS                           |")
  (format t "~% |                  Player: ~a                           |" player)
  (format t "~% |                  Analysed Nodes: ~5D               |" *analised-nodes*)
  (format t "~% |                  Expanded Nodes: ~6D              |" *expanded-nodes*)
  (format t "~% |                  Alfa Cuts: ~4D                     |" *cortes-alfa*)
  (format t "~% |                  Beta Cuts: ~4D                     |" *cortes-beta*)
  (format t "~% |                  Time: ~5D ms                      |" time)
  (format t "~% |______________________________________________________|")
  (format t "~%~%  Novo Estado:~a~%~%~%" (no-estado *estado*))
)

;; guarda (ou faz append) das estatisticas da jogada realizada num ficheiro de texto
(defun guardar-resultado-em-ficheiro (time player)
  (if *first-play*
    ;; caso seja a primeira vez que está a guardar o ficheiro, deve criar se nao existir e caso exista,
    ;; deve criar um novo
    (with-open-file (output (ficheiro-solucao) :direction :output :if-exists :rename-and-delete :if-does-not-exist :create)
      (format output "====================================================================================== ~%" )
      (format output "Player: ~a~%" player)
      (format output "Analysed Nodes: ~a~%" *analised-nodes*)
      (format output "Expanded Nodes: ~a~%" *expanded-nodes*)
      (format output "Alfa Cuts: ~a~%" *cortes-alfa*)
      (format output "Beta Cuts: ~a~%" *cortes-beta*)
      (format output "Time: ~ams ~%" time)
      (format output "Novo Estado: ~a~%" (no-estado *estado*))
      (format output "====================================================================================== ~%~%~%" )
    )
    (with-open-file (output (ficheiro-solucao) :direction :output :if-exists :append :if-does-not-exist :create)
      (format output "====================================================================================== ~%" )
      (format output "Player: ~a~%" player)
      (format output "Analysed Nodes: ~a~%" *analised-nodes*)
      (format output "Expanded Nodes: ~a~%" *expanded-nodes*)
      (format output "Alfa Cuts: ~a~%" *cortes-alfa*)
      (format output "Beta Cuts: ~a~%" *cortes-beta*)
      (format output "Time: ~ams ~%" time)
      (format output "Novo Estado: ~a~%" (no-estado *estado*))
      (format output "====================================================================================== ~%~%~%" )
    )
  )
)

