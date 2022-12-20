;; Algoritmos
(defun bfs (no-inicial fn-solucao fn-sucessores operadores caixas-a-fechar &optional (abertos nil) (fechados nil))
  (cond ((and (null abertos) (null fechados)) (bfs no-inicial fn-solucao fn-sucessores operadores caixas-a-fechar (list no-inicial) fechados))
        ((null abertos) nil)
        (T
          (let* (
            ;;coloca o 1 de abertos em fechados
            (novos-fechados (append fechados (list (car abertos))))
            ;;gera sucessores, verifica se ja estao em fechados e caso estejam, nao os coloca na lista de sucessores unicos
            (sucessores-unicos (nos-unicos-bfs novos-fechados (funcall fn-sucessores (car abertos) operadores 'bfs)))
            ;;testa todos os nos sucessores para ver se algum é solução e coloca numa lista
            (no-objective-value (no-objetivo sucessores-unicos fn-solucao caixas-a-fechar)) 
            )
            ;; debug, deixado prepositadamente
            ; (format t "abertos: ~a~%" (length (cdr abertos)))
            ; (format t "fechados: ~a~%" (length novos-fechados))
            ; (format t "sucessores: ~a~%" (length sucessores-unicos))
            ; (format t "depth: ~a~%" (no-profundidade (car abertos)))
				    ; (terpri)
            ; (print-table (no-estado (car abertos)))
            ;;caso a lista anterior tenha algum elemento, devolve uma lista com o primeiro elemento que é solucao, o numero de nos gerados e o numero de nos expandidos
            (cond ((not (null no-objective-value)) (list (car no-objective-value) (list (- (+ (length (cdr abertos)) (length novos-fechados) (length sucessores-unicos)) 1) (length novos-fechados))))
                  (t (bfs no-inicial fn-solucao fn-sucessores operadores caixas-a-fechar (abertos-bfs (cdr abertos) sucessores-unicos) novos-fechados))
            )
          )
        )
  )
)

(defun dfs (no-inicial fn-solucao fn-sucessores operadores depth caixas-a-fechar &optional (abertos nil) (fechados nil))
  (cond ((and (null abertos) (null fechados)) (dfs no-inicial fn-solucao fn-sucessores operadores depth caixas-a-fechar (list no-inicial) fechados))
        ((null abertos) nil)
        (T 
          (let* (
            ;;coloca o 1 de abertos em fechados
            (novos-fechados (append fechados (list (car abertos))))
            ;;gera sucessores, verifica se ja estao em fechados ou abertos e caso estejam verifica tambem se a depth é menor.
            ;;se a depth for menor coloca na lista de sucessores tambem
            (sucessores-unicos (nos-unicos-dfs abertos novos-fechados (funcall fn-sucessores (car abertos) operadores 'dfs depth)))
            ;;testa todos os nos sucessores para ver se algum é solução e coloca numa lista
            (no-objective-value (no-objetivo sucessores-unicos fn-solucao caixas-a-fechar))
            )
            ;; debug, deixado prepositadamente
            ; (format t "abertos: ~a~%" (length (cdr abertos)))
            ; (format t "fechados: ~a~%" (length novos-fechados))
            ; (format t "sucessores: ~a~%" (length sucessores-unicos))
            ; (format t "depth: ~a~%" (no-profundidade (car abertos)))
					  ; (terpri)
            ; (print-table (no-estado (car abertos)))
            ;;caso a lista anterior tenha algum elemento, devolve uma lista com o primeiro elemento que é solucao, o numero de nos gerados e o numero de nos expandidos
            (cond ((not (null no-objective-value)) (list (car no-objective-value) (list (- (+ (length (cdr abertos)) (length novos-fechados) (length sucessores-unicos)) 1) (length novos-fechados))))
                  (t (dfs no-inicial fn-solucao fn-sucessores operadores depth caixas-a-fechar (abertos-dfs (cdr abertos) sucessores-unicos) (remove-sucessores-from-fechados novos-fechados sucessores-unicos)))
            )
          )
        )
  )
)

(defun astar (no-inicial fn-solucao fn-sucessores operadores caixas-a-fechar heuristica-a-usar &optional (abertos nil) (fechados nil))
  (cond ((and (null abertos) (null fechados)) (astar no-inicial fn-solucao fn-sucessores operadores caixas-a-fechar heuristica-a-usar (list no-inicial) fechados))
        ((null abertos) nil)
        ;;testar se é no soluçao logo aqui, antes de expandir ao contrario dos outros algoritmos
        ;;caso o no seja solucao, devolve uma lista com o primeiro elemento que é solucao, o numero de nos gerados e o numero de nos expandidos
        ((funcall fn-solucao (car abertos)) (list (car abertos) (list (- (+ (length (cdr abertos)) (length fechados)) 1) (length fechados))))
        (T 
          (let* (
            ;;coloca o 1 de abertos em fechados
            (novos-fechados (append fechados (list (car abertos))))
            ;;gera sucessores, verifica se ja estao em fechados ou abertos e caso estejam verifica tambem se o custo (f=g+h) é menor.
            ;;se o custo for menor coloca na lista de sucessores tambem ---- (o 0 aqui é so um placeholder para o depth)
            (sucessores-unicos (nos-unicos-astar abertos novos-fechados (funcall fn-sucessores (car abertos) operadores 'astar caixas-a-fechar heuristica-a-usar)))
            )
            ;; debug, deixado prepositadamente
            ; (format t "abertos: ~a~%" (length (cdr abertos)))
            ; (format t "fechados: ~a~%" (length novos-fechados))
            ; (format t "depth: ~a~%" (no-profundidade (car abertos)))
            ; (format t "heuristica: ~a~%" (no-heuristica (car abertos)))
            ; (print-table (no-estado (car abertos)))
					  ; (terpri)
            (astar no-inicial fn-solucao fn-sucessores operadores caixas-a-fechar heuristica-a-usar (colocar-sucessores-em-abertos (cdr abertos) sucessores-unicos) (remove-sucessores-from-fechados novos-fechados sucessores-unicos))
          )
        )
  )
)