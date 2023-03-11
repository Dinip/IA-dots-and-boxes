;; algoritmo alfabeta, que chama a funcao alfabeta-max ou alfabeta-min
;; conforme a profunidade do no
(defun alfabeta (no &optional (max-depth 2) (jogador 1) (max-time 5000) (alfa most-negative-fixnum) (beta most-positive-fixnum) (initial-time (get-internal-real-time)))
  (cond (
          (OR (tabuleiro-preenchidop no) 
		          (= (no-profundidade no) max-depth)
              (>= (get-internal-real-time) (+ initial-time (* max-time 0.9)))
              (null no)
          )
          (progn 
            (incf *analised-nodes*)
            (avaliar no)
          )
        )
        (t
          (let ((nos-sucessores (remove-duplicates (sucessores no jogador) :test #'equal)))
            (incf *expanded-nodes* (length nos-sucessores))
            (cond ((= (mod (no-profundidade no) 2) 0) (alfabeta-max (sort nos-sucessores #'> :key #'avaliar) max-depth jogador max-time alfa beta initial-time))
                   (t (alfabeta-min (sort nos-sucessores #'< :key #'avaliar) max-depth jogador max-time alfa beta initial-time))
            )
          )
        )
  )
)

;; aplica o alfabeta max
(defun alfabeta-max (sucessores max-depth jogador max-time alfa-atual beta-atual initial-time)
  (cond ((null sucessores) alfa-atual)
        ((>= alfa-atual beta-atual) 
          (progn 
            (incf *cortes-alfa*) 
            beta-atual
          )
        )
        (t (let ((alfa-novo (max alfa-atual (alfabeta (car sucessores) max-depth (trocar-jogador jogador) max-time alfa-atual beta-atual initial-time))))
            (if (= (no-profundidade (car sucessores)) 1)
              (guardar-solucao (car sucessores) alfa-novo)
            )
            (alfabeta-max (cdr sucessores) max-depth max-time jogador alfa-novo beta-atual initial-time)
          )
        )
  )
)

;; aplicacao do alfabeta min
(defun alfabeta-min (sucessores max-depth jogador max-time alfa-atual beta-atual initial-time)
  (cond ((null sucessores) beta-atual)
        ((<= beta-atual alfa-atual) 
          (progn 
            (incf *cortes-beta*) 
            alfa-atual
          )
        ) 
        (t (let ((beta-novo (min beta-atual (alfabeta (car sucessores) max-depth (trocar-jogador jogador) max-time alfa-atual beta-atual initial-time))))
            (alfabeta-min (cdr sucessores) max-depth jogador max-time alfa-atual beta-novo initial-time)
          )
        )
  )
)