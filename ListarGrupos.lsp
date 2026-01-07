(defun c:LGRUPOS (/ dict i grupo nome quantidade soma item g obj_ename vla_obj comp h1 h2 h3)
  ;; Carrega funções VL para medição de curvas
  (vl-load-com)
  
  (setq dict (dictsearch (namedobjdict) "ACAD_GROUP"))
  
  (if (not dict)
    (princ "\nNenhum grupo encontrado no desenho.")
    (progn
      ;; Cabeçalho da Tabela
      (setq h1 "NOME DO GRUPO" h2 "QTD" h3 "COMPRIMENTO TOTAL")
      (princ "\n------------------------------------------------------------------")
      (princ (strcat "\n" h1 (substr "                    " (1+ (strlen h1))) 
                     " | " h2 "  | " h3))
      (princ "\n------------------------------------------------------------------")
      
      (setq i 0)
      (foreach item dict
        (if (= (car item) 3) (setq nome (cdr item))) ; Armazena o nome
        
        (if (= (car item) 350) ; Identifica a entidade do grupo
          (progn
            (setq grupo (entget (cdr item)))
            (setq quantidade 0)
            (setq soma 0.0)
            
            ;; Varre os objetos dentro do grupo (código 340)
            (foreach g grupo
              (if (= (car g) 340)
                (progn
                  (setq quantidade (1+ quantidade))
                  (setq obj_ename (cdr g))
                  (setq vla_obj (vlax-ename->vla-object obj_ename))
                  
                  ;; Tenta calcular o comprimento se for um objeto linear/curvo
                  (setq comp (vl-catch-all-apply 'vlax-curve-getDistAtParam 
                                (list vla_obj (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_obj)))))
                  
                  (if (not (vl-catch-all-error-p comp))
                    (setq soma (+ soma comp))
                  )
                )
              )
            )
            
            ;; Formatação da linha na tabela
            (princ (strcat "\n" nome (substr "                    " (1+ (strlen nome))) 
                           " | " (substr (strcat (itoa quantidade) "    ") 1 4)
                           " | " (rtos soma 2 2)))
            (setq i (1+ i))
          )
        )
      )
      (princ "\n------------------------------------------------------------------")
      (princ (strcat "\nTotal de grupos processados: " (itoa i)))
    )
  )
  (princ "\nPressione F2 para visualizar o relatório completo.")
  (princ)
)

(princ "\nComando [ LGRUPOS ] carregado com sucesso.")
(princ)
