(defun c:LGROUPS (/ dict i grupo nome lista quantidade)
  (setq dict (dictsearch (namedobjdict) "ACAD_GROUP"))
  (if (not dict)
    (princ "\nNenhum grupo encontrado no desenho.")
    (progn
      (princ "\n------------------------------------------")
      (princ (strcat "\n" (setq h1 "NOME DO GRUPO") (substr "                    " (1+ (strlen h1))) " | QTD ELEMENTOS"))
      (princ "\n------------------------------------------")
      
      (setq i 0)
      (foreach item dict
        (if (= (car item) 3) (setq nome (cdr item))) ; Pega o nome do grupo
        (if (= (car item) 350) ; Pega a entidade do grupo
          (progn
            (setq grupo (entget (cdr item)))
            (setq quantidade 0)
            ; Conta quantos códigos 340 (objetos) existem no grupo
            (foreach g grupo
              (if (= (car g) 340) (setq quantidade (1+ quantidade)))
            )
            ; Formata a saída visual
            (princ (strcat "\n" nome (substr "                    " (1+ (strlen nome))) " | " (itoa quantidade)))
            (setq i (1+ i))
          )
        )
      )
      (princ "\n------------------------------------------")
      (princ (strcat "\nTotal de grupos encontrados: " (itoa i)))
    )
  )
  (princ "\nPressione F2 para ver a lista completa.")
  (princ)
)

(princ "\nComando carregado. Digite LISTARGRUPOS para executar.")
(princ)
