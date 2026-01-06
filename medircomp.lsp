(defun c:SOMARGRUPO (/ ent ename dados soma lista_objs tipo vla_obj comp)
  ;; Carrega as funções extendidas do AutoLISP
  (vl-load-com)

  ;; Solicita a seleção de um objeto
  (setq ent (entsel "\nSelecione um objeto que pertença ao grupo para somar: "))

  (if ent
    (progn
      (setq ename (car ent))
      (setq dados (entget ename))
      (setq soma 0.0)

      ;; Procura o código DXF 330, que aponta para o dicionário do grupo
      (setq lista_objs (vl-remove-if-not '(lambda (x) (= (car x) 330)) dados))

      (if (null lista_objs)
        (princ "\nErro: O objeto selecionado não faz parte de nenhum grupo.")
        (progn
          ;; Varre os dicionários associados
          (foreach d lista_objs
            (setq grupo_dados (entget (cdr d)))
            
            ;; O código 340 no dicionário de grupos lista todos os membros do grupo
            (foreach item grupo_dados
              (if (= (car item) 340)
                (progn
                  (setq obj_ename (cdr item))
                  (setq tipo (cdr (assoc 0 (entget obj_ename))))
                  (setq vla_obj (vlax-ename->vla-object obj_ename))

                  ;; Tenta medir o comprimento se for um objeto curvo (Linha, Polilinha, Arco, Círculo)
                  (if (not (vl-catch-all-error-p 
                        (setq comp (vl-catch-all-apply 'vlax-curve-getDistAtParam 
                            (list vla_obj (vl-catch-all-apply 'vlax-curve-getEndParam (list vla_obj)))))))
                    (setq soma (+ soma comp))
                  )
                )
              )
            )
          )

          ;; Exibe o resultado formatado
          (princ "\n------------------------------------------")
          (princ (strcat "\nSOMA TOTAL DO COMPRIMENTO DO GRUPO: " (rtos soma 2 4)))
          (princ "\n------------------------------------------")
        )
      )
    )
    (princ "\nCancelado: Nenhum objeto selecionado.")
  )
  (princ)
)

(princ "\nComando [ SOMARGRUPO ] carregado.")
(princ)
