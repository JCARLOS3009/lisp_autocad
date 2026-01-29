(defun c:PAREDE (/ sel altura total_len i ent obj)
  (vl-load-com) 
  (princ "\nSelecione as linhas soltas ou polilinhas das paredes: ")
  
  (if (setq sel (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC"))))
    (progn
      (setq altura (getdist "\nDigite a altura da parede: "))
      (setq total_len 0.0)
      (setq i 0)
      
      (repeat (sslength sel)
        (setq ent (ssname sel i))
        (setq obj (vlax-ename->vla-object ent))
        ;; O comando vla-get-length lê o comprimento real de LINES e Polylines
        (setq total_len (+ total_len (vla-get-length obj)))
        (setq i (1+ i))
      )
      
      (alert (strcat "RESULTADO:\n"
                     "Comprimento: " (rtos total_len 2 2) "m\n"
                     "Altura: " (rtos altura 2 2) "m\n"
                     "Área Total: " (rtos (* total_len altura) 2 2) "m2"))
    )
    (princ "\nErro: Selecione apenas linhas ou polilinhas.")
  )
  (princ)
)
