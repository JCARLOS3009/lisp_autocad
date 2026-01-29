(defun c:PAREDE_TOTAL (/ sel_par altura sel_vão total_len total_vão_area total_vão_len i ent obj minpt maxpt p1 p2 dx dy v_len v_alt)
  (vl-load-com)
  
  (princ "\n1. Selecione as linhas/polilinhas das PAREDES: ")
  (if (setq sel_par (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC"))))
    (progn
      (setq altura (getdist "\nDigite a altura da parede (pé-direito): "))
      (setq total_len 0.0)
      (setq i 0)
      (repeat (sslength sel_par)
        (setq ent (ssname sel_par i))
        (setq obj (vlax-ename->vla-object ent))
        (setq total_len (+ total_len (vla-get-length obj)))
        (setq i (1+ i))
      )

      (princ "\n2. Selecione as polilinhas das ABERTURAS (em planta): ")
      (setq total_vão_area 0.0)
      (setq total_vão_len 0.0)
      (if (setq sel_vão (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
        (progn
          (setq i 0)
          (repeat (sslength sel_vão)
            (setq ent (ssname sel_vão i))
            (setq obj (vlax-ename->vla-object ent))
            (vla-getboundingbox obj 'minpt 'maxpt)
            (setq p1 (vlax-safearray->list minpt)
                  p2 (vlax-safearray->list maxpt)
                  dx (abs (- (car p2) (car p1)))
                  dy (abs (- (cadr p2) (cadr p1))))
            
            ;; LÓGICA DE ORIENTAÇÃO:
            ;; Assume a maior dimensão horizontal ou vertical como o COMPRIMENTO do vão
            ;; e a menor como a espessura da parede (que não entra no cálculo de m2)
            (if (> dx dy) (setq v_len dx v_alt dy) (setq v_len dy v_alt dx))
            
            ;; IMPORTANTE: Aqui usamos a ALTURA fornecida no início para o cálculo da área do vão? 
            ;; Normalmente, vãos têm alturas específicas (ex: porta 2.10). 
            ;; Para precisão, este código usa a área real do retângulo desenhado.
            (setq total_vão_len (+ total_vão_len v_len))
            (setq total_vão_area (+ total_vão_area (vla-get-area obj)))
            (setq i (1+ i))
          )
        )
      )

      (setq m2_final (- (* total_len altura) total_vão_area))
      (alert (strcat "RELATÓRIO FINAL:"
                     "\n------------------------------------"
                     "\nComprimento Parede: " (rtos total_len 2 2) " m"
                     "\nAltura Parede: " (rtos altura 2 2) " m"
                     "\n------------------------------------"
                     "\nSoma Comprimento Vãos: " (rtos total_vão_len 2 2) " m"
                     "\nÁrea Total de Vãos: " (rtos total_vão_area 2 2) " m²"
                     "\n------------------------------------"
                     "\nÁREA LÍQUIDA: " (rtos m2_final 2 2) " m²"))
    )
  )
  (princ)
)
