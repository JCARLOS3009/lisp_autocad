(defun c:PAREDE_DETALHADA (/ sel_par altura sel_vão total_len total_vão_area total_vão_len i ent obj minpt maxpt p1 p2 dx dy)
  (vl-load-com)
  
  ;; 1. Seleção das Paredes
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

      ;; 2. Seleção das Aberturas
      (princ "\n2. Selecione os retângulos/polilinhas das ABERTURAS: ")
      (setq total_vão_area 0.0)
      (setq total_vão_len 0.0)
      (if (setq sel_vão (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
        (progn
          (setq i 0)
          (repeat (sslength sel_vão)
            (setq ent (ssname sel_vão i))
            (setq obj (vlax-ename->vla-object ent))
            
            ;; Extrai dimensões do vão (Bounding Box)
            (vla-getboundingbox obj 'minpt 'maxpt)
            (setq p1 (vlax-safearray->list minpt)
                  p2 (vlax-safearray->list maxpt)
                  dx (abs (- (car p2) (car p1))) ;; Comprimento do vão
                  dy (abs (- (cadr p2) (cadr p1))) ;; Altura do vão
            )
            
            (setq total_vão_len (+ total_vão_len dx))
            (setq total_vão_area (+ total_vão_area (* dx dy)))
            (setq i (1+ i))
          )
        )
      )

      ;; 3. Relatório Detalhado
      (setq m2_final (- (* total_len altura) total_vão_area))
      (alert (strcat "RELATÓRIO DETALHADO:"
                     "\n------------------------------------"
                     "\nPAREDE:"
                     "\nComprimento Total: " (rtos total_len 2 2) " m"
                     "\nAltura (Pé-direito): " (rtos altura 2 2) " m"
                     "\n------------------------------------"
                     "\nVÃOS (ABERTURAS):"
                     "\nSoma Comprimento Vãos: " (rtos total_vão_len 2 2) " m"
                     "\nÁrea Total de Vãos: " (rtos total_vão_area 2 2) " m²"
                     "\n------------------------------------"
                     "\nRESULTADO LÍQUIDO: " (rtos m2_final 2 2) " m²"))
    )
    (princ "\nNenhuma parede selecionada.")
  )
  (princ)
)
