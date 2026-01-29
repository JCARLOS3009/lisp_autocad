(defun c:PAREDE_FINAL (/ sel_par h_parede sel_vão total_len total_vão_area total_vão_len i ent obj minpt maxpt p1 p2 dx dy v_len h_vão)
  (vl-load-com)
  (setq h_vão 0.50) ;; Altura padrão das janelas definida como 0.50m

  (princ "\n1. Selecione as linhas/polilinhas das PAREDES: ")
  (if (setq sel_par (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC"))))
    (progn
      (setq h_parede (getdist "\nDigite a altura da parede (pé-direito): "))
      (setq total_len 0.0)
      (setq i 0)
      (repeat (sslength sel_par)
        (setq ent (ssname sel_par i))
        (setq obj (vlax-ename->vla-object ent))
        (setq total_len (+ total_len (vla-get-length obj)))
        (setq i (1+ i))
      )

      (princ (strcat "\n2. Selecione as aberturas (considerando altura padrão de " (rtos h_vão 2 2) "m): "))
      (setq total_vão_area 0.0)
      (setq total_vão_len 0.0)
      
      (if (setq sel_vão (ssget '((0 . "LWPOLYLINE,POLYLINE,RECTANGLE"))))
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
            
            ;; Identifica o comprimento do vão (maior dimensão entre X e Y)
            (if (> dx dy) (setq v_len dx) (setq v_len dy))
            
            (setq total_vão_len (+ total_vão_len v_len))
            (setq total_vão_area (+ total_vão_area (* v_len h_vão)))
            (setq i (1+ i))
          )
        )
      )

      (setq m2_final (- (* total_len h_parede) total_vão_area))
      
      (alert (strcat "RELATÓRIO DE QUANTITATIVOS:"
                     "\n------------------------------------"
                     "\nPAREDE:"
                     "\nComprimento: " (rtos total_len 2 2) " m"
                     "\nAltura: " (rtos h_parede 2 2) " m"
                     "\n------------------------------------"
                     "\nABERTURAS (Janelas):"
                     "\nComprimento Total Vãos: " (rtos total_vão_len 2 2) " m"
                     "\nAltura Padrão Aplicada: " (rtos h_vão 2 2) " m"
                     "\nÁrea Descontada: " (rtos total_vão_area 2 2) " m²"
                     "\n------------------------------------"
                     "\nÁREA LÍQUIDA FINAL: " (rtos m2_final 2 2) " m²"))
    )
    (princ "\nErro: Nenhuma parede selecionada.")
  )
  (princ)
)
