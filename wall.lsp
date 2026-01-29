(defun c:PAREDE_LAYERS (/ sel_par h_parede sel_vão total_len total_vão_area total_vão_len i ent obj lay_name h_vão v_len dx dy minpt maxpt p1 p2)
  (vl-load-com)
  
  ;; Função auxiliar para extrair altura do nome da Layer (ex: "VAO_1.20" -> 1.20)
  (defun extrair_altura (nome / pos val)
    (setq pos (vl-string-search "_" nome))
    (if pos
      (setq val (distof (substr nome (+ pos 2))))
      (setq val 0.50) ;; Altura padrão caso não encontre número na layer
    )
    (if (not val) (setq val 0.50) val)
  )

  (princ "\n1. Selecione as linhas/polilinhas das PAREDES: ")
  (if (setq sel_par (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC"))))
    (progn
      (setq h_parede (getdist "\nDigite a altura da parede (pé-direito): "))
      (setq total_len 0.0)
      (setq i 0)
      (repeat (sslength sel_par)
        (setq ent (ssname sel_par i))
        (setq total_len (+ total_len (vla-get-length (vlax-ename->vla-object ent))))
        (setq i (1+ i))
      )

      (princ "\n2. Selecione as aberturas (A altura será lida da LAYER): ")
      (setq total_vão_area 0.0)
      (setq total_vão_len 0.0)
      
      (if (setq sel_vão (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
        (progn
          (setq i 0)
          (repeat (sslength sel_vão)
            (setq ent (ssname sel_vão i))
            (setq obj (vlax-ename->vla-object ent))
            (setq lay_name (vla-get-layer obj))
            
            ;; Pega a altura específica desta abertura baseada na Layer
            (setq h_vão (extrair_altura lay_name))
            
            ;; Identifica comprimento (maior dimensão X ou Y)
            (vla-getboundingbox obj 'minpt 'maxpt)
            (setq p1 (vlax-safearray->list minpt)
                  p2 (vlax-safearray->list maxpt)
                  dx (abs (- (car p2) (car p1)))
                  dy (abs (- (cadr p2) (cadr p1))))
            (if (> dx dy) (setq v_len dx) (setq v_len dy))
            
            (setq total_vão_len (+ total_vão_len v_len))
            (setq total_vão_area (+ total_vão_area (* v_len h_vão)))
            (setq i (1+ i))
            (princ (strcat "\nObjeto na layer [" lay_name "] lido com H=" (rtos h_vão 2 2)))
          )
        )
      )

      (setq m2_final (- (* total_len h_parede) total_vão_area))
      
      (alert (strcat "RELATÓRIO AUTOMATIZADO POR LAYER:"
                     "\n------------------------------------"
                     "\nComprimento Total Paredes: " (rtos total_len 2 2) " m"
                     "\nÁrea Bruta: " (rtos (* total_len h_parede) 2 2) " m²"
                     "\n------------------------------------"
                     "\nComprimento Total Vãos: " (rtos total_vão_len 2 2) " m"
                     "\nDesconto Total Aberturas: " (rtos total_vão_area 2 2) " m²"
                     "\n------------------------------------"
                     "\nÁREA LÍQUIDA FINAL: " (rtos m2_final 2 2) " m²"))
    )
  )
  (princ)
)
