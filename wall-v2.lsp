
(defun c:WR1 (/ sel_par h_parede_total sel_vão total_m2_parede total_vão_area i ent obj lay_name h_vão v_len dx dy minpt maxpt p1 p2 
               acadObj doc space tableObj insPt numVãos numParedes vãosList paredesList row h_temp p_len p_eixo)
  (vl-load-com)
  
  ;; Função para extrair altura do nome da layer (ex: PAREDE_H2.80)
  (defun extrair_valor (nome / pos val)
    (setq pos (vl-string-search "_" nome))
    (if pos (setq val (distof (substr nome (+ pos 2)))) nil)
    val
  )

  (princ "\n[1/2] Selecione as PAREDES (O comprimento será dividido por 2): ")
  (if (setq sel_par (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC"))))
    (progn
      (setq total_m2_parede 0.0 i 0 paredesList nil)
      (repeat (sslength sel_par)
        (setq ent (ssname sel_par i)
              obj (vlax-ename->vla-object ent)
              lay_name (vla-get-layer obj)
              h_temp (extrair_valor lay_name))
        
        (if (not h_temp) (setq h_temp 3.35)) ;; Altura padrão se não houver na layer
        
        ;; Pega o comprimento total e divide por 2 para obter o eixo
        (setq p_len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))
              p_eixo (/ p_len 2.0)
              total_m2_parede (+ total_m2_parede (* p_eixo h_temp))
              paredesList (cons (list (strcat "Parede " (itoa (1+ i))) (rtos p_eixo 2 2) (rtos h_temp 2 2) (rtos (* p_eixo h_temp) 2 2)) paredesList))
        (setq i (1+ i))
      )

      (princ "\n[2/2] Selecione os VÃOS: ")
      (setq total_vão_area 0.0 i 0 vãosList nil)
      (if (setq sel_vão (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
        (repeat (sslength sel_vão)
          (setq ent (ssname sel_vão i)
                obj (vlax-ename->vla-object ent)
                h_vão (extrair_valor (vla-get-layer obj)))
          (if (not h_vão) (setq h_vão 0.0))
          (vla-getboundingbox obj 'minpt 'maxpt)
          (setq p1 (vlax-safearray->list minpt) p2 (vlax-safearray->list maxpt)
                dx (abs (- (car p2) (car p1))) dy (abs (- (cadr p2) (cadr p1)))
                v_len (if (> dx dy) dx dy))
          (setq total_vão_area (+ total_vão_area (* v_len h_vão))
                vãosList (cons (list (strcat "Vão " (itoa (1+ i))) (rtos v_len 2 2) (rtos h_vão 2 2) (rtos (* v_len h_vão) 2 2)) vãosList))
          (setq i (1+ i))
        )
      )

      (setq m2_final (- total_m2_parede total_vão_area)
            numParedes (length paredesList)
            numVãos (length vãosList))

      (if (setq insPt (getpoint "\nPonto de inserção da tabela: "))
        (progn
          (setq acadObj (vlax-get-acad-object)
                doc (vla-get-activedocument acadObj)
                space (if (= (getvar "CVPORT") 1) (vla-get-paperspace doc) (vla-get-modelspace doc))
                ;; Tabela: Cabeçalho + Títulos + Dados + 3 linhas de fechamento
                tableObj (vla-addtable space (vlax-3d-point insPt) (+ numParedes numVãos 5) 4 1.5 35))
          
          (vla-settext tableObj 0 0 "MEMORIAL_ALVENARIA (EIXO)")
          (vla-settext tableObj 1 0 "DESCRICAO") (vla-settext tableObj 1 1 "COMP_EIXO_M") 
          (vla-settext tableObj 1 2 "ALT_M") (vla-settext tableObj 1 3 "AREA_M2")

          (setq row 2)
          (foreach p (reverse paredesList)
            (vla-settext tableObj row 0 (nth 0 p)) (vla-settext tableObj row 1 (nth 1 p))
            (vla-settext tableObj row 2 (nth 2 p)) (vla-settext tableObj row 3 (nth 3 p))
            (setq row (1+ row))
          )
          (vla-settext tableObj row 0 "SUBTOTAL_PAREDES") (vla-settext tableObj row 3 (rtos total_m2_parede 2 2))
          (setq row (1+ row))

          (foreach v (reverse vãosList)
            (vla-settext tableObj row 0 (nth 0 v)) (vla-settext tableObj row 1 (nth 1 v))
            (vla-settext tableObj row 2 (nth 2 v)) (vla-settext tableObj row 3 (strcat "-" (nth 3 v)))
            (setq row (1+ row))
          )
          (vla-settext tableObj row 0 "SUBTOTAL_VAOS") (vla-settext tableObj row 3 (rtos total_vão_area 2 2))
          (setq row (1+ row))

          (vla-settext tableObj row 0 "TOTAL_LIQUIDO") (vla-settext tableObj row 3 (rtos m2_final 2 2))
          
          (princ "\nTabela gerada com sucesso.")
        )
      )
    )
  )
  (princ)
)
