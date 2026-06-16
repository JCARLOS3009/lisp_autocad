(defun c:ECCSV ( / ss i ent txt strVal pos outputList finalStr)
  (setq ss (ssget '((0 . "TEXT,MTEXT,DIMENSION"))))
  (if ss
    (progn
      (textscr) ;; Abre a janela de texto do AutoCAD (F2)
      (princ "\n--- INÍCIO DOS DADOS (SEPARADO POR +) ---")
      
      (setq i 0)
      (setq outputList nil) ;; Lista para armazenar os valores temporariamente
      
      (while (< i (sslength ss))
        (setq ent (entget (ssname ss i)))
        (if (= (cdr (assoc 0 ent)) "DIMENSION")
          (setq txt (cdr (assoc 42 ent))) ;; Valor numérico real da cota
          (setq txt (cdr (assoc 1 ent)))  ;; Texto ou MText
        )
        
        ;; Se for número (cota), converte para texto com 2 casas decimais
        (if (numberp txt) 
          (setq strVal (rtos txt 2 2))
          (setq strVal txt)
        )
        
        ;; Adiciona o zero à esquerda se o número começar direto com ponto (ex: .50 -> 0.50)
        (if (= (substr strVal 1 1) ".")
          (setq strVal (strcat "0" strVal))
        )
        
        ;; Substitui ponto por vírgula para o sistema decimal brasileiro
        (while (setq pos (vl-string-search "." strVal))
          (setq strVal (vl-string-subst "," "." strVal pos))
        )
        
        ;; Adiciona o zero à esquerda se o texto começar direto com vírgula (ex: ,50 -> 0,50)
        (if (= (substr strVal 1 1) ",")
          (setq strVal (strcat "0" strVal))
        )
        
        ;; Adiciona o valor formatado à lista de resultados
        (setq outputList (cons strVal outputList))
        
        (setq i (1+ i))
      )
      
      ;; Inverte a lista para manter a ordem de seleção original do AutoCAD
      (setq outputList (reverse outputList))
      
      ;; Cria a string final juntando os itens com o sinal de mais (+)
      (setq finalStr (car outputList))
      (setq i 1)
      (while (< i (length outputList))
        (setq finalStr (strcat finalStr "+" (nth i outputList)))
        (setq i (1+ i))
      )
      
      ;; Imprime todos os dados em uma única linha no console
      (princ (strcat "\n" finalStr))
      (princ "\n--- FIM DOS DADOS ---\n")
    )
    (princ "\nNenhum objeto selecionado.")
  )
  (princ)
)
