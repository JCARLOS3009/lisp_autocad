;;; ============================================================
;;;  NomearParede.lsp  v8.0
;;;  Paredes : MLINE  (layer ex: parede_3.35 -> altura 3.35m)
;;;  Vaos    : LWPOLYLINE/POLYLINE ou dentro de blocos/grupos
;;;            (layer ex: VAO_0.90 -> altura 0.90m)
;;;  Largura do vao = maior lado do retangulo desenhado
;;;  Tabela  : PAREDE | COMP.BRUTO | VAOS | COMP.LIQ | ALTURA | AREA(m2)
;;;  Ordem   : cima->baixo / esquerda->direita
;;;  Comando : NOMEARPAREDE
;;; ============================================================

(defun C:NOMEARPAREDE (
  / ssPar ssVao i n ent tp layNome
    paredes vaos pares sorted dadosTabela
    prefixo camada altura fatorM uniOpc
    compMin ignoradas tolY tolVao
    ptTabela
    ptC bruto desc liq altStr dArea
    numStr nome dNome dBruto dDesc dLiq dAlt
    totBruto totVaos totLiq totArea
    largCol0 largCol1 largCol2 largCol3 largCol4 largCol5
    altLinha xTab yTab
    x0 x1 x2 x3 x4 x5 x6
    yTopo yBase yMid)

  (vl-load-com)

  ;;----------------------------------------------------------
  ;; UTILITARIOS
  ;;----------------------------------------------------------

  (defun U:dist2 (a b)
    (sqrt (+ (expt (- (car b)  (car a))  2.0)
             (expt (- (cadr b) (cadr a)) 2.0))))

  ;; Extrai numero apos "_" na string da layer
  (defun U:extraiNum (s / pos sub ok lst)
    (setq pos (vl-string-search "_" s))
    (if (and pos (< (+ pos 1) (strlen s)))
      (progn
        (setq sub (substr s (+ pos 2))
              lst (vl-string->list sub)
              ok  nil)
        (foreach c lst
          (if (and (>= c 48) (<= c 57)) (setq ok T)))
        (if ok sub nil))
      nil))

  ;; Distancia de ponto P ao segmento AB
  (defun U:distPtoSeg (p a b / dx dy tt)
    (setq dx (- (car b)  (car a))
          dy (- (cadr b) (cadr a)))
    (if (and (= dx 0.0) (= dy 0.0))
      (U:dist2 p a)
      (progn
        (setq tt (/ (+ (* (- (car p)  (car a))  dx)
                       (* (- (cadr p) (cadr a)) dy))
                    (+ (* dx dx) (* dy dy))))
        (setq tt (max 0.0 (min 1.0 tt)))
        (U:dist2 p (list (+ (car a)  (* tt dx))
                         (+ (cadr a) (* tt dy)))))))

  ;;----------------------------------------------------------
  ;; MLINE
  ;;----------------------------------------------------------

  (defun ML:verts (e)
    (mapcar 'cdr
      (vl-remove-if-not
        '(lambda (x) (= (car x) 11))
        (entget e))))

  (defun ML:comp (e / verts tot v1)
    (setq verts (ML:verts e) tot 0.0)
    (if (> (length verts) 1)
      (progn
        (setq v1 (car verts))
        (foreach v2 (cdr verts)
          (setq tot (+ tot (U:dist2 v1 v2)) v1 v2))))
    tot)

  (defun ML:centro (e / verts sx sy n)
    (setq verts (ML:verts e) sx 0.0 sy 0.0 n 0)
    (foreach v verts
      (setq sx (+ sx (car v)) sy (+ sy (cadr v)) n (1+ n)))
    (if (> n 0) (list (/ sx n) (/ sy n) 0.0) (list 0.0 0.0 0.0)))

  (defun ML:distPto (pt e / verts dMin d v1)
    (setq verts (ML:verts e) dMin 1.0e10)
    (if (> (length verts) 1)
      (progn
        (setq v1 (car verts))
        (foreach v2 (cdr verts)
          (setq d (U:distPtoSeg pt v1 v2))
          (if (< d dMin) (setq dMin d))
          (setq v1 v2))))
    dMin)

  ;;----------------------------------------------------------
  ;; POLYLINE  ->  retorna lista de pontos 2D
  ;;----------------------------------------------------------

  (defun PL:pts (e / tp ed vtx lst)
    (setq tp (cdr (assoc 0 (entget e))))
    (cond
      ((= tp "LWPOLYLINE")
       (mapcar 'cdr
         (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget e))))
      ((= tp "POLYLINE")
       (setq lst '() vtx (entnext e))
       (while (and vtx (/= (cdr (assoc 0 (entget vtx))) "SEQEND"))
         (setq lst (cons (cdr (assoc 10 (entget vtx))) lst)
               vtx (entnext vtx)))
       (reverse lst))
      (t '())))

  ;; Maior lado de uma lista de pontos (retangulo)
  (defun PL:maiorLado (pts / maxL v1 d)
    (setq maxL 0.0)
    (if (> (length pts) 1)
      (progn
        (setq v1 (car pts))
        (foreach v2 (cdr pts)
          (setq d (U:dist2 v1 v2))
          (if (> d maxL) (setq maxL d))
          (setq v1 v2))
        ;; fecha: ultimo -> primeiro
        (setq d (U:dist2 v1 (car pts)))
        (if (> d maxL) (setq maxL d))))
    maxL)

  ;; Centro de lista de pontos
  (defun PL:centro (pts / sx sy n)
    (setq sx 0.0 sy 0.0 n 0)
    (foreach v pts
      (setq sx (+ sx (car v)) sy (+ sy (cadr v)) n (1+ n)))
    (if (> n 0) (list (/ sx n) (/ sy n) 0.0) (list 0.0 0.0 0.0)))

  ;;----------------------------------------------------------
  ;; EXTRAI VAOS DE BLOCOS/GRUPOS
  ;; Retorna lista de listas de pontos transformados
  ;;----------------------------------------------------------

  (defun BLK:vaos (insEnt / blkNome entBlk sub tp ed layS pts result
                            ox oy ang sx sy)
    (setq result '()
          blkNome (cdr (assoc 2  (entget insEnt)))
          ox      (car  (cdr (assoc 10 (entget insEnt))))
          oy      (cadr (cdr (assoc 10 (entget insEnt))))
          ang     (cdr (assoc 50 (entget insEnt)))
          sx      (cdr (assoc 41 (entget insEnt)))
          sy      (cdr (assoc 42 (entget insEnt)))
          ang     (if ang ang 0.0)
          sx      (if sx  sx  1.0)
          sy      (if sy  sy  1.0)
          entBlk  (tblobjname "BLOCK" blkNome))

    (defun xfPt (p / lx ly)
      (setq lx (* (car p) sx) ly (* (cadr p) sy))
      (list (+ ox (- (* lx (cos ang)) (* ly (sin ang))))
            (+ oy (+ (* lx (sin ang)) (* ly (cos ang))))
            0.0))

    (if entBlk
      (progn
        (setq sub (entnext entBlk))
        (while (and sub (/= (cdr (assoc 0 (entget sub))) "ENDBLK"))
          (setq tp   (cdr (assoc 0 (entget sub)))
                layS (strcase (cdr (assoc 8 (entget sub)))))
          (cond
            ((and (or (= tp "LWPOLYLINE") (= tp "POLYLINE"))
                  (= (substr layS 1 3) "VAO"))
             (setq pts (mapcar 'xfPt (PL:pts sub)))
             (setq result (cons pts result)))
            ((= tp "INSERT")
             (setq result (append result (BLK:vaos sub)))))
          (setq sub (entnext sub)))))
    result)

  ;;----------------------------------------------------------
  ;; ORDENACAO
  ;;----------------------------------------------------------

  (defun U:ordenar (lst tol)
    (vl-sort lst
      '(lambda (a b / ya yb xa xb)
         (setq ya (cadr (car a)) yb (cadr (car b))
               xa (car  (car a)) xb (car  (car b)))
         (cond ((> (- ya yb) tol) T)
               ((> (- yb ya) tol) nil)
               (t (< xa xb))))))

  ;;----------------------------------------------------------
  ;; DESENHO
  ;;----------------------------------------------------------

  (defun D:texto (str pt alt cam)
    (entmake
      (list '(0 . "TEXT")
            (cons 8  cam)
            (cons 10 (list 0.0 0.0 0.0))
            (cons 11 pt)
            (cons 40 alt)
            (cons 1  str)
            (cons 7  "STANDARD")
            (cons 72 1)
            (cons 73 2))))

  (defun D:linha (p1 p2 cam)
    (entmake
      (list '(0 . "LINE")
            (cons 8  cam)
            (cons 10 p1)
            (cons 11 p2))))

  ;;----------------------------------------------------------
  ;; INICIO DO COMANDO
  ;;----------------------------------------------------------

  (prompt "\n=== NOMEAR PAREDES v8.0 ===")

  (prompt "\nSelecione as MLINEs de PAREDE: ")
  (setq ssPar (ssget '((0 . "MLINE"))))
  (if (null ssPar)
    (progn (prompt "\nNenhuma MLINE. Encerrando.") (exit)))

  (prompt "\nSelecione POLYLINEs de VAO ou GRUPOs (Enter para pular): ")
  (setq ssVao (ssget '((0 . "LWPOLYLINE,POLYLINE,INSERT"))))

  (setq prefixo (getstring T "\nPrefixo <P>: "))
  (if (= prefixo "") (setq prefixo "P"))

  (setq camada (getstring T "\nCamada para textos e tabela <NOME_PAREDES>: "))
  (if (= camada "") (setq camada "NOME_PAREDES"))

  (setq altura (getreal "\nAltura do texto <25>: "))
  (if (null altura) (setq altura 25.0))

  (setq ptTabela (getpoint "\nClique para inserir a tabela: "))
  (if (null ptTabela) (progn (prompt "\nPonto nao informado.") (exit)))

  (prompt "\nUnidade: [1]mm  [2]cm  [3]m  <2>: ")
  (setq uniOpc (getint ""))
  (cond ((= uniOpc 1) (setq fatorM 0.001))
        ((= uniOpc 3) (setq fatorM 1.0))
        (t            (setq fatorM 0.01)))

  (setq compMin (/ 0.5 fatorM)
        tolY    (* altura 3.0)
        tolVao  (* altura 10.0))

  (if (null (tblsearch "LAYER" camada))
    (command "_.-LAYER" "_N" camada "_C" "4" camada ""))

  ;;----------------------------------------------------------
  ;; COLETA PAREDES
  ;;----------------------------------------------------------

  (setq paredes '() ignoradas 0 n (sslength ssPar) i 0)
  (while (< i n)
    (setq ent (ssname ssPar i))
    (if (>= (ML:comp ent) compMin)
      (setq paredes (cons ent paredes))
      (setq ignoradas (1+ ignoradas)))
    (setq i (1+ i)))

  ;;----------------------------------------------------------
  ;; COLETA VAOS  ->  lista de listas de pontos 2D
  ;;----------------------------------------------------------

  (setq vaos '())
  (if ssVao
    (progn
      (setq n (sslength ssVao) i 0)
      (while (< i n)
        (setq ent     (ssname ssVao i)
              tp      (cdr (assoc 0 (entget ent)))
              layNome (strcase (cdr (assoc 8 (entget ent)))))
        (cond
          ((and (or (= tp "LWPOLYLINE") (= tp "POLYLINE"))
                (= (substr layNome 1 3) "VAO"))
           (setq vaos (cons (PL:pts ent) vaos)))
          ((= tp "INSERT")
           (setq vaos (append vaos (BLK:vaos ent)))))
        (setq i (1+ i)))))

  (prompt (strcat "\n  Paredes: " (itoa (length paredes))
                  "  Vaos: "     (itoa (length vaos))
                  "  Ignoradas: " (itoa ignoradas)))

  ;;----------------------------------------------------------
  ;; MONTA PARES: (centro bruto desc liq altStr)
  ;;----------------------------------------------------------

  (setq pares '())
  (foreach pw paredes
    (setq ptC    (ML:centro pw)
          bruto  (ML:comp   pw)
          altStr (U:extraiNum (cdr (assoc 8 (entget pw))))
          desc   0.0)
    (foreach vPts vaos
      (setq cVao (PL:centro vPts))
      (if (< (ML:distPto cVao pw) tolVao)
        (setq desc (+ desc (PL:maiorLado vPts)))))
    (setq liq (max 0.0 (- bruto desc)))
    (setq pares
      (cons (list ptC bruto desc liq (if altStr altStr "--"))
            pares)))

  (setq sorted (U:ordenar pares tolY))

  ;;----------------------------------------------------------
  ;; INSERE TEXTOS
  ;;----------------------------------------------------------

  (setq i 1 dadosTabela '())
  (foreach par sorted
    (setq ptC    (nth 0 par)
          bruto  (nth 1 par)
          desc   (nth 2 par)
          liq    (nth 3 par)
          altStr (nth 4 par)
          numStr (if (< i 10) (strcat "0" (itoa i)) (itoa i))
          nome   (strcat prefixo "-" numStr))
    (D:texto nome ptC altura camada)
    (setq dadosTabela
      (append dadosTabela (list (list nome bruto desc liq altStr))))
    (prompt (strcat "\n  " nome
                    "  bruto=" (rtos (* bruto fatorM) 2 2) "m"
                    "  vaos="  (rtos (* desc  fatorM) 2 2) "m"
                    "  liq="   (rtos (* liq   fatorM) 2 2) "m"
                    "  alt="   altStr "m"))
    (setq i (1+ i)))

  ;;----------------------------------------------------------
  ;; TABELA
  ;;----------------------------------------------------------

  (setq largCol0 (* altura 5.5)
        largCol1 (* altura 8.0)
        largCol2 (* altura 6.5)
        largCol3 (* altura 8.0)
        largCol4 (* altura 6.5)
        largCol5 (* altura 7.0)
        altLinha (* altura 2.5)
        xTab     (car  ptTabela)
        yTab     (cadr ptTabela)
        x0 xTab
        x1 (+ x0 largCol0)
        x2 (+ x1 largCol1)
        x3 (+ x2 largCol2)
        x4 (+ x3 largCol3)
        x5 (+ x4 largCol4)
        x6 (+ x5 largCol5))

  ;; Desenha uma faixa (linha topo, base e verticais)
  (defun TAB:faixa (yT yB)
    (D:linha (list x0 yT 0.0) (list x6 yT 0.0) camada)
    (D:linha (list x0 yB 0.0) (list x6 yB 0.0) camada)
    (foreach xx (list x0 x1 x2 x3 x4 x5 x6)
      (D:linha (list xx yT 0.0) (list xx yB 0.0) camada)))

  ;; Centro X entre duas colunas
  (defun TAB:cx (xa xb) (/ (+ xa xb) 2.0))

  ;; Cabecalho
  (setq yTopo yTab
        yBase (- yTab altLinha)
        yMid  (- yTopo (/ altLinha 2.0)))
  (TAB:faixa yTopo yBase)
  (D:texto "PAREDE"     (list (TAB:cx x0 x1) yMid 0.0) altura camada)
  (D:texto "COMP.BRUTO" (list (TAB:cx x1 x2) yMid 0.0) altura camada)
  (D:texto "VAOS"       (list (TAB:cx x2 x3) yMid 0.0) altura camada)
  (D:texto "COMP.LIQ"   (list (TAB:cx x3 x4) yMid 0.0) altura camada)
  (D:texto "ALTURA"     (list (TAB:cx x4 x5) yMid 0.0) altura camada)
  (D:texto "AREA(m2)"   (list (TAB:cx x5 x6) yMid 0.0) altura camada)

  ;; Linhas de dados
  (setq totBruto 0.0 totVaos 0.0 totLiq 0.0 totArea 0.0)
  (foreach dado dadosTabela
    (setq dNome  (nth 0 dado)
          dBruto (nth 1 dado)
          dDesc  (nth 2 dado)
          dLiq   (nth 3 dado)
          dAlt   (nth 4 dado)
          dArea  (if (/= dAlt "--")
                   (* (* dLiq fatorM) (atof dAlt))
                   nil)
          totBruto (+ totBruto dBruto)
          totVaos  (+ totVaos  dDesc)
          totLiq   (+ totLiq   dLiq)
          totArea  (+ totArea  (if dArea dArea 0.0))
          yTopo    (- yTopo altLinha)
          yBase    (- yTopo altLinha)
          yMid     (- yTopo (/ altLinha 2.0)))
    (TAB:faixa yTopo yBase)
    (D:texto dNome
      (list (TAB:cx x0 x1) yMid 0.0) altura camada)
    (D:texto (strcat (rtos (* dBruto fatorM) 2 2) "m")
      (list (TAB:cx x1 x2) yMid 0.0) altura camada)
    (D:texto (if (> dDesc 0.0) (strcat (rtos (* dDesc fatorM) 2 2) "m") "--")
      (list (TAB:cx x2 x3) yMid 0.0) altura camada)
    (D:texto (strcat (rtos (* dLiq fatorM) 2 2) "m")
      (list (TAB:cx x3 x4) yMid 0.0) altura camada)
    (D:texto (if (= dAlt "--") "--" (strcat dAlt "m"))
      (list (TAB:cx x4 x5) yMid 0.0) altura camada)
    (D:texto (if dArea (strcat (rtos dArea 2 2) "m2") "--")
      (list (TAB:cx x5 x6) yMid 0.0) altura camada))

  ;; Linha de total
  (setq yTopo (- yTopo altLinha)
        yBase (- yTopo altLinha)
        yMid  (- yTopo (/ altLinha 2.0)))
  (TAB:faixa yTopo yBase)
  (D:texto "TOTAL"
    (list (TAB:cx x0 x1) yMid 0.0) altura camada)
  (D:texto (strcat (rtos (* totBruto fatorM) 2 2) "m")
    (list (TAB:cx x1 x2) yMid 0.0) altura camada)
  (D:texto (strcat (rtos (* totVaos fatorM) 2 2) "m")
    (list (TAB:cx x2 x3) yMid 0.0) altura camada)
  (D:texto (strcat (rtos (* totLiq fatorM) 2 2) "m")
    (list (TAB:cx x3 x4) yMid 0.0) altura camada)
  (D:texto "--"
    (list (TAB:cx x4 x5) yMid 0.0) altura camada)
  (D:texto (strcat (rtos totArea 2 2) "m2")
    (list (TAB:cx x5 x6) yMid 0.0) altura camada)

  (prompt (strcat "\n\nConcluido! " (itoa (1- i)) " parede(s)."
                  "  Bruto:"  (rtos (* totBruto fatorM) 2 2) "m"
                  "  Liq:"    (rtos (* totLiq   fatorM) 2 2) "m"
                  "  Area:"   (rtos totArea 2 2) "m2"))
  (princ)
)

(prompt "\nNomearParede.lsp v8.0 carregado -> NOMEARPAREDE")
(princ)