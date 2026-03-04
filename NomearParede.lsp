;;; ============================================================
;;;  NomearParede.lsp  v11.0
;;;  Paredes : MLINE  (layer ex: parede_3.35 -> altura 3.35m)
;;;  Vaos    : LWPOLYLINE/POLYLINE ou blocos
;;;            (layer ex: VAO_0.90 -> altura vao 0.90m)
;;;            Largura vao = maior lado do retangulo desenhado
;;;  Desconto: AREA.LIQ = (COMP * ALT.PAREDE) - SUM(LARG.VAO * ALT.VAO)
;;;  Tabela  : PAREDE | COMP | N.VAO | LARG.VAO | ALT.VAO | AREA.VAO | ALTURA | AREA.BRUTA | AREA.LIQ
;;;  Ordem   : cima->baixo / esquerda->direita
;;;  Comando : NOMEARPAREDE
;;; ============================================================

(defun C:NOMEARPAREDE (
  / ssPar ssVao i n ent tp lay idx
    paredes vaos assocVaos pares sorted dadosTabela
    prefixo camada altura fatorM uniOpc
    compMin ignoradas tolY
    ptTabela
    ptC comp altStr vaosList desc nome numStr
    vRec larg vAlt areaVao
    dRec dNome dComp dVaos dAlt dAreaBruta dAreaLiq vIdx v1
    totComp totAreaBruta totAreaVaos totAreaLiq
    C0 C1 C2 C3 C4 C5 C6 C7 C8
    altLinha xTab yTab
    x0 x1 x2 x3 x4 x5 x6 x7 x8 x9
    yTopo yBase yMid)

  (vl-load-com)

  ;;----------------------------------------------------------
  ;; UTILITARIOS
  ;;----------------------------------------------------------

  (defun U:dist2 (a b)
    (sqrt (+ (expt (- (car b)  (car a))  2.0)
             (expt (- (cadr b) (cadr a)) 2.0))))

  (defun U:extraiNum (s / pos sub k c ok)
    (setq pos nil k 1)
    (while (<= k (strlen s))
      (if (= (substr s k 1) "_") (setq pos k))
      (setq k (1+ k)))
    (if (and pos (< pos (strlen s)))
      (progn
        (setq sub (substr s (1+ pos)) ok nil k 1)
        (while (<= k (strlen sub))
          (setq c (ascii (substr sub k 1)))
          (if (and (>= c 48) (<= c 57)) (setq ok T))
          (setq k (1+ k)))
        (if ok sub nil))
      nil))

  (defun U:distSeg (p a b / dx dy tt)
    (setq dx (- (car b) (car a))
          dy (- (cadr b) (cadr a)))
    (if (and (= dx 0.0) (= dy 0.0))
      (U:dist2 p a)
      (progn
        (setq tt (max 0.0 (min 1.0
                   (/ (+ (* (- (car p) (car a)) dx)
                         (* (- (cadr p) (cadr a)) dy))
                      (+ (* dx dx) (* dy dy))))))
        (U:dist2 p (list (+ (car a) (* tt dx))
                         (+ (cadr a) (* tt dy)))))))

  ;;----------------------------------------------------------
  ;; MLINE
  ;;----------------------------------------------------------

  (defun ML:verts (e)
    (mapcar 'cdr
      (vl-remove-if-not '(lambda (x) (= (car x) 11)) (entget e))))

  (defun ML:comp (e / v tot v1)
    (setq v (ML:verts e) tot 0.0)
    (if (> (length v) 1)
      (progn (setq v1 (car v))
             (foreach v2 (cdr v)
               (setq tot (+ tot (U:dist2 v1 v2)) v1 v2))))
    tot)

  (defun ML:centro (e / v sx sy n)
    (setq v (ML:verts e) sx 0.0 sy 0.0 n 0)
    (foreach p v (setq sx (+ sx (car p)) sy (+ sy (cadr p)) n (1+ n)))
    (if (> n 0) (list (/ sx n) (/ sy n) 0.0) (list 0.0 0.0 0.0)))

  (defun ML:distPto (pt e / v dMin d v1)
    (setq v (ML:verts e) dMin 1.0e10)
    (if (> (length v) 1)
      (progn (setq v1 (car v))
             (foreach v2 (cdr v)
               (setq d (U:distSeg pt v1 v2))
               (if (< d dMin) (setq dMin d))
               (setq v1 v2))))
    dMin)

  ;;----------------------------------------------------------
  ;; POLYLINE -> lista de pontos 2D
  ;;----------------------------------------------------------

  (defun PL:pts (e / tp lst vtx)
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

  (defun PL:maiorLado (pts / mx v1 d)
    (setq mx 0.0)
    (if (> (length pts) 1)
      (progn
        (setq v1 (car pts))
        (foreach v2 (cdr pts)
          (setq d (U:dist2 v1 v2))
          (if (> d mx) (setq mx d))
          (setq v1 v2))
        (setq d (U:dist2 v1 (car pts)))
        (if (> d mx) (setq mx d))))
    mx)

  (defun PL:centro (pts / sx sy n)
    (setq sx 0.0 sy 0.0 n 0)
    (foreach p pts (setq sx (+ sx (car p)) sy (+ sy (cadr p)) n (1+ n)))
    (if (> n 0) (list (/ sx n) (/ sy n) 0.0) (list 0.0 0.0 0.0)))

  ;;----------------------------------------------------------
  ;; VAOS EM BLOCOS
  ;;----------------------------------------------------------

  (defun BLK:vaos (ins / blk sub tp layS pts ox oy ang sx sy res)
    (setq res '()
          blk (tblobjname "BLOCK" (cdr (assoc 2 (entget ins))))
          ox  (car  (cdr (assoc 10 (entget ins))))
          oy  (cadr (cdr (assoc 10 (entget ins))))
          ang (cdr (assoc 50 (entget ins)))
          sx  (cdr (assoc 41 (entget ins)))
          sy  (cdr (assoc 42 (entget ins)))
          ang (if ang ang 0.0)
          sx  (if sx sx 1.0)
          sy  (if sy sy 1.0))
    (defun xf (p / lx ly)
      (setq lx (* (car p) sx) ly (* (cadr p) sy))
      (list (+ ox (- (* lx (cos ang)) (* ly (sin ang))))
            (+ oy (+ (* lx (sin ang)) (* ly (cos ang)))) 0.0))
    (if blk
      (progn
        (setq sub (entnext blk))
        (while (and sub (/= (cdr (assoc 0 (entget sub))) "ENDBLK"))
          (setq tp   (cdr (assoc 0 (entget sub)))
                layS (strcase (cdr (assoc 8 (entget sub)))))
          (cond
            ((and (or (= tp "LWPOLYLINE") (= tp "POLYLINE"))
                  (vl-string-search "VAO" layS))
             (setq pts (mapcar 'xf (PL:pts sub)))
             (setq res (cons (list pts (U:extraiNum layS)) res)))
            ((= tp "INSERT")
             (setq res (append res (BLK:vaos sub)))))
          (setq sub (entnext sub)))))
    res)

  ;;----------------------------------------------------------
  ;; PROJECAO: vao projeta dentro do segmento da parede?
  ;;----------------------------------------------------------

  (defun U:vaoNaParede (cVao pw / verts v1 ok dx dy len tt)
    (setq verts (ML:verts pw) ok nil)
    (if (> (length verts) 1)
      (progn
        (setq v1 (car verts))
        (foreach v2 (cdr verts)
          (setq dx  (- (car v2)  (car v1))
                dy  (- (cadr v2) (cadr v1))
                len (+ (* dx dx) (* dy dy)))
          (if (> len 0.0)
            (progn
              (setq tt (/ (+ (* (- (car cVao)  (car v1))  dx)
                             (* (- (cadr cVao) (cadr v1)) dy))
                          len))
              (if (and (>= tt -0.05) (<= tt 1.05)) (setq ok T))))
          (setq v1 v2))))
    ok)

  ;;----------------------------------------------------------
  ;; ASSOCIA cada vao a UMA unica parede (a mais proxima)
  ;;----------------------------------------------------------

  (defun U:associaVaos (vaos paredes / result vRec vPts vAlt cVao
                                        bestIdx bestDist d idx)
    (setq result '())
    (foreach vRec vaos
      (setq vPts     (car  vRec)
            vAlt     (cadr vRec)
            cVao     (PL:centro vPts)
            bestIdx  -1
            bestDist 1.0e10
            idx      0)
      (foreach pw paredes
        (if (U:vaoNaParede cVao pw)
          (progn
            (setq d (ML:distPto cVao pw))
            (if (< d bestDist) (setq bestDist d bestIdx idx))))
        (setq idx (1+ idx)))
      (if (>= bestIdx 0)
        (setq result
          (cons (list bestIdx (PL:maiorLado vPts) vAlt) result))))
    result)

  ;;----------------------------------------------------------
  ;; ORDENACAO
  ;;----------------------------------------------------------

  (defun U:sort (lst tol)
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

  (defun D:txt (s p h cam)
    (entmake (list '(0 . "TEXT")
                   (cons 8  cam)
                   (cons 10 (list 0.0 0.0 0.0))
                   (cons 11 p)
                   (cons 40 h)
                   (cons 1  s)
                   (cons 7  "STANDARD")
                   (cons 72 1)
                   (cons 73 2))))

  (defun D:lin (p1 p2 cam)
    (entmake (list '(0 . "LINE")
                   (cons 8  cam)
                   (cons 10 p1)
                   (cons 11 p2))))

  ;;----------------------------------------------------------
  ;; INICIO
  ;;----------------------------------------------------------

  (prompt "\n=== NOMEAR PAREDES v11.0 ===")

  (prompt "\nSelecione as MLINEs de PAREDE: ")
  (setq ssPar (ssget '((0 . "MLINE"))))
  (if (null ssPar) (progn (prompt "\nNenhuma MLINE. Encerrando.") (exit)))

  (prompt "\nSelecione POLYLINEs de VAO ou GRUPOs (Enter pula): ")
  (setq ssVao (ssget '((0 . "LWPOLYLINE,POLYLINE,INSERT"))))

  (setq prefixo (getstring T "\nPrefixo <P>: "))
  (if (= prefixo "") (setq prefixo "P"))

  (setq camada (getstring T "\nCamada textos/tabela <NOME_PAREDES>: "))
  (if (= camada "") (setq camada "NOME_PAREDES"))

  (setq altura (getreal "\nAltura texto <25>: "))
  (if (null altura) (setq altura 25.0))

  (setq ptTabela (getpoint "\nClique para inserir tabela: "))
  (if (null ptTabela) (progn (prompt "\nPonto nao informado.") (exit)))

  (prompt "\nUnidade: [1]mm  [2]cm  [3]m  <2>: ")
  (setq uniOpc (getint ""))
  (cond ((= uniOpc 1) (setq fatorM 0.001))
        ((= uniOpc 3) (setq fatorM 1.0))
        (t            (setq fatorM 0.01)))

  (setq compMin (/ 0.5 fatorM)
        tolY    (* altura 3.0))

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
  ;; COLETA VAOS -> lista de (ptsList altStr)
  ;;----------------------------------------------------------

  (setq vaos '())
  (if ssVao
    (progn
      (setq n (sslength ssVao) i 0)
      (while (< i n)
        (setq ent (ssname ssVao i)
              tp  (cdr (assoc 0 (entget ent)))
              lay (strcase (cdr (assoc 8 (entget ent)))))
        (cond
          ((and (or (= tp "LWPOLYLINE") (= tp "POLYLINE"))
                (vl-string-search "VAO" lay))
           (setq vaos (cons (list (PL:pts ent) (U:extraiNum lay)) vaos)))
          ((= tp "INSERT")
           (setq vaos (append vaos (BLK:vaos ent)))))
        (setq i (1+ i)))))

  (prompt (strcat "\n  Paredes: " (itoa (length paredes))
                  "  Vaos: "     (itoa (length vaos))
                  "  Ignoradas: " (itoa ignoradas)))

  ;;----------------------------------------------------------
  ;; ASSOCIA VAOS -> UMA parede cada
  ;;----------------------------------------------------------

  (setq assocVaos (U:associaVaos vaos paredes))

  ;;----------------------------------------------------------
  ;; MONTA PARES: (ptC comp vaosList altStr)
  ;; vaosList = ((larg altStr) ...)  -- sem desconto linear
  ;;----------------------------------------------------------

  (setq pares '() idx 0)
  (foreach pw paredes
    (setq ptC      (ML:centro pw)
          comp     (ML:comp pw)
          altStr   (U:extraiNum (cdr (assoc 8 (entget pw))))
          vaosList '())
    (foreach aRec assocVaos
      (if (= (nth 0 aRec) idx)
        (setq vaosList
          (append vaosList (list (list (nth 1 aRec) (nth 2 aRec)))))))
    (setq pares (cons (list ptC comp vaosList (if altStr altStr "--")) pares))
    (setq idx (1+ idx)))

  (setq sorted (U:sort pares tolY))

  ;;----------------------------------------------------------
  ;; INSERE TEXTOS
  ;;----------------------------------------------------------

  (setq i 1 dadosTabela '())
  (foreach par sorted
    (setq ptC      (nth 0 par)
          comp     (nth 1 par)
          vaosList (nth 2 par)
          altStr   (nth 3 par)
          numStr   (if (< i 10) (strcat "0" (itoa i)) (itoa i))
          nome     (strcat prefixo "-" numStr))
    (D:txt nome ptC altura camada)
    (setq dadosTabela
      (append dadosTabela (list (list nome comp vaosList altStr))))
    (prompt (strcat "\n  " nome
                    " comp="  (rtos (* comp fatorM) 2 2) "m"
                    " vaos="  (itoa (length vaosList))
                    " alt="   altStr "m"))
    (setq i (1+ i)))

  ;;----------------------------------------------------------
  ;; TABELA
  ;; Colunas: PAREDE | COMP | N.VAO | LARG.VAO | ALT.VAO | AREA.VAO | ALTURA | AREA.BRUTA | AREA.LIQ
  ;;----------------------------------------------------------

  (setq C0 (* altura 5.0)    ;; PAREDE
        C1 (* altura 7.0)    ;; COMP
        C2 (* altura 4.5)    ;; N.VAO
        C3 (* altura 7.0)    ;; LARG.VAO
        C4 (* altura 6.0)    ;; ALT.VAO
        C5 (* altura 7.0)    ;; AREA.VAO
        C6 (* altura 6.0)    ;; ALTURA
        C7 (* altura 7.5)    ;; AREA.BRUTA
        C8 (* altura 7.5)    ;; AREA.LIQ
        altLinha (* altura 2.5)
        xTab (car  ptTabela)
        yTab (cadr ptTabela)
        x0 xTab
        x1 (+ x0 C0)
        x2 (+ x1 C1)
        x3 (+ x2 C2)
        x4 (+ x3 C3)
        x5 (+ x4 C4)
        x6 (+ x5 C5)
        x7 (+ x6 C6)
        x8 (+ x7 C7)
        x9 (+ x8 C8))

  (defun T:faixa (yT yB)
    (D:lin (list x0 yT 0.0) (list x9 yT 0.0) camada)
    (D:lin (list x0 yB 0.0) (list x9 yB 0.0) camada)
    (foreach xx (list x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
      (D:lin (list xx yT 0.0) (list xx yB 0.0) camada)))

  (defun T:subFaixa (yT yB)
    (D:lin (list x0 yB 0.0) (list x9 yB 0.0) camada)
    (foreach xx (list x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
      (D:lin (list xx yT 0.0) (list xx yB 0.0) camada)))

  (defun T:cx (a b) (/ (+ a b) 2.0))

  ;; Cabecalho
  (setq yTopo yTab
        yBase (- yTab altLinha)
        yMid  (- yTopo (/ altLinha 2.0)))
  (T:faixa yTopo yBase)
  (D:txt "PAREDE"     (list (T:cx x0 x1) yMid 0.0) altura camada)
  (D:txt "COMP"       (list (T:cx x1 x2) yMid 0.0) altura camada)
  (D:txt "N.VAO"      (list (T:cx x2 x3) yMid 0.0) altura camada)
  (D:txt "LARG.VAO"   (list (T:cx x3 x4) yMid 0.0) altura camada)
  (D:txt "ALT.VAO"    (list (T:cx x4 x5) yMid 0.0) altura camada)
  (D:txt "AREA.VAO"   (list (T:cx x5 x6) yMid 0.0) altura camada)
  (D:txt "ALTURA"     (list (T:cx x6 x7) yMid 0.0) altura camada)
  (D:txt "AREA.BRUTA" (list (T:cx x7 x8) yMid 0.0) altura camada)
  (D:txt "AREA.LIQ"   (list (T:cx x8 x9) yMid 0.0) altura camada)

  ;; Dados
  (setq totComp 0.0 totAreaBruta 0.0 totAreaVaos 0.0 totAreaLiq 0.0)
  (foreach dRec dadosTabela
    (setq dNome  (nth 0 dRec)
          dComp  (nth 1 dRec)
          dVaos  (nth 2 dRec)
          dAlt   (nth 3 dRec))

    ;; Calcula areas
    (setq dAreaBruta
      (if (/= dAlt "--")
        (* (* dComp fatorM) (atof dAlt)) nil))

    ;; Soma areas dos vaos: larg(m) * alt(m)
    (setq sumAreaVaos 0.0)
    (foreach vRec dVaos
      (setq larg    (* (nth 0 vRec) fatorM)
            vAlt    (nth 1 vRec))
      (if vAlt
        (setq sumAreaVaos (+ sumAreaVaos (* larg (atof vAlt))))))

    (setq dAreaLiq
      (if dAreaBruta
        (max 0.0 (- dAreaBruta sumAreaVaos)) nil))

    (setq totComp     (+ totComp     dComp)
          totAreaBruta (+ totAreaBruta (if dAreaBruta dAreaBruta 0.0))
          totAreaVaos  (+ totAreaVaos  sumAreaVaos)
          totAreaLiq   (+ totAreaLiq   (if dAreaLiq dAreaLiq 0.0)))

    ;; Linha principal da parede
    (setq yTopo (- yTopo altLinha)
          yBase (- yTopo altLinha)
          yMid  (- yTopo (/ altLinha 2.0)))
    (T:faixa yTopo yBase)
    (D:txt dNome                                             (list (T:cx x0 x1) yMid 0.0) altura camada)
    (D:txt (strcat (rtos (* dComp fatorM) 2 2) "m")          (list (T:cx x1 x2) yMid 0.0) altura camada)
    (D:txt (itoa (length dVaos))                             (list (T:cx x2 x3) yMid 0.0) altura camada)
    (cond
      ((= (length dVaos) 0)
       (D:txt "--" (list (T:cx x3 x4) yMid 0.0) altura camada)
       (D:txt "--" (list (T:cx x4 x5) yMid 0.0) altura camada)
       (D:txt "--" (list (T:cx x5 x6) yMid 0.0) altura camada))
      ((= (length dVaos) 1)
       (setq v1 (car dVaos)
             areaVao (* (* (nth 0 v1) fatorM)
                        (if (nth 1 v1) (atof (nth 1 v1)) 0.0)))
       (D:txt (strcat (rtos (* (nth 0 v1) fatorM) 2 2) "m")
         (list (T:cx x3 x4) yMid 0.0) altura camada)
       (D:txt (if (nth 1 v1) (strcat (nth 1 v1) "m") "--")
         (list (T:cx x4 x5) yMid 0.0) altura camada)
       (D:txt (strcat (rtos areaVao 2 2) "m2")
         (list (T:cx x5 x6) yMid 0.0) altura camada))
      (t
       (D:txt "v.abaixo" (list (T:cx x3 x4) yMid 0.0) (* altura 0.75) camada)
       (D:txt "v.abaixo" (list (T:cx x4 x5) yMid 0.0) (* altura 0.75) camada)
       (D:txt (strcat (rtos sumAreaVaos 2 2) "m2")
         (list (T:cx x5 x6) yMid 0.0) altura camada)))
    (D:txt (if (= dAlt "--") "--" (strcat dAlt "m"))
      (list (T:cx x6 x7) yMid 0.0) altura camada)
    (D:txt (if dAreaBruta (strcat (rtos dAreaBruta 2 2) "m2") "--")
      (list (T:cx x7 x8) yMid 0.0) altura camada)
    (D:txt (if dAreaLiq (strcat (rtos dAreaLiq 2 2) "m2") "--")
      (list (T:cx x8 x9) yMid 0.0) altura camada)

    ;; Sub-linhas individuais de vao (se > 1)
    (if (> (length dVaos) 1)
      (progn
        (setq vIdx 1)
        (foreach vRec dVaos
          (setq areaVao (* (* (nth 0 vRec) fatorM)
                           (if (nth 1 vRec) (atof (nth 1 vRec)) 0.0)))
          (setq yTopo (- yTopo altLinha)
                yBase (- yTopo altLinha)
                yMid  (- yTopo (/ altLinha 2.0)))
          (T:subFaixa yTopo yBase)
          (D:txt (strcat "Vao-" (itoa vIdx))
            (list (T:cx x0 x1) yMid 0.0) (* altura 0.8) camada)
          (D:txt "" (list (T:cx x1 x2) yMid 0.0) altura camada)
          (D:txt (itoa vIdx)
            (list (T:cx x2 x3) yMid 0.0) (* altura 0.8) camada)
          (D:txt (strcat (rtos (* (nth 0 vRec) fatorM) 2 2) "m")
            (list (T:cx x3 x4) yMid 0.0) (* altura 0.8) camada)
          (D:txt (if (nth 1 vRec) (strcat (nth 1 vRec) "m") "--")
            (list (T:cx x4 x5) yMid 0.0) (* altura 0.8) camada)
          (D:txt (strcat (rtos areaVao 2 2) "m2")
            (list (T:cx x5 x6) yMid 0.0) (* altura 0.8) camada)
          (D:txt "" (list (T:cx x6 x7) yMid 0.0) altura camada)
          (D:txt "" (list (T:cx x7 x8) yMid 0.0) altura camada)
          (D:txt "" (list (T:cx x8 x9) yMid 0.0) altura camada)
          (setq vIdx (1+ vIdx))))))

  ;; Total
  (setq yTopo (- yTopo altLinha)
        yBase (- yTopo altLinha)
        yMid  (- yTopo (/ altLinha 2.0)))
  (T:faixa yTopo yBase)
  (D:txt "TOTAL"
    (list (T:cx x0 x1) yMid 0.0) altura camada)
  (D:txt (strcat (rtos (* totComp fatorM) 2 2) "m")
    (list (T:cx x1 x2) yMid 0.0) altura camada)
  (D:txt "--" (list (T:cx x2 x3) yMid 0.0) altura camada)
  (D:txt "--" (list (T:cx x3 x4) yMid 0.0) altura camada)
  (D:txt "--" (list (T:cx x4 x5) yMid 0.0) altura camada)
  (D:txt (strcat (rtos totAreaVaos 2 2) "m2")
    (list (T:cx x5 x6) yMid 0.0) altura camada)
  (D:txt "--" (list (T:cx x6 x7) yMid 0.0) altura camada)
  (D:txt (strcat (rtos totAreaBruta 2 2) "m2")
    (list (T:cx x7 x8) yMid 0.0) altura camada)
  (D:txt (strcat (rtos totAreaLiq 2 2) "m2")
    (list (T:cx x8 x9) yMid 0.0) altura camada)

  (prompt (strcat "\n\nConcluido! " (itoa (1- i)) " parede(s)."
                  "  Area.Bruta:" (rtos totAreaBruta 2 2) "m2"
                  "  Area.Vaos:"  (rtos totAreaVaos  2 2) "m2"
                  "  Area.Liq:"   (rtos totAreaLiq   2 2) "m2"))
  (princ)
)

(prompt "\nNomearParede.lsp v11.0 carregado -> NOMEARPAREDE")
(princ)
