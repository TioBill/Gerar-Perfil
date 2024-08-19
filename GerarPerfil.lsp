"
A função do comando é simples:
Gerar o perfil de forma automática

1: Colocando uma line de cor azul a cada \"endpoint\" de uma PolyLine
2: Realizar os cálculos e colocar abaixo da linha base: 

Estacas, Cota do terreno, Cota geratriz inferior, Profundidade, distancia progressiva, declividade, extensão.
"

(command "_.style" "Standard" "" "0" "1" "0" "No" "No")


(setvar 'cmdecho 0)

(setq acadObj (vlax-get-acad-object)
  doc (vla-get-activedocument acadObj)
  model_space (vla-get-modelspace doc)
) ; defun

(defun criaLayersNecessarias ()
  (if (null (tblsearch "LAYER" "EMPRESAOSM_TEXTO_FINO"))
    (entmake 
      '(
        (0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "EMPRESAOSM_TEXTO_FINO") (70 . 0) (62 . 7) (6 . "Continuous")
      )
    )
  )
  
  (if (null (tblsearch "LAYER" "EMPRESAOSM_TERRENO"))
    (entmake
      '(
        (0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "EMPRESAOSM_TERRENO") (70 . 0) (62 . 54) (6 . "Continuous")
      )
    )
  )

  (if (null (tblsearch "LAYER" "EMPRESAOSM_LINHA_ESTACA"))
    (entmake 
    '(
      (0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "EMPRESAOSM_LINHA_ESTACA") (70 . 0) (62 . 5) (6 . "Continuous")
      )
    )    
  )

  (if (null (tblsearch "LAYER" "EMPRESAOSM_LINHA_ESTACAS"))
    (entmake 
      '(
        (0 . "LAYER") (100 . "AcDbSymbolTableRecord") (100 . "AcDbLayerTableRecord") (2 . "EMPRESAOSM_LINHA_ESTACAS") '(70 . 0) '(62 . 5) '(6 . "Continuous")
      )
    )
  )
) ; defun


; // NOTE - Otimizado
(defun desenha-linha-estaca(coordenadas-geratriz-inferior coordenada-y-da-linha-de-base 
                            / contador startpoint endpoint)
  "Função que recebe uma lista de coordenadas de uma Polyline e desenha a cada posição uma linha na cor azul"
  
  (setq 
    contador 0
    contador-y 1
  )
  
  ; Variavel contador coordenada y
 
  
  (repeat (/ (length coordenadas-geratriz-inferior) 2)
    
    (setq 
      startpoint (vlax-3d-point (nth contador coordenadas-geratriz-inferior) (nth (1+ contador) coordenadas-geratriz-inferior) 0)
      endpoint (vlax-3d-point (nth contador coordenadas-geratriz-inferior) coordenada-y-da-linha-base 0))
        

    ; Desenha uma linha de cor azul     
    (vla-put-layer (vla-addline model_space startpoint endpoint) "EMPRESAOSM_LINHA_ESTACA")
    (vla-put-color (vlax-ename->vla-object (entlast)) 256)
    
    (setq contador (+ contador 2))
  ) ; repeat
  
  (princ)
  
) ; defun


; // NOTE - Otimizado
(defun muda_layer ( multiple_objects nome_layer cor / contador)
  """
  Simplesmente muda a layer de qualquer objeto já criado anteriormente.
  """
  (repeat (setq contador (sslength multiple_objects))
    (vla-put-layer (vlax-ename->vla-object (ssname multiple_objects (setq contador (1- contador)))) nome_layer)
  )
) ; defun


; // NOTE - Revisado
(defun escreve-numero-da-estaca (coordenadas-da-geratriz-inferior linha-base tamanho-do-texto distancia-digitada 
                                 / estaca-intermediaria 
                                 distancia estaca-anterior estaca-posterior posicao-x posicao-y 
                                 numero-da-estaca contador-auxiliar-estaca-anterior contador-auxiliar-estaca-posterior estaca-atual texto_atual
                                 )
  "
  Escreve abaixo de cada linha de estaca seu respectivo número
  
  Essa função recebe as coordenadas da geratriz-inferior, a linha base e o tamanho que o texto terá.
  "
  
  (setq contador-auxiliar-estaca-anterior 0)
  (setq contador-auxiliar-estaca-posterior 2)
  (setq estaca-atual 0)
  
  (setq numero-da-estaca 0)
  
  (setq posicao-y (- linha-base (* tamanho-do-texto 20))) ; Seta a posição inicial de Y
  
  (repeat (/ (length coordenadas-da-geratriz-inferior) 2)
    (setq posicao-x (nth estaca-atual coordenadas-da-geratriz-inferior))
    
    (setq estaca-anterior (nth contador-auxiliar-estaca-anterior coordenadas-da-geratriz-inferior))
    (setq estaca-posterior (nth contador-auxiliar-estaca-posterior coordenadas-da-geratriz-inferior))
    
    (setq distancia (distance (list estaca-anterior 0 0) (list estaca-posterior 0 0)))
    
    (if (= estaca-atual 0)
      (progn
        (setq texto_atual (vla-addtext model_space numero-da-estaca (vlax-3d-point posicao-x posicao-y 0) tamanho-do-texto))
        (vla-put-rotation texto_atual 1.5708)
        (vla-put-layer texto_atual "EMPRESAOSM_TEXTO_FINO")
        (vla-put-color (vlax-ename->vla-object (entlast)) 256)
      )
      
  
      ;; Bug estranho acontencendo onde dois números iguais no calculo de >= diz que a variavel distancia é menor que distancia-digitada, corrigido?
      (if  (or (>= distancia distancia-digitada) (eq (rtos distancia 2 2) (rtos distancia-digitada 2 2))) ; Distancia de estaca. A cada X (m) aumenta contagem estaca +1.
        (progn
          (setq contador-auxiliar-estaca-anterior contador-auxiliar-estaca-posterior)
          (setq numero-da-estaca (+ numero-da-estaca 1))
          
          (setq texto_atual (vla-addtext model_space numero-da-estaca (vlax-3d-point posicao-x posicao-y 0) tamanho-do-texto))
          (vla-put-rotation texto_atual 1.5708) ; Rotação de 90º
          (vla-put-layer texto_atual "EMPRESAOSM_TEXTO_FINO")
          (vla-put-color (vlax-ename->vla-object (entlast)) 256)
          
          (setq contador-auxiliar-estaca-posterior (+ contador-auxiliar-estaca-posterior 2))
        )
      
        (progn
          (setq estaca-intermediaria (strcat (rtos numero-da-estaca 2 0) " + " (rtos distancia 2 0)))
          
          (setq texto_atual (vla-addtext model_space estaca-intermediaria (vlax-3d-point posicao-x posicao-y 0) tamanho-do-texto))
          (vla-put-rotation texto_atual 1.5708)
          (vla-put-layer texto_atual "EMPRESAOSM_TEXTO_FINO")
          (vla-put-color (vlax-ename->vla-object (entlast)) 256)
          
          (setq contador-auxiliar-estaca-posterior (+ contador-auxiliar-estaca-posterior 2))
        )
      )   
    )
    

    (setq estaca-atual (+ estaca-atual 2))
  ) ; repeat

) ; defun


; // NOTE - Revisado
(defun escreve-distancia-progressiva (coordenadas-da-geratriz-inferior linha-base tamanho-do-texto 
                                      / distancia-progressiva 
                                      estaca-atual 
                                      posicao-y  
                                      primeira-estaca 
                                      contador-auxiliar 
                                      insertionPoint texto_atual)
  "
  Escreve da profundidade a distancia da primeira estaca e a estaca atual
  "
  
  (setq primeira-estaca (car coordenadas-da-geratriz-inferior))
  
  (setq contador-auxiliar 0)
  
  (setq posicao-y (- linha-base (* tamanho-do-texto 52)))
  
  (repeat (/ (length coordenadas-da-geratriz-inferior) 2)
    
    (setq estaca-atual (nth contador-auxiliar coordenadas-da-geratriz-inferior))

    (setq distancia-progressiva (rtos (distance (list primeira-estaca 0 0) (list estaca-atual 0 0)) 2 2))
    
    (setq insertionPoint (vlax-3d-point estaca-atual posicao-y 0))
    
    (setq texto_atual (vla-addtext model_space (strcat distancia-progressiva "m") insertionPoint tamanho-do-texto))
    (vla-put-rotation texto_atual 1.5708)
    (vla-put-layer texto_atual "EMPRESAOSM_TEXTO_FINO")
    (vla-put-color (vlax-ename->vla-object (entlast)) 256)
    
    (setq contador-auxiliar (+ contador-auxiliar 2))
  ) ; repeat
  
) ; defun


; // NOTE - Revisado
(defun escreve-cota (coordenadas linha-base tamanho-do-texto valor-inicial distancia-texto 
                     / inclinacao-do-texto contador-auxiliar-de-x posicao-y
                     ponto-y-da-linha estaca-atual cota insertionPoint texto_atual)
  "
  Escreve abaixo da cota do terreno o nivel da geratriz inferior no terreno
  
  // NOTE Devemos considerar que a cada 10cm é acrescentado uma nova cota do terreno.
  Logo se começamos com um valor inicial de 0 a cada 10cm é aumentado esse valor, ou seja a nova cota seria 1.
  "
  
  (setq contador-auxiliar-de-x 0)
  
  (setq posicao-y (- linha-base distancia-texto))
  
  
  (repeat (/ (length coordenadas) 2)
    (setq ponto-y-da-linha (nth (1+ contador-auxiliar-de-x) coordenadas))

    (setq estaca-atual (nth contador-auxiliar-de-x coordenadas))
    
    (setq cota (distance (list 0 linha-base 0) (list 0 ponto-y-da-linha 0))) ; Pegamos primeiro a distancia do piso até a linha

    (setq cota (rtos (+ (/ cota 10) valor-inicial) 2 3)) ; Depois dividimos por dez e acrescentamos o número inicial
    
    (setq texto_atual (vla-addtext model_space cota (vlax-3d-point estaca-atual posicao-y 0) tamanho-do-texto))
    (vla-put-rotation texto_atual 1.5708)
    (vla-put-layer texto_atual "EMPRESAOSM_TEXTO_FINO")
    (vla-put-color (vlax-ename->vla-object (entlast)) 256)
    
    (setq contador-auxiliar-de-x (+ contador-auxiliar-de-x 2))
  ) ; repeat
  
) ; defun


; // NOTE - Revisado
(defun escreve-declividade (coordenadas-da-geratriz-inferior linha-base tamanho-do-texto valor-inicial 
                            / quantidade contador-auxiliar-estaca-anterior-de-x contador-auxiliar-estaca-posterior-de-x
                            contador-auxiliar-do-ponto-y-estaca-anterior contador-auxiliar-do-ponto-y-estaca-posterior
                            posicao-y estaca-anterior estaca-posterior distancia estaca-anterior-ponto-y estaca-posterior-ponto-y
                            cota-estaca-anterior cota-estaca-posterior declividade meio-da-estaca)
  "
  Escreve entre dois pontos (a estaca anterior e a estaca atual) a sua declividade
  
  Declividade = (Geratriz inferior atual - Geratriz inferior anterior) / distância
  "
  
  (setq quantidade (- (/ (length coordenadas-da-geratriz-inferior) 2) 1))
  
  (setq contador-auxiliar-estaca-anterior-de-x 0)
  (setq contador-auxiliar-estaca-posterior-de-x 2)
  
  (setq contador-auxiliar-do-ponto-y-estaca-anterior 1)
  (setq contador-auxiliar-do-ponto-y-estaca-posterior 3)
  
  (setq posicao-y (- linha-base (* tamanho-do-texto 81)))
  
  (repeat quantidade
    
    (setq estaca-anterior (nth contador-auxiliar-estaca-anterior-de-x coordenadas-da-geratriz-inferior))
    (setq estaca-posterior (nth contador-auxiliar-estaca-posterior-de-x coordenadas-da-geratriz-inferior))
    
    (setq distancia (distance (list estaca-anterior 0 0) (list estaca-posterior 0 0)))
    
    (setq estaca-anterior-ponto-y (nth contador-auxiliar-do-ponto-y-estaca-anterior coordenadas-da-geratriz-inferior))
    (setq estaca-posterior-ponto-y (nth contador-auxiliar-do-ponto-y-estaca-posterior coordenadas-da-geratriz-inferior))
    
    ; // TODO Calcular a G.I das duas estacas e dividir pela distância
    (setq cota-estaca-anterior (distance (list 0 linha-base 0) (list 0 estaca-anterior-ponto-y 0)))
    (setq cota-estaca-posterior (distance (list 0 linha-base 0) (list 0 estaca-posterior-ponto-y 0)))
    
    
    (setq cota-estaca-anterior (+ (/ cota-estaca-anterior 10) valor-inicial))
    (setq cota-estaca-posterior (+ (/ cota-estaca-posterior 10) valor-inicial))

    
    (if (zerop distancia)
      (progn
        (alert (strcat "ALERTA! Detectado dois pontos no mesmo local na " (rtos (/ contador-auxiliar-estaca-posterior-de-x 2) 2 0) "a estaca/linha (contagem de 1 ate n). Setaremos o valor de declividade para 0"))
        (setq declividade 0)
      )
      (setq declividade (rtos (abs (/ (- cota-estaca-posterior cota-estaca-anterior) distancia)) 2 4))
    )
    
    (setq meio-da-estaca (/ (+ estaca-anterior estaca-posterior) 2))
    
    (vla-put-layer (vla-addtext model_space declividade (vlax-3d-point meio-da-estaca posicao-y 0) tamanho-do-texto) "EMPRESAOSM_TEXTO_FINO")
    (vla-put-color (vlax-ename->vla-object (entlast)) 256)
  
    (setq contador-auxiliar-estaca-anterior-de-x (+ contador-auxiliar-estaca-anterior-de-x 2))
    (setq contador-auxiliar-estaca-posterior-de-x (+ contador-auxiliar-estaca-anterior-de-x 2))
    (setq contador-auxiliar-do-ponto-y-estaca-anterior (+ contador-auxiliar-do-ponto-y-estaca-anterior 2))
    (setq contador-auxiliar-do-ponto-y-estaca-posterior (+ contador-auxiliar-do-ponto-y-estaca-posterior 2))
  ) ; repeat
  
) ; defun


; // NOTE - Revisado
(defun escreve-extensao (coordenadas-da-geratriz-inferior linha-base tamanho-do-texto
                         / contador-auxiliar-estaca-anterior contador-auxiliar-estaca-posterior posicao-y repeticoes
                         estaca-anterior estaca-posterior extensao posicao-x)
  "
  Escreve entre dois pontos (a estaca anterior e a estaca atual) a extensão entre a 
  estaca atual e a estaca anterior
  "

  (setq contador-auxiliar-estaca-anterior 0)
  (setq contador-auxiliar-estaca-posterior 2)
  (setq posicao-y (- linha-base (* tamanho-do-texto 71)))
  
  (setq repeticoes (+ (/ (length coordenadas-da-geratriz-inferior) 2) 1))
  
  (repeat (- repeticoes 2)
    
    (setq estaca-anterior (nth contador-auxiliar-estaca-anterior coordenadas-da-geratriz-inferior))
    (setq estaca-posterior (nth contador-auxiliar-estaca-posterior coordenadas-da-geratriz-inferior))
    
    
    (setq extensao (rtos (distance (list estaca-anterior 0 0) (list estaca-posterior 0 0)) 2 2))
 
    
    (setq posicao-x (/(+ estaca-anterior estaca-posterior) 2))
  
    (vla-put-layer (vla-addtext model_space (strcat extensao "m") (vlax-3d-point posicao-x posicao-y) tamanho-do-texto) "EMPRESAOSM_TEXTO_FINO")
    (vla-put-color (vlax-ename->vla-object (entlast)) 256)
    
    (setq contador-auxiliar-estaca-anterior (+ contador-auxiliar-estaca-anterior 2))
    (setq contador-auxiliar-estaca-posterior (+ contador-auxiliar-estaca-posterior 2))
  ) ; repeat
  
) ; defun


(defun pega-as-coordenadas-das-linhas (linhas 
                                       / lista-coordenadas quantidade entidade objeto xref yref )
  """
  Essa função se encarregara de pegar todas as linhas ligadas ao nivel do terreno separar suas coordenas X Y
  e retornar a mesma.
  """
  
  
  (setq lista-coordenadas (list))
  
  (setq quantidade (sslength linhas))
  
  (setq i 0)
  
  (repeat quantidade
    (setq entidade (ssname linhas i))
    
    (setq objeto (entget entidade))
  
    (setq xref (cadr (assoc 10 objeto)))
    (setq yref (caddr (assoc 10 objeto)))  
    
    (setq lista-coordenadas (append lista-coordenadas (list xref yref)))
    
    (setq i (1+ i)) 
  ) ; repeat
  
  lista-coordenadas

) ; defun


; // NOTE - Revisado
(defun escreve-profundidade (coordenadas valor-inicial coordenada-y-da-linha-base tamanho-do-texto coordenadas-geratriz-inferior
                             / texto_atual contador-auxiliar-de-x contador-auxiliar-de-y posicao-y ponto-y-da-linha-terreno
                             ponto-y-endpoint-geratriz-inferior estaca-atual cota-do-terreno cota-geratriz-inferior profundidade)
  
  
  
  (setq contador-auxiliar-de-x 0)
  (setq contador-auxiliar-de-y 1)
  
  (setq posicao-y (- coordenada-y-da-linha-base (* tamanho-do-texto 62)))
  
  (repeat (/ (length coordenadas) 2)
    
    (setq ponto-y-da-linha-terreno (nth contador-auxiliar-de-x coordenadas))
    (setq ponto-y-endpoint-geratriz-inferior (nth contador-auxiliar-de-y coordenadas-geratriz-inferior))
    
    (setq estaca-atual (nth contador-auxiliar-de-y coordenadas))
    
    (setq cota-do-terreno (distance (list 0 coordenada-y-da-linha-base 0) (list 0 ponto-y-da-linha-terreno 0))) ; Pegamos primeiro a distancia do piso até a linha
    (setq cota-do-terreno (+ (/ cota-do-terreno 10) valor-inicial)) ; Depois dividimos por dez e acrescentamos o número inicial
    
    
    (setq cota-geratriz-inferior (distance (list 0 coordenada-y-da-linha-base 0) (list 0 ponto-y-endpoint-geratriz-inferior 0)))
    (setq cota-geratriz-inferior (+ (/ cota-geratriz-inferior 10) valor-inicial))
    
    
    (setq profundidade (rtos (- cota-do-terreno cota-geratriz-inferior) 2 3))
    
    (setq texto_atual (vla-addtext model_space profundidade (vlax-3d-point estaca-atual posicao-y 0) tamanho-do-texto))
    (vla-put-rotation texto_atual 1.5708)
    (vla-put-layer texto_atual "EMPRESAOSM_TEXTO_FINO")
    (vla-put-color (vlax-ename->vla-object (entlast)) 256)
    
    (setq contador-auxiliar-de-x (+ contador-auxiliar-de-x 2))
    (setq contador-auxiliar-de-y (+ contador-auxiliar-de-y 2))
  
  ) ; repeat

) ; defun

; // VERSÃO 3.1
(defun desbloqueia_layer ( status / layer)
  
  ;; Criar lista do zero
  (if status
    (progn
      (setq layers_trocadas (list))
  
      (setq layer (tblnext "layer" "rewind"))
      
      (while layer
        (if (equal 4 (logand 4 (cdr (assoc 70 layer))))
          (progn
            (command "_-layer" "_unlock" (cdr (assoc 2 layer)) "")
            (setq layers_trocadas (append layers_trocadas (list (cdr (assoc 2 layer))))) ;; Salve apenas o nome da layer
          )
        )
        
        (setq layer (tblnext "layer"))
      )
    )
    
    ;; Bloqueia as layers novamente
    (progn
      (foreach layer layers_trocadas
        (command "_-layer" "_lock" layer "")
      )
    )
  )
) ; defun


(defun muda_visibilidade ( selecao visibilidade 
                          / contador-dos-objeto vla-objeto)

  
  (repeat (setq contador-dos-objetos (sslength selecao))
    (setq vla-objeto (ssname selecao (setq contador-dos-objetos (1- contador-dos-objetos))))
    (setq vla-objeto (vlax-ename->vla-object vla-objeto))
    
    (if (eq visibilidade "True")
      (vla-put-visible vla-objeto :vlax-true)
      (vla-put-visible vla-objeto :vlax-false)
    )    
  )
) ; defun


(defun extende_linhas (linhas 
                       / quantidade_de_linhas linha_atual selecao vlaobj_old startpoint_old linha_atual vlaobj_new)
  """
  Função que recebe uma lista de linhas e faz seu extend de acordo com as coordenadas inicias
  """
  
  (setq selecao (ssget "_x" 
                  '((-4 . "<not")
                  (8 . "EMPRESAOSM_TERRENO")
                  (-4 . "not>")
                  (-4 . "<not")
                  (8 . "EMPRESAOSM_LINHA_ESTACA")
                  (-4 . "not>"))
                 )
   )
  
  (desbloqueia_layer t)
  
  (muda_visibilidade selecao "False")
  
  (repeat (setq quantidade_de_linhas (sslength linhas))

    (setq linha_atual (ssname linhas (setq quantidade_de_linhas (1- quantidade_de_linhas))))
    
    (setq vlaobj_old (vlax-ename->vla-object linha_atual))
    (setq startpoint_old (vlax-get vlaobj_old 'Startpoint))
    
    (setq linha_atual (append (list linha_atual) (list startpoint_old)))
    
    (command "_extend" "" linha_atual "")
    
    (setq vlaobj_new (vlax-ename->vla-object (car linha_atual)))
    
    (if (equal startpoint_old (vlax-get vlaobj_new 'StartPoint))
      (command "_trim" "" linha_atual "")
    ) 
    
  ) ; repeat

  (muda_visibilidade selecao "True")
  (desbloqueia_layer nil)
  
) ; defun

; // SECTION - Funções principais
(defun c:GerarPerfil( / *error* terreno_pl distancia-digitada coordenadas-geratriz-inferior coordenada-y-da-linha-base valor-inicial tamanho-do-texto linhas coordenadas)
  
  
  ; // SECTION Error handling 
  (defun *error* (msg)
    (vla-endundomark doc)
    
    (or 
      (wcmatch (strcase msg) "*BREAK, *CANCEL*, *EXIT*") 
      (alert (strcat "ERROR: " msg "**"))
    )

    (setvar 'osmode original_osmode)
  )
  
  (VL-LOAD-COM)
  
  (criaLayersNecessarias)
  
  (vla-startundomark doc)  

  (setq original_osmode (getvar 'osmode))
  
  (setvar 'osmode 0)
  
  (setq coordenadas-geratriz-inferior (car (entsel "Selecione a geratriz inferior")))
  
  (setq terreno_pl (car (entsel "Selecione o terreno: ")))  
  
  (setq coordenada-y-da-linha-base (caddr (assoc 10 (entget (car (entsel "Selecione a base do perfil"))))))
    
  (setq valor-inicial (getint "Digite o valor inicial da cota do terreno: "))
  (setq tamanho-do-texto (getreal "Digite o tamanho do texto: "))
  
  (setq distancia-digitada (getreal "Aumentar o numero da estaca a cada (m)? Precisao maxima recomendada: 0.00: "))
  
  (setq bootTime (getvar 'MILLISECS))

  (vla-put-layer (vlax-ename->vla-object terreno_pl) "EMPRESAOSM_TERRENO")
  
  (command "_zoom" "_o" coordenadas-geratriz-inferior terreno_pl "")  
  
  (setq coordenadas-geratriz-inferior (vlax-get (vlax-ename->vla-object coordenadas-geratriz-inferior) 'Coordinates))
  
  (princ "Desenhando \"EMPRESAOSM_LINHA_ESTACAS\"... ")
  (setq milliseconds (getvar 'MILLISECS))

  (desenha-linha-estaca coordenadas-geratriz-inferior coordenada-y-da-linha-base)
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  
  (setq linhas (ssget "A"
                '((0 . "line") (8 . "EMPRESAOSM_LINHA_ESTACA"))
               ) ; ssget
  ) ; setq
  
  
  (princ "Gerando numero das estacas... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-numero-da-estaca 
    coordenadas-geratriz-inferior 
    coordenada-y-da-linha-base 
    tamanho-do-texto
    distancia-digitada
  )
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  
  (princ "Calculando nivel da Geratrix Inferior (G.I)... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-cota
    coordenadas-geratriz-inferior 
    coordenada-y-da-linha-base  
    tamanho-do-texto
    valor-inicial
    (* 40 tamanho-do-texto))
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  
  (princ "Calculando distancia progressiva... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-distancia-progressiva 
    coordenadas-geratriz-inferior 
    coordenada-y-da-linha-base 
    tamanho-do-texto)
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  
  (princ "Calculando distancia entre as estacas... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-extensao 
    coordenadas-geratriz-inferior 
    coordenada-y-da-linha-base 
    tamanho-do-texto)
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  
  (princ "Calculando declividade... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-declividade
    coordenadas-geratriz-inferior
    coordenada-y-da-linha-base 
    tamanho-do-texto
    valor-inicial)
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  

  (princ "Extendendo as linha das estacas (Esse processo pode levar um bom tempo, entao seja paciente)... ")
  (setq milliseconds (getvar 'MILLISECS))
  (extende_linhas linhas)
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  
  (setq coordenadas (pega-as-coordenadas-das-linhas linhas))
  
  (princ "Calculando nivel de terreno... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-cota
    coordenadas 
    coordenada-y-da-linha-base 
    tamanho-do-texto
    valor-inicial
    (* 30 tamanho-do-texto))
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n"))
  
  (setq coordenadas (reverse coordenadas))
 
  (princ "Calculando nivel de profundidade... ")
  (setq milliseconds (getvar 'MILLISECS))
  (escreve-profundidade
    coordenadas
    valor-inicial
    coordenada-y-da-linha-base 
    tamanho-do-texto
    coordenadas-geratriz-inferior)
  (princ (strcat (rtos (- (getvar 'MILLISECS) milliseconds) 2 0) "ms\n\n"))
  
  
  (muda_layer linhas "EMPRESAOSM_LINHA_ESTACAS" "5")

  
  (alert (strcat "Tarefa concluida\n\nTEMPO DE EXECUCAO: " (rtos (- (getvar 'MILLISECS) bootTime) 2 0) "ms"))
  
  (setvar 'osmode original_osmode)
  
  (vla-endundomark doc)
  (princ)
  
) ; defun

(alert "LISP Carregada com sucesso! Digite \"GerarPerfil\" para comecar.")
