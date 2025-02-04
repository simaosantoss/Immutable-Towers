module Desenhar where

import Graphics.Gloss
import ImmutableTowers
import LI12425
import Niveis

{-|
 Módulo responsável por todas as operações de desenho do jogo.
 Contém funções para desenhar diferentes estados do jogo, como o menu, o jogo em andamento,
as telas de vitória e derrota.
-}


-- | Função principal de desenho
desenharEstadoIO :: EstadoGloss -> IO Picture
desenharEstadoIO est = 
  return (desenharEstado est)

-- | Desenha o estado do jogo baseado no estado fornecido.
desenharEstado :: EstadoGloss -> Picture
desenharEstado (EstadoGloss MenuPrincipal _ imgsJogo opcaoMenu _ ) = desenharMenu imgsJogo opcaoMenu
desenharEstado (EstadoGloss SelecaoLevel _ imgsJogo _ opcaoLevel ) = desenharSelecaoLevels imgsJogo opcaoLevel
desenharEstado (EstadoGloss GameOver      _ imgsJogo _ _ ) = desenharGameOver imgsJogo
desenharEstado (EstadoGloss Venceu        _ imgsJogo _ _ ) = desenharVitoria imgsJogo
desenharEstado (EstadoGloss (EmJogo jogo) _ imgsJogo opcao _ ) =
    desenharJogo jogo imgsJogo


---------------------------------------------------------------------------------
---------------------------- Menu Principal -------------------------------------
---------------------------------------------------------------------------------

-- | Função para desenhar o menu
desenharMenu :: ImagensJogo -> OpcaoMenu -> Picture
desenharMenu imgsJogo opcaoSelecionada =
  Pictures
    [ desenharFundo (imgMenuBg (background imgsJogo))
    , desenharBotaoPlay (menuImgs imgsJogo) opcaoSelecionada
    , desenharBotaoQuit (menuImgs imgsJogo) opcaoSelecionada
    ]

-- | Função para desenhar o botão Play
desenharBotaoPlay :: ImagensMenu -> OpcaoMenu -> Picture
desenharBotaoPlay imgsMenu opcaoSelecionada =
  let pic = if opcaoSelecionada == BotaoPlay
            then botaoPlaySelecionado imgsMenu
            else botaoPlayNormal imgsMenu
  in Translate 0 (-200) pic

-- | Função para desenhar o botão Quit
desenharBotaoQuit :: ImagensMenu -> OpcaoMenu -> Picture
desenharBotaoQuit imgsMenu opcaoSelecionada =
  let pic = if opcaoSelecionada == BotaoQuit
            then botaoQuitSelecionado imgsMenu
            else botaoQuitNormal imgsMenu
  in Translate 0 (-350) pic

-- | Função para desenhar um fundo específico
desenharFundo :: Picture -> Picture
desenharFundo img =
  Scale escala escala img
  where
    escala = 1.0  -- Valor ajustável com base no tamanho da imagem e da tela

---------------------------------------------------------------------------------
---------------------- Menu de Seleção de níveis --------------------------------
---------------------------------------------------------------------------------

-- | Função para desenhar a área de seleção de níveis
desenharSelecaoLevels :: ImagensJogo -> OpcaoLevel -> Picture
desenharSelecaoLevels imgsJogo opcaoSelecionada =
  Pictures
    [ desenharFundo (imgLevelSelectionBg (background imgsJogo))  -- Novo fundo
    , desenharLevel imgsJogo Level1 opcaoSelecionada (-500)      -- Botão Esquerda
    , desenharLevel imgsJogo Level2 opcaoSelecionada 0           -- Botão Centro
    , desenharLevel imgsJogo Level3 opcaoSelecionada 500         -- Botão Direita
    ]

-- | Função auxiliar para desenhar cada botão de nível
desenharLevel :: ImagensJogo -> OpcaoLevel -> OpcaoLevel -> Float -> Picture
desenharLevel imgsJogo level opcaoSelecionada x =
  let
    imgsLevel = levelImgs imgsJogo
    pic = case level of
      Level1 -> if opcaoSelecionada == Level1
                then level1Selecionado imgsLevel
                else level1Normal imgsLevel
      Level2 -> if opcaoSelecionada == Level2
                then level2Selecionado imgsLevel
                else level2Normal imgsLevel
      Level3 -> if opcaoSelecionada == Level3
                then level3Selecionado imgsLevel
                else level3Normal imgsLevel
    escala = 2.0
  in Translate x 0 (Scale escala escala pic)  -- Alinha horizontalmente com y = 0

-- | Função para desenhar Game Over com imagem de fundo
desenharGameOver :: ImagensJogo -> Picture
desenharGameOver imgsJogo =
  Pictures
    [ desenharFundo (imgGameOverBg (background imgsJogo))
    ]

-- | Função para desenhar Vitória com imagem de fundo
desenharVitoria :: ImagensJogo -> Picture
desenharVitoria imgsJogo =
  Pictures
    [ desenharFundo (imgVitoriaBg (background imgsJogo))
    ]

----------------------------------------------------------------------------------
--------------------------------- Jogo -------------------------------------------
----------------------------------------------------------------------------------

-- | Função para desenhar o jogo em si 
desenharJogo :: Jogo -> ImagensJogo -> Picture
desenharJogo jogo imgsJogo =
  Pictures
    [ desenharFundo (imgJogoBg (background imgsJogo))
    , desenharMapa (mapaJogo jogo) (terreno imgsJogo)
    , desenharBase (baseJogo jogo) (baseImg imgsJogo)
    , desenharPortais (portaisJogo jogo) (portaisImg imgsJogo)
    , desenharTorres (torresJogo jogo) (torres imgsJogo)
    , desenharInimigos (inimigosJogo jogo) (inimigoImg imgsJogo)
    , desenharLoja (lojaJogo jogo) (creditosBase (baseJogo jogo)) (torreSelecionada jogo) imgsJogo    
    , desenharHUD jogo (hudImgs imgsJogo)
    ]

-- | Largura e altura da janela
larguraTela, alturaTela :: Float
larguraTela = 1920.0
alturaTela = 1080.0

-- Definição um mapa de exemplo (usado como referência global para o cálculo de l), dado que os mapas tem todos o mesmo tamanho
mapaExemplo :: Mapa
mapaExemplo = mapaJogo1

-- | Calcula o tamanho do lado dos blocos com base no mapa e nas dimensões da tela
l :: Float
l = min (larguraTela / fromIntegral (length (head mapaExemplo)))  -- Ajusta para a largura
        (alturaTela / fromIntegral (length mapaExemplo))          -- Ajusta para a altura

-- | Converte a posição de uma célula na matriz/mapa (coluna, linha) para coordenadas cartesianas (x, y)
posicaoParaCoordenadas :: Float -> Float -> (Float, Float)
posicaoParaCoordenadas coluna linha =
  let x = (coluna * l) - (larguraTela / 2) 
      y = (alturaTela / 2) - (linha * l) 
  in (x, y)

-- | Função para desenhar o mapa       
desenharMapa :: Mapa -> ImagensTerreno -> Picture
desenharMapa mapa imgs =
  Pictures
    [ desenharTerreno imgs (fromIntegral coluna + 0.5) (fromIntegral linha + 0.5) (mapa !! linha !! coluna)
    | linha  <- [0..(length mapa - 1)]
    , coluna <- [0..(length (head mapa) - 1)]
    ]

-- | Função para desenhar uma célula do mapa
desenharTerreno :: ImagensTerreno -> Float -> Float -> Terreno -> Picture
desenharTerreno imgs col lin terr =
  let (x,y) = posicaoParaCoordenadas col lin
      picBase = case terr of
        Relva -> imgRelva imgs
        Terra -> imgTerra imgs
        Agua  -> imgAgua  imgs
   in Translate x y picBase

-- | Função para desenhar a base
desenharBase :: Base -> ImagensBase -> Picture
desenharBase base imgsBase =
  let tamanhoTile :: Float
      tamanhoTile = 40                             -- Tamanho de cada tile
      (coluna, linha) = posicaoBase base           -- Posição da base no mapa
      (x, y) = posicaoParaCoordenadas coluna linha -- Converte para coordenadas na tela
      picBase = imgBase imgsBase
      escala = l / 108.0                           -- A imagem da base tem 108x108 pixels
      picFinal = Scale escala escala picBase
  in Translate x y picFinal

-- | Função para desenhar o portal
desenharPortais :: [Portal] -> ImagensPortais -> Picture
desenharPortais portais imgsPortais =
  let tamanhoPortal = l * 0.5 
  in  Pictures $ map (\portal ->
       let (coluna, linha) = posicaoPortal portal
           (x, y) = posicaoParaCoordenadas coluna linha
           picPortal = imgPortal imgsPortais
           escala = l / 108.0               -- A imagem do portal tem 108x108 pixels
           picFinal = Scale escala escala picPortal
       in Translate x y picFinal
    ) portais

-- | Função para desenhar as torres
desenharTorres :: [Torre] -> ImagensTorres -> Picture
desenharTorres torres imgsTorres =
  Pictures (map (desenharUmaTorre imgsTorres) torres)

-- | Função para desenhar uma torre
desenharUmaTorre :: ImagensTorres -> Torre -> Picture
desenharUmaTorre imgsTorres torre =
  let (x, y) = posicaoTorre torre
      (sx, sy) = posicaoParaCoordenadas x y
      picTorre = case tipoProjetil (projetilTorre torre) of
                   Fogo   -> imgTorreFogo   imgsTorres
                   Gelo   -> imgTorreGelo   imgsTorres
                   Resina -> imgTorreResina imgsTorres
  in Translate sx sy picTorre

-- | Função para desenhar os inimigos
desenharInimigos :: [Inimigo] -> ImagensInimigos -> Picture
desenharInimigos inimigos imagemInimigo =
  Pictures $
    map (\inimigo ->
      let (coluna, linha) = posicaoInimigo inimigo
          (x, y) = posicaoParaCoordenadas coluna linha
          pontosDeVida = vidaInimigo inimigo
      in Translate x y $
           Pictures
             [ Scale 0.5 0.5 (imgInimigo imagemInimigo) 
             , Translate (-10) 35 $ Scale 0.12 0.12 (Color black (Text (show (floor pontosDeVida))))
             , Translate (-11) 35 $ Scale 0.12 0.12 (Color black (Text (show (floor pontosDeVida))))
             , Translate (-12) 35 $ Scale 0.12 0.12 (Color black (Text (show (floor pontosDeVida))))
             , Translate (-13) 35 $ Scale 0.12 0.12 (Color black (Text (show (floor pontosDeVida))))
             ]
    ) inimigos

-- | Função para desenhar a loja com imagens das torres e ícone de crédito
desenharLoja :: Loja -> Creditos -> Int -> ImagensJogo -> Picture
desenharLoja loja creditos indiceSel imgsJogo =
  let baseX      = 680
      baseY      = -160
      intervaloY = 130
      imgsTorres = torres imgsJogo
      iconeCred  = hudImgCred (hudImgs imgsJogo)
  in  Translate baseX baseY $
      Pictures
       [ desenharItemLoja (loja !! idx)
                          creditos
                          (idx == indiceSel) 
                          imgsTorres 
                          iconeCred
                          (fromIntegral idx * intervaloY)
       | idx <- [0 .. length loja - 1]
       ]

-- | Função auxiliar que desenha um item da loja
desenharItemLoja :: (Creditos, Torre) -> Creditos -> Bool -> ImagensTorres -> Picture -> Float -> Picture
desenharItemLoja (custo, modeloTorre) cred selecionado imgsTorres picCred offsetY =
  let
    corDisponivel =
      if fromIntegral custo <= cred then green else (makeColorI 78 47 47 100)
    
    -- Destaca se for selecionado
    destaqueSelecionado :: Bool -> Picture
    destaqueSelecionado selecionado
     | selecionado = pictures
      [ Color (makeColorI 78 47 47 100) (Translate (90) (-14) $ Scale 0.25 0.25 $ Text "->"),
        Color (makeColorI 78 47 47 100) (Translate (91) (-15) $ Scale 0.25 0.25 $ Text "->"),
        Color (makeColorI 78 47 47 100) (Translate (91) (-16) $ Scale 0.25 0.25 $ Text "->"),
        Color (makeColorI 78 47 47 100) (Translate (91) (-17) $ Scale 0.25 0.25 $ Text "->"),
        Color (makeColorI 78 47 47 100) (Translate (93) (-16) $ Scale 0.25 0.25 $ Text "->"),
        Color (makeColorI 78 47 47 100) (Translate (93) (-17) $ Scale 0.25 0.25 $ Text "->")
      ]
     | otherwise = Blank

    -- Para saber a imagem da torre, verificamos o tipo de projetil
    imgTorre = case tipoProjetil (projetilTorre modeloTorre) of
                 Fogo   -> imgTorreFogo   imgsTorres
                 Gelo   -> imgTorreGelo   imgsTorres
                 Resina -> imgTorreResina imgsTorres

    -- Montamos a parte do custo com um ícone e número
    escalaIcone   = 0.5
    escalaTexto   = 0.35
    blocoCusto = Pictures
      [ Scale escalaIcone escalaIcone picCred
      , Translate 30 (-10) $
          Scale escalaTexto escalaTexto $
            Color corDisponivel $
              Text (show custo)
      ]
    itemCompleto = Pictures
      [ destaqueSelecionado selecionado
      , Translate 35 0 $ Scale 0.8 0.8 imgTorre
      , Translate 170 (-15) blocoCusto
      , Translate 171 (-15) blocoCusto
      , Translate 172 (-15) blocoCusto
      , Translate 173 (-15) blocoCusto
      , Translate 174 (-15) blocoCusto
      , Translate 175 (-15) blocoCusto
      ]
  in
    Translate 0 (negate offsetY) itemCompleto

-- | Função para desenhar o HUD
desenharHUD :: Jogo -> ImagensHUD -> Picture
desenharHUD jogo imgsHUD =
  let base         = baseJogo jogo
      pontosDeVida = floor (vidaBase base)
      creditos     = creditosBase base
      inims        = length (inimigosJogo jogo)
      ondasRest    = ondasRestantes jogo

      baseX        = 750
      baseY        = 50
      escalaTexto  = 0.35
      corTexto     = makeColorI 78 47 47 100

      -- Espaçamento vertical entre as linhas
      espacY       = 130

      -- Ícones
      iconeHP      = hudImgHP imgsHUD
      iconeCred    = hudImgCred imgsHUD
      iconeInims   = hudImgInims imgsHUD
      iconeOndas   = hudImgOndas imgsHUD

      -- Exemplo de escala do ícone
      iconeEscala  = 0.5

      -- Desenha texto do valor em seguida
      textHP    = Text (show pontosDeVida)
      textCred  = Text (show creditos)
      textInims = Text (show inims)
      textOndas = Text (show ondasRest)

      linhaHP = Pictures
         [ Scale iconeEscala iconeEscala iconeHP
         , Translate 50 (-15) (Scale escalaTexto escalaTexto (Color corTexto textHP))
         , Translate 51 (-15) (Scale escalaTexto escalaTexto (Color corTexto textHP))
         , Translate 52 (-15) (Scale escalaTexto escalaTexto (Color corTexto textHP))
         , Translate 53 (-15) (Scale escalaTexto escalaTexto (Color corTexto textHP))
         , Translate 54 (-15) (Scale escalaTexto escalaTexto (Color corTexto textHP))
         , Translate 55 (-15) (Scale escalaTexto escalaTexto (Color corTexto textHP))

         ]

      linhaCred = Pictures
         [ Scale iconeEscala iconeEscala iconeCred
         , Translate 50 (-15) (Scale escalaTexto escalaTexto (Color corTexto textCred))
         , Translate 51 (-15) (Scale escalaTexto escalaTexto (Color corTexto textCred))
         , Translate 52 (-15) (Scale escalaTexto escalaTexto (Color corTexto textCred))
         , Translate 53 (-15) (Scale escalaTexto escalaTexto (Color corTexto textCred))
         , Translate 54 (-15) (Scale escalaTexto escalaTexto (Color corTexto textCred))
         , Translate 55 (-15) (Scale escalaTexto escalaTexto (Color corTexto textCred))
         ]

      linhaInims = Pictures
         [ Scale iconeEscala iconeEscala iconeInims
         , Translate 50 (-15) (Scale escalaTexto escalaTexto (Color corTexto textInims))
         , Translate 51 (-15) (Scale escalaTexto escalaTexto (Color corTexto textInims))
         , Translate 52 (-15) (Scale escalaTexto escalaTexto (Color corTexto textInims)) 
         , Translate 53 (-15) (Scale escalaTexto escalaTexto (Color corTexto textInims))
         , Translate 54 (-15) (Scale escalaTexto escalaTexto (Color corTexto textInims))
         , Translate 55 (-15) (Scale escalaTexto escalaTexto (Color corTexto textInims))
                 
         ]

      linhaOndas = Pictures
         [ Scale iconeEscala iconeEscala iconeOndas
         , Translate 50 (-15) (Scale escalaTexto escalaTexto (Color corTexto textOndas))
         , Translate 51 (-15) (Scale escalaTexto escalaTexto (Color corTexto textOndas))
         , Translate 52 (-15) (Scale escalaTexto escalaTexto (Color corTexto textOndas))
         , Translate 53 (-15) (Scale escalaTexto escalaTexto (Color corTexto textOndas))
         , Translate 54 (-15) (Scale escalaTexto escalaTexto (Color corTexto textOndas))
         , Translate 55 (-15) (Scale escalaTexto escalaTexto (Color corTexto textOndas))
         ]

  in Translate baseX baseY $
       Pictures
         [ -- HP na linha 1
           Translate 0 (espacY * 3) linhaHP
         , -- CR na linha 2
           Translate 0 (espacY * 2) linhaCred
         , -- Inims na linha 3
           Translate 0 (espacY * 1) linhaInims
         , -- Ondas na linha 4
           Translate 0 (espacY * 0) linhaOndas
         ]

-- | Constroi uma torre de acordo com o clique do mouse colocando sempre a torre exatamente no centro do tile de relva.
construirTorre :: Jogo -> (Float, Float) -> Jogo
construirTorre jogo (mouseX, mouseY) =
  let
      -- Identifica a coluna e linha do tile clicado
      posX              = floor ((mouseX + (larguraTela / 2)) / l)
      posY              = floor ((alturaTela / 2 - mouseY) / l)
  in 
    -- 1) Verifica se a posição (posX, posY) está dentro do mapa
    if not (coordenadaValida (fromIntegral posX) (fromIntegral posY) (mapaJogo jogo))
      then jogo  -- Caso a posicao de clique esteja fora do mapa
      else
        -- 2) Indica qual o terreno do tile
        let terrenoTile = ((mapaJogo jogo) !! posY) !! posX
        in 
          -- 3) Verifica se existe a torre selecionada
          if (torreSelecionada jogo) < 0 || (torreSelecionada jogo) >= length (lojaJogo jogo)
            then jogo
            else
              let (custo, modeloDeTorre) = (lojaJogo jogo) !! (torreSelecionada jogo)
              in 
                -- 4) Verifica se o tile é relva
                if terrenoTile /= Relva
                  then jogo  -- Caso não seja relva
                  else
                    -- 5) Calcula a posição exata (centro) para a colocação da torre
                    let torreX = fromIntegral posX + 0.5
                        torreY = fromIntegral posY + 0.5
                    in 
                      -- 6) Verifica se já existe outra torre no centro desse tile
                      if any (\t -> posicaoTorre t == (torreX, torreY)) (torresJogo jogo)
                        then jogo  -- Case já haja outra torre no mesmo local
                        else
                          -- 7) Verifica se há créditos suficientes para adquirir a torre
                          if fromIntegral custo > creditosBase (baseJogo jogo)
                            then jogo
                            else
                              -- 8) Cria a torre no centro do tile e atualiza o jogo
                              let novaTorre = modeloDeTorre
                                              { posicaoTorre = (torreX, torreY) }
                                  baseAtualizada = (baseJogo jogo)
                                                 { creditosBase = creditosBase (baseJogo jogo) - custo
                                                 }
                              in jogo { baseJogo   = baseAtualizada
                                      , torresJogo = novaTorre : torresJogo jogo
                                      }


-- | Verifica se (coluna, linha) está dentro do mapa
coordenadaValida :: Float -> Float -> Mapa -> Bool
coordenadaValida coluna linha mapa = (floor linha) >= 0 && (floor linha) < length mapa && (floor coluna) >= 0 && (floor coluna) < length (head mapa)
