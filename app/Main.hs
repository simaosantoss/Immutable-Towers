module Main where

import Desenhar
import Eventos
import ImmutableTowers
import Tempo
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game  
import Graphics.Gloss.Data.Picture


{-|
 Módulo responsável por iniciar o jogo, configurar a janela de exibição, fazer a renderização 
 de todas as imagens e atualização do estado do jogo.
-}

main :: IO ()
main = do

-- Carrega imagens do terreno:
  relvaPic <- loadBMP "assets/relva.bmp"
  terraPic <- loadBMP "assets/terra.bmp"
  aguaPic  <- loadBMP "assets/agua.bmp"

-- Carrega imagens das torres:
  torreFogoPic   <- loadBMP "assets/torre_fogo1.bmp"
  torreGeloPic   <- loadBMP "assets/torre_gelo1.bmp"
  torreResinaPic <- loadBMP "assets/torre_resina1.bmp"

-- Carrega imagem do portal:
  portalPic   <- loadBMP "assets/portal.bmp"

-- Carrega imagem do portal:
  basePic   <- loadBMP "assets/base.bmp"

 -- Carrega imagem dos inimigos
  inimigosPic <- loadBMP "assets/inimigo1.bmp"

-- Carrega imagens de fundo:
  menuBgPic      <- loadBMP "assets/menu_background.bmp"
  gameOverBgPic  <- loadBMP "assets/gameover_background.bmp"
  vitoriaBgPic   <- loadBMP "assets/vitoria_background.bmp"
  jogoBgPic      <- loadBMP "assets/bgJogo.bmp"
  levelSelectionBgPic <- loadBMP "assets/bgMenuLevels.bmp"

-- Carrega botoes do menu inicial:
  playNormalPic <- loadBMP "assets/play.bmp"
  playSelecionadoPic <- loadBMP "assets/playSelecionado.bmp"
  quitNormalPic <- loadBMP "assets/quit.bmp"
  quitSelecionadoPic <- loadBMP "assets/quitSelecionado.bmp"

  -- Carrega botoes dos níveis:
  level1NormalPic        <- loadBMP "assets/level1Normal.bmp"
  level1SelecionadoPic   <- loadBMP "assets/level1Selecionado.bmp"
  level2NormalPic        <- loadBMP "assets/level2Normal.bmp"
  level2SelecionadoPic   <- loadBMP "assets/level2Selecionado.bmp"
  level3NormalPic        <- loadBMP "assets/level3Normal.bmp"
  level3SelecionadoPic   <- loadBMP "assets/level3Selecionado.bmp"

 -- Carrega as imagens da HUD
  hpPic     <- loadBMP "assets/vida.bmp"       
  credPic   <- loadBMP "assets/creditos.bmp"    
  inimsPic  <- loadBMP "assets/inimigosInfo.bmp"
  ondasPic  <- loadBMP "assets/ondas.bmp"

  -- Monta os pacotes de imagens
  let imgsTerreno = ImagensTerreno
        { imgRelva = relvaPic
        , imgTerra = terraPic
        , imgAgua  = aguaPic
        }

      imgsTorres = ImagensTorres
        { imgTorreFogo   = torreFogoPic
        , imgTorreGelo   = torreGeloPic
        , imgTorreResina = torreResinaPic
        }

      imgsBase = ImagensBase
        { imgBase = basePic
        }

      imgsPortais = ImagensPortais
        { imgPortal = portalPic
        }

      imgsInimigos = ImagensInimigos 
        { imgInimigo = inimigosPic
        }

      imgsBackground = ImagensBackground
        { imgMenuBg     = menuBgPic
        , imgGameOverBg = gameOverBgPic
        , imgVitoriaBg   = vitoriaBgPic
        , imgJogoBg      = jogoBgPic
        , imgLevelSelectionBg = levelSelectionBgPic
        }

      imgsMenu = ImagensMenu
        { botaoPlayNormal       = playNormalPic
        , botaoPlaySelecionado  = playSelecionadoPic
        , botaoQuitNormal       = quitNormalPic
        , botaoQuitSelecionado  = quitSelecionadoPic
        }

      imgsHUD = ImagensHUD
        { hudImgHP    = hpPic
        , hudImgCred  = credPic
        , hudImgInims = inimsPic
        , hudImgOndas = ondasPic
        }

      imgsLevels = ImagensLevels
        { level1Normal      = level1NormalPic
        , level1Selecionado = level1SelecionadoPic
        , level2Normal      = level2NormalPic
        , level2Selecionado = level2SelecionadoPic
        , level3Normal      = level3NormalPic
        , level3Selecionado = level3SelecionadoPic
        }

      imagensJogo = ImagensJogo
        { terreno      = imgsTerreno
        , torres       = imgsTorres
        , baseImg      = imgsBase
        , portaisImg   = imgsPortais
        , background   = imgsBackground
        , menuImgs     = imgsMenu
        , hudImgs      = imgsHUD
        , levelImgs    = imgsLevels
        , inimigoImg   = imgsInimigos
        }

  -- Cria o estado inicial que contem todas as imagens
  let estadoInicial = EstadoGloss
        { estadoAtual  = MenuPrincipal
        , mousePosicao = (0,0)
        , imagens      = imagensJogo
        , opcaoMenu    = BotaoPlay
        , opcaoLevel    = Level1 
        }

  -- Parametros de janela/tempo:
  let janela = InWindow "Immutable Towers" (1920,1080) (100,100)
      fundo  = makeColorI 220 220 255 255
      fr     = 60

  -- Play com o estado inicial
  play janela fundo fr
       estadoInicial
       desenharEstado
       reagirEventos
       reageTempo