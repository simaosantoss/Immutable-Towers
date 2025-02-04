module ImmutableTowers where
import LI12425
import Graphics.Gloss


-- | Módulo que contém as definições de tipos e estruturas de dados usadas para representar o estado do jogo 
-- e todas as imagens usadas.


-- | Define os diferentes estados possíveis do jogo.
-- Pode estar no menu principal, seleção de nível, jogo em andamento, ecrã de game over ou ecrâ de vitória.
data EstadoApp
  = MenuPrincipal
  | SelecaoLevel
  | EmJogo Jogo
  | GameOver
  | Venceu
  deriving (Eq, Show)

-- | Define as opções do menu principal.
-- Inclui botões como "Play" e "Quit".
data OpcaoMenu = BotaoPlay | BotaoQuit
  deriving (Eq, Show)

-- | Define as opções de seleção de nível.
-- Os níveis disponíveis são Level1, Level2 e Level3.
data OpcaoLevel = Level1 | Level2 | Level3
  deriving (Eq, Show)

-- | Estrutura de dados para armazenar imagens dos diferentes tipos de terreno.
data ImagensTerreno = ImagensTerreno
  { imgRelva :: Picture  -- Imagem da relva.
  , imgTerra :: Picture  -- Imagem da terra.
  , imgAgua  :: Picture  -- Imagem da água.
  }

-- | Estrutura de dados para armazenar imagens das torres disponíveis no jogo.
data ImagensTorres = ImagensTorres
  { imgTorreFogo   :: Picture  -- Imagem da torre de fogo.
  , imgTorreGelo   :: Picture  -- Imagem da torre de gelo.
  , imgTorreResina :: Picture  -- Imagem da torre de resina.
  }

-- | Estrutura de dados para armazenar a imagem da base do jogador.
data ImagensBase = ImagensBase
  { imgBase :: Picture  -- Imagem da base.  
  }

-- | Estrutura de dados para armazenar imagens dos portais do jogo.
data ImagensPortais = ImagensPortais
  { imgPortal :: Picture  -- Imagem do portal.
  }

-- | Estrutura de dados para armazenar imagens dos inimigos.
data ImagensInimigos = ImagensInimigos
  { imgInimigo :: Picture  -- Imagem dos inimigos.
  }

-- | Estrutura de dados para armazenar imagens de fundo dos diferentes estados do jogo.
data ImagensBackground = ImagensBackground
  { imgMenuBg           :: Picture  -- Imagem de fundo do menu.
  , imgGameOverBg       :: Picture  -- Imagem de fundo da tela de game over.
  , imgVitoriaBg        :: Picture  -- Imagem de fundo da tela de vitória.
  , imgJogoBg           :: Picture  -- Imagem de fundo durante o jogo.
  , imgLevelSelectionBg :: Picture  -- Imagem de fundo da tela de seleção de níveis.
  }

-- | Estrutura de dados para armazenar imagens do menu, incluindo os estados normal e selecionado dos botões.
data ImagensMenu = ImagensMenu
  { botaoPlayNormal      :: Picture  -- Imagem do botão "Play" em estado normal.
  , botaoPlaySelecionado :: Picture  -- Imagem do botão "Play" quando selecionado.
  , botaoQuitNormal      :: Picture  -- Imagem do botão "Quit" em estado normal.
  , botaoQuitSelecionado :: Picture  -- Imagem do botão "Quit" quando selecionado.
  }

-- | Estrutura de dados para armazenar imagens do HUD.
data ImagensHUD = ImagensHUD
  { hudImgHP     :: Picture  -- Imagem que representa a vida do jogador.
  , hudImgCred   :: Picture  -- Imagem que representa créditos ou pontos.
  , hudImgInims  :: Picture  -- Imagem que representa o número de inimigos restantes.
  , hudImgOndas  :: Picture  -- Imagem que representa as ondas de inimigos.
  }

-- | Estrutura de dados para armazenar imagens de seleção de nível, incluindo estados normal (não selecionado) e após seleção.
data ImagensLevels = ImagensLevels
  { level1Normal      :: Picture  -- Imagem do nível 1 em estado normal.
  , level1Selecionado :: Picture  -- Imagem do nível 1 quando selecionado.
  , level2Normal      :: Picture  -- Imagem do nível 2 em estado normal.
  , level2Selecionado :: Picture  -- Imagem do nível 2 quando selecionado.
  , level3Normal      :: Picture  -- Imagem do nível 3 em estado normal.
  , level3Selecionado :: Picture  -- Imagem do nível 3 quando selecionado.
  }

-- | Estrutura de dados que reúne todas as imagens do jogo.
-- Inclui terrenos, torres, base, portais, fundo, menu, HUD, níveis e inimigos.
data ImagensJogo = ImagensJogo
  { terreno      :: ImagensTerreno     -- Imagens dos terrenos.
  , torres       :: ImagensTorres      -- Imagens das torres.
  , baseImg      :: ImagensBase        -- Imagem da base.
  , portaisImg   :: ImagensPortais     -- Imagem dos portais.
  , background   :: ImagensBackground  -- Imagens de fundo.
  , menuImgs     :: ImagensMenu        -- Imagens do menu.
  , hudImgs      :: ImagensHUD         -- Imagens do HUD.
  , levelImgs    :: ImagensLevels      -- Imagens dos níveis.
  , inimigoImg   :: ImagensInimigos    -- Imagens dos inimigos.
  }

-- | Estrutura que representa o estado completo do jogo a nível gráfico.
-- Inclui o estado atual do jogo, a posição do mouse, e todas as imagens do jogo.
data EstadoGloss = EstadoGloss
  { estadoAtual  :: EstadoApp       -- O estado atual do jogo.
  , mousePosicao :: (Float, Float)  -- A posição atual do mouse.
  , imagens      :: ImagensJogo     -- Todas as imagens usadas no jogo.
  , opcaoMenu    :: OpcaoMenu       -- A opção de menu atualmente selecionada.
  , opcaoLevel   :: OpcaoLevel      -- O nível atualmente selecionado.
  }


