module Eventos where

import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Desenhar
import Niveis


{-| 
 Módulo responsável pelos eventos do jogo.
 Lida com eventos de entrada do usuário, como cliques do rato (chamamos mouse por conveniencia) e teclas pressionadas,
e atualiza o estado do jogo de acordo com as interações do jogador.
-}


reagirEventos :: Event -> EstadoGloss -> EstadoGloss

-- Setas para mudar a seleção no Menu Inicial
reagirEventos (EventKey (SpecialKey KeyUp) Down _ _) est@(EstadoGloss MenuPrincipal mp imgsJogo opcaoAtual opcaoLevel) =
  let novo = if opcaoAtual == BotaoQuit then BotaoPlay else BotaoPlay
      -- aqui como temos só 2 botoes, "KeyUp" vai sempre BotaoPlay
  in est { opcaoMenu = novo }

reagirEventos (EventKey (SpecialKey KeyDown) Down _ _) est@(EstadoGloss MenuPrincipal mp imgsJogo opcaoAtual opcaoLevel) =
  let novo = if opcaoAtual == BotaoPlay then BotaoQuit else BotaoQuit
      -- aqui, com 2 botoes, "KeyDown" sempre BotaoQuit
  in est { opcaoMenu = novo }

-- Enter para confirmar a escolha no Menu Principal
reagirEventos (EventKey (SpecialKey KeyEnter) Down _ _)
              est@(EstadoGloss MenuPrincipal mp imgsJogo opcaoAtual opcaoLevel) =
  case opcaoAtual of
    BotaoPlay -> 
      -- Transiciona para a Seleção de Level
      est { estadoAtual = SelecaoLevel }
    BotaoQuit ->
      error "Saindo do jogo..."  -- fecha o jogo

-- ESC => sai
reagirEventos (EventKey (SpecialKey KeyEsc) Down _ _) _ =
  error "Saindo do jogo..."

-- ENTER na Seleção de Level (tambem chamamos level por conveniencia)
reagirEventos (EventKey (SpecialKey KeyEnter) Down _ _)
              est@(EstadoGloss SelecaoLevel mp imgsJogo opcaoAtual opcaoLevel) =
  case opcaoLevel of
    Level1 -> iniciarJogoNoLevel Level1 est
    Level2 -> iniciarJogoNoLevel Level2 est
    Level3 -> iniciarJogoNoLevel Level3 est

-- ENTER no GameOver
reagirEventos (EventKey (SpecialKey KeyEnter) Down _ _)
              (EstadoGloss GameOver posMouse oldImgs opcaoMenu opcaoLevel) =
  EstadoGloss MenuPrincipal posMouse oldImgs opcaoMenu opcaoLevel

-- ENTER na Vitória
reagirEventos (EventKey (SpecialKey KeyEnter) Down _ _)
              (EstadoGloss Venceu posMouse oldImgs opcaoMenu opcaoLevel) =
  EstadoGloss MenuPrincipal posMouse oldImgs opcaoMenu opcaoLevel

-- Teclas 1,2,3 => selecionar torres
reagirEventos (EventKey (Char '1') Down _ _)
              (EstadoGloss (EmJogo jogo) posMouse imgs opcaoMenu opcaoLevel) =
  EstadoGloss (EmJogo (jogo { torreSelecionada = 0 })) posMouse imgs opcaoMenu opcaoLevel

reagirEventos (EventKey (Char '2') Down _ _)
              (EstadoGloss (EmJogo jogo) posMouse imgs opcaoMenu opcaoLevel) =
  EstadoGloss (EmJogo (jogo { torreSelecionada = 1 })) posMouse imgs opcaoMenu opcaoLevel

reagirEventos (EventKey (Char '3') Down _ _)
              (EstadoGloss (EmJogo jogo) posMouse imgs opcaoMenu opcaoLevel) =
  EstadoGloss (EmJogo (jogo { torreSelecionada = 2 })) posMouse imgs opcaoMenu opcaoLevel

-- Clique do rato => comprar torre
reagirEventos (EventKey (MouseButton LeftButton) Down _ (mouseX,mouseY))
              (EstadoGloss (EmJogo jogo) posMouse imgs opcaoMenu opcaoLevel) =
  let jogoAtualizado = construirTorre jogo (mouseX,mouseY)
  in EstadoGloss (EmJogo jogoAtualizado) posMouse imgs opcaoMenu opcaoLevel

-- Movimento do rato
reagirEventos (EventMotion (mx,my))
              (EstadoGloss estApp _ oldImgs opcaoMenu opcaoLevel) =
  EstadoGloss estApp (mx,my) oldImgs opcaoMenu opcaoLevel

-- Setas para mudar a seleção do level
-- Navegar para o nível anterior com a seta para a esquerda
reagirEventos (EventKey (SpecialKey KeyLeft) Down _ _) est@(EstadoGloss SelecaoLevel _ _ _ opcaoLevel) =
  let novo = case opcaoLevel of
               Level1 -> Level3
               Level2 -> Level1
               Level3 -> Level2
  in est { opcaoLevel = novo }

-- Navegar para o próximo nível com a seta para a direita
reagirEventos (EventKey (SpecialKey KeyRight) Down _ _) est@(EstadoGloss SelecaoLevel _ _ _ opcaoLevel) =
  let novo = case opcaoLevel of
               Level1 -> Level2
               Level2 -> Level3
               Level3 -> Level1
  in est { opcaoLevel = novo }

-- Outros eventos não tratados permanecem inalterados
reagirEventos _ estado = estado


-- Função auxiliar para iniciar o jogo no Level selecionado
iniciarJogoNoLevel :: OpcaoLevel -> EstadoGloss -> EstadoGloss
iniciarJogoNoLevel levelSelecionado (EstadoGloss _ posMouse imgs opcaoMenu opcaoLevel) =
    case levelSelecionado of
      Level1 ->  EstadoGloss (EmJogo jogo1) posMouse imgs opcaoMenu opcaoLevel
      Level2 ->  EstadoGloss (EmJogo jogo2) posMouse imgs opcaoMenu opcaoLevel
      Level3 ->  EstadoGloss (EmJogo jogo3) posMouse imgs opcaoMenu opcaoLevel

  
