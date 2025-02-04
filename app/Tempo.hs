module Tempo where
import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3


{-|
 Módulo responsável pela resposta ao tempo decorrido no jogo.
 Atualizar o estado do jogo com base no tempo que passou, incluindo a evolução do jogo 
 e verificação de condições de fim de jogo.
-}

-- | Atualiza o estado do jogo com base no tempo decorrido.
reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo dt (EstadoGloss (EmJogo jogo) mp imgs opcaoMenu opcaoLevel) =
  let jogoAtualizado = atualizaJogo dt jogo
  in if vidaBase (baseJogo jogoAtualizado) <= 0        
       -- Se a vida da base do jogador for zero ou menos, o jogo termina com "Game Over".
       then EstadoGloss GameOver mp imgs opcaoMenu opcaoLevel
       else if terminouJogo jogoAtualizado
            -- Se o jogo foi completado com sucesso, o estado muda para "Venceu".
            then EstadoGloss Venceu mp imgs opcaoMenu opcaoLevel
            --  Caso contrário, o jogo continua com o estado atualizado.
            else EstadoGloss (EmJogo jogoAtualizado) mp imgs opcaoMenu opcaoLevel
-- Qualquer outro estado, permanece inalterado
reageTempo _ estado = estado
