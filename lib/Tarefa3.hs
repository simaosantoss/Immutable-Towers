{-|
Module      : Tarefa3
Description : Mecânica do Jogo
Copyright   : Gabriel Matos Rodrigues <a110987@alunos.uminho.pt>
              Simão Pedro Ribeiro Santos <a109767@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 2024/25.
-}

module Tarefa3 where

import LI12425

import Tarefa2

-- | Atualiza o estado do jogo
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo t =
    atualizaPortais t
  . atualizaTorres t
  . moveInimigos t
  . atualizaInimigos t
  . colisoesBase 

------------------------------------------------------------------------------------------------------
-------------------------- 3.3.1 Comportamento das torres --------------------------------------------
------------------------------------------------------------------------------------------------------

-- | Atualiza as torres e os seus ataques aos inimigos no jogo.
atualizaTorres :: Tempo -> Jogo -> Jogo
atualizaTorres tempo jogo = 
  jogo { 
    -- Atualiza o estado das torres no jogo.
    torresJogo = novasTorres, 
    -- Atualiza o estado dos inimigos no jogo.
    inimigosJogo = inimigosAtualizados 
  }
  where
    -- Processa todas as torres do jogo e retorna torres atualizadas e inimigos atualizados.
    (novasTorres, inimigosAtualizados) = processaTodasAsTorres tempo (torresJogo jogo) (inimigosJogo jogo)


-- | Processa todas as torres para disparar nos inimigos que estão dentro do alcance.
processaTodasAsTorres :: Tempo -> [Torre] -> [Inimigo] -> ([Torre], [Inimigo])
processaTodasAsTorres _ [] inimigos = 
  ([], inimigos) -- se não há torres, retorna os inimigos sem alterações.
processaTodasAsTorres tempo (torre:resto) inimigos = 
  let 
    -- Atualiza o estado da torre e dos inimigos após a torre disparar.
    (torreAtualizada, inimigosAposDisparo) = disparaTorre torre tempo inimigos 
    -- Processa o resto das torre e os inimigos após o disparo da torre atual.
    (outrasTorresAtualizadas, inimigosFinal) = processaTodasAsTorres tempo resto inimigosAposDisparo 
  in 
    -- Combina a torre atualizada com as outras torres processadas e retorna o estado final dos inimigos.
    (torreAtualizada : outrasTorresAtualizadas, inimigosFinal)


-- | Trata do disparo de uma unica torre, se esta estiver já pronta a disparar.
disparaTorre :: Torre -> Tempo -> [Inimigo] -> (Torre, [Inimigo])
disparaTorre torre tempo inimigos
  | tempoTorre torre > 0 = 
      -- Se a torre está a recarregar, o tempo de recarga vai sendo reduzido, mas a torre nao dispara.
      (torre { tempoTorre = tempoTorre torre - tempo }, inimigos) 
  | otherwise = 
      let 
        -- Identifica os inimigos dentro do alcance da torre.
        inimigosNoAlvo = inimigosNoAlcance torre inimigos 
        -- Seleciona um número limitado de inimigos (baseado em rajadaTorre) para serem atingidos pela torre.
        inimigosAtingidos = take (rajadaTorre torre) inimigosNoAlvo 
        -- Atualiza a lista de inimigos após ser aplicado o dano (ou efeito) da torre.
        inimigosRestantes = atualizaListaDeInimigos torre inimigos inimigosAtingidos 
        -- Recarrega a torre, sendo ajustado o tempo de recarga da torre de acordo com cicloTorre.
        torreRecarregada = torre { tempoTorre = cicloTorre torre } 
      in 
        -- Retorna o novo estado da torre e a lista atualizada de inimigos.
        (torreRecarregada, inimigosRestantes)


-- | Atualiza a lista de inimigos atingidos pelas torres.
atualizaListaDeInimigos :: Torre -> [Inimigo] -> [Inimigo] -> [Inimigo]
atualizaListaDeInimigos torre inimigos inimigosAtingidos =
  let 
    -- Remove da lista original os inimigos que foram atingidos (comparando as suas posições).
    inimigosRestantes = filter (\inim1 -> not (any (\inim2 -> posicaoInimigo inim1 == posicaoInimigo inim2) inimigosAtingidos)) inimigos
    -- Aplica os efeitos (ou dano) específicos da torre nos inimigos que foram atingidos.
    inimigosAtualizados = map (atingeInimigoSinergias torre) inimigosAtingidos 
  in 
    -- Junta os inimigos restantes (os que não foram atingidos atingidos) aos inimigos atualizados (que sofreram dano ou efeito).
    inimigosRestantes ++ inimigosAtualizados

------------------------------------------------------------------------------------------------------
------------------------- 3.3.2 Comportamento dos inimigos -------------------------------------------
------------------------------------------------------------------------------------------------------

-- | Atualiza os inimigos: movimentação, efeitos e interações
atualizaInimigos :: Tempo -> Jogo -> Jogo
atualizaInimigos tempo jogo =
  let 
    -- Processa os inimigos e retorna a nova lista de inimigos, o total de dano causado à base e o total de butim que foi obtido após matar o inimigo
    (inimigosNovos, danoBase, butimTotal) =
        processaInimigos (inimigosJogo jogo) tempo
                         (mapaJogo jogo) (posicaoBase (baseJogo jogo))
    -- Base com dano aplicado (vida reduzida pelo total de dano dos inimigos, ou seja, ataqueInimigo).
    baseComDano    = (baseJogo jogo) { vidaBase = max 0 (vidaBase (baseJogo jogo) - danoBase) }
    -- Base atualizada com o butim que foi acumulado, butimInimigo.
    baseAtualizada = baseComDano { creditosBase = creditosBase baseComDano + round butimTotal }
  in 
    -- Atualiza o jogo com os novos estados dos inimigos e da base.
    jogo { inimigosJogo  = inimigosNovos
         , baseJogo      = baseAtualizada
         }


-- | Processa os inimigos atualizando o movimento deles e os danos sofridos
processaInimigos :: [Inimigo] -> Tempo -> Mapa -> Posicao -> ([Inimigo], Float, Float)
processaInimigos [] _ _ _ = ([], 0, 0)      -- sem inimigos para processar.
processaInimigos (inimigo:resto) tempo mapa posBase =
  let 
    -- Processa o resto dos inimigos
    (inimigosRestantes, danoRestante, butimRestante) = processaInimigos resto tempo mapa posBase
  in case (atualizaInimigo tempo inimigo posBase) of
      -- Se o inimigo chegou à base ou foi derrotado (por uma torre)
       Left (dano, butim) ->
         ( inimigosRestantes        -- Não adiciona o inimigo à nova lista
         , dano + danoRestante      -- Soma o dano causado à base
         , butim + butimRestante    -- Soma o butim obtido
         )
       Right inimigoVivo ->
         -- Se o inimigo ainda está vivo, volta para a lista
         ( inimigoVivo : inimigosRestantes   -- Adiciona o inimigo à lista
         , danoRestante                      -- Dano à base nao muda
         , butimRestante                     -- Butim não muda
         )


-- | Atualiza o estado de um unico inimigo
atualizaInimigo :: Tempo -> Inimigo -> Posicao -> Either (Float, Float) Inimigo
atualizaInimigo tempo inimigo posBase
  | posicaoInimigo inimigo == posBase =
      Left (ataqueInimigo inimigo, 0)  -- Causa dano na base, mas não dá butim
  | novaVida <= 0 =
      Left (0, fromIntegral (butimInimigo inimigo))  -- Dá butim
  | otherwise =
      Right inimigoAtualizado
  where
    -- Atualiza e descarta projéteis expirados
    projeteisOk = atualizaEDescartaProjeteis tempo (projeteisInimigo inimigo)
    -- Vida do inimigo é atualizada sendo aplicado o dano do fogo
    novaVida    = max 0 (vidaInimigo inimigo - (calculaDanoPorFogo tempo projeteisOk))
    -- Estado atualizado do inimigo
    inimigoAtualizado =
      inimigo 
        { vidaInimigo      = novaVida,
          projeteisInimigo = projeteisOk
          -- Não altera diretamente 'velocidadeInimigo', a velocidade é gerenciada na função moveUmInimigo
        }

-- | Calcula o dano contínuo aplicado por projétils de fogo
calculaDanoPorFogo :: Tempo -> [Projetil] -> Float                          
calculaDanoPorFogo tempo projeteis =
    let 
      -- Filtra apenas projétils de Fogo.
      fogoProjeteis = filter ((== Fogo) . tipoProjetil) projeteis         
    in 
      -- Soma o dano contínuo causado por cada projétil de fogo no intervalo de tempo.
      sum (map (\_ -> 5.0 * tempo) fogoProjeteis)

-- | Determina o dano por segundo de um projétil
danoPorSegundo :: Projetil -> Float
danoPorSegundo projetil =
    case tipoProjetil projetil of
        Fogo -> 5                   -- as torres de fogo dão 5 de dano
        _    -> 0                   -- Os outros tipos de projéteis não causam dano contínuo (3.3.2.2 e 3.3.3.3)


-- | Diminui a duração de um projétil e remove aqueles que já expiraram
atualizaEDescartaProjeteis :: Tempo -> [Projetil] -> [Projetil]
atualizaEDescartaProjeteis tempo = 
 -- Filtra projéteis que ainda estão ativos (duração > 0 ou infinita).
    filter (\p -> case duracaoProjetil p of
                    Finita t -> t > 0
                    Infinita -> True
           )
 -- Atualiza a duração de cada projétil.
  . map (\p -> p { duracaoProjetil = decrementaDuracaoDoProjetil tempo (duracaoProjetil p) })

-- | Decrementa o tempo restante de um projétil.
decrementaDuracaoDoProjetil :: Tempo -> Duracao -> Duracao
decrementaDuracaoDoProjetil tempo (Finita t) = Finita (t - tempo)   -- Reduz o tempo restante
decrementaDuracaoDoProjetil _  Infinita      = Infinita             -- Projéteis de duração infinita nao são alterados (resina)


------------------------------------------------------------------------------------------------------
---------------------------- 3.3.3 Comportamento dos portais -----------------------------------------
------------------------------------------------------------------------------------------------------

-- | Atualiza os portais para que sejam lançados inimigos por eles
atualizaPortais :: Float -> Jogo -> Jogo
atualizaPortais tempo jogo =
  let 
    -- Cada portal processa a sua atualização e os inimigos que deve lançar.
    -- unzip divide o resultado de cada processaUmPortal em duas listas: portais atualizados e inimigos lançados.
    (portaisAtualizados, novosInimigos) = unzip $ map (processaUmPortal tempo) (portaisJogo jogo)
  in 
    jogo
      { -- Atualiza a lista de portais com os portais atualizados.
        portaisJogo = portaisAtualizados
        -- Atualiza a lista de inimigos do jogo sendo adicionados os novos inimigos lançados pelos portais.
      , inimigosJogo = inimigosJogo jogo ++ concat novosInimigos
      -- Se não houver inimigos novos lançados e ainda houver ondas restantes, decrementa a quantidade de ondas
      , ondasRestantes = if null novosInimigos && ondasRestantes jogo > 0
                           then ondasRestantes jogo - 1
                           else ondasRestantes jogo
      }


-- | Processa um portal: atualiza as ondas e lança inimigos
processaUmPortal :: Tempo -> Portal -> (Portal, [Inimigo])
processaUmPortal tempo portal =
    let 
      -- Atualiza o tempo das ondas do portal, decrementando o tempo restante de cada uma.
      ondasAtualizadas = atualizaOndas tempo (ondasPortal portal)         
      -- Ativa os inimigos que estão prontos para serem lançados a partir das ondas.
      (portalFinal, inimigosLançados) = ativaInimigos portal ondasAtualizadas 
    in 
      -- Retorna o portal atualizado e os inimigos lançados.
      (portalFinal, inimigosLançados)


-- | Atualiza o tempo das ondas de um portal
atualizaOndas :: Tempo -> [Onda] -> [Onda]
atualizaOndas tempo = 
  -- Para cada onda é decrementado o tempo de entrada e o tempo da onda
  map (\onda -> onda { entradaOnda = max 0 (entradaOnda onda - tempo), tempoOnda = max 0 (tempoOnda onda - tempo) })


-- | Ativa os inimigos prontos para serem lançados
ativaInimigos :: Portal -> [Onda] -> (Portal, [Inimigo])
ativaInimigos portal [] = (portal { ondasPortal = [] }, [])      -- sem ondas restantes, o portal não ativa inimigos.
ativaInimigos portal (onda:restoOndas)
  -- O inimigo ainda não pode ser ativado (a onda ainda não está pronta, entradaOnda > 0)
  | entradaOnda onda > 0 = 
      let      
        -- Se a entrada da onda ainda não passou, continua para a próxima onda.
        (portalAtualizado, inimigosRestantes) = ativaInimigos portal restoOndas
      in
        -- Retorna o portal atualizado e os inimigos restantes (nenhum foi ativado ainda).
        (portalAtualizado { ondasPortal = onda : ondasPortal portalAtualizado }, inimigosRestantes)

 -- A onda está pronta para ativar inimigos.
  | tempoOnda onda <= 0 && not (null (inimigosOnda onda)) =
      let 
       -- Lista de inimigos da onda
        (inimigo:restoInimigos) = inimigosOnda onda
        -- Atualiza a onda com o inimigo removido e dá reset do tempo da onda (cicloOnda).
        ondaAtualizada = onda { inimigosOnda = restoInimigos, tempoOnda = cicloOnda onda }
        -- Processa as ondas restantes do portal.
        (portalAtualizado, outrosInimigos) = ativaInimigos portal restoOndas
      in  
       -- Retorna o portal atualizado com a onda modificada e o inimigo lançado.
       (portalAtualizado { ondasPortal = ondaAtualizada : ondasPortal portalAtualizado }, inimigo : outrosInimigos)

 -- A onda não está pronta para ativar inimigos (mas ainda existem ondas para processar).
  | otherwise =
      let       
       -- Processa as ondas restantes
       (portalAtualizado, inimigosRestantes) = ativaInimigos portal restoOndas
      in      
       -- Retorna o portal atualizado com a onda atualizada (sem alteração de inimigos).
       ( portalAtualizado { ondasPortal = onda : ondasPortal portalAtualizado }
       , inimigosRestantes
       )


-- O que vem abaixo não foi pedido para fazer na tarefa mas consideramos que este modulo seria o melhor
-- para colocar este código, dado que vai ser usado na atualizaJogo

------------------------------------------------------------------------------------------------------
------------------------- Movimentações dos inimigos -------------------------------------------------
------------------------------------------------------------------------------------------------------

-- | Definição dos caminhos para cada mapa (do portal à base)

-- | Caminho Mapa1
pathMapa1 :: [(Int, Int)]
pathMapa1 =
  [ (0,0), (0,1), (0,2), (1,2), (2,2), (3,2), (4,2), (5,2),
    (6,2), (7,2), (8,2), (9,2), (10,2), (11,2), (12,2), (13,2),
    (14,2), (14,3), (14,4), (14,5), (14,6), (14,7), (14,8), (14,9) ]

-- | Caminho Mapa2
pathMapa2 :: [(Int, Int)]
pathMapa2 =
  [ (0,0), (1,0), (2,0), (2,1), (2,2), (3,2), (4,2), (4,3),
    (4,4), (5,4), (6,4), (7,4), (7,5), (7,6), (7,7), (8,7),
    (9,7), (10,7), (10,8), (10,9), (11,9), (12,9), (13,9), (14,9) ]

-- | Caminho Mapa3
pathMapa3 :: [(Int, Int)]
pathMapa3 =
  [ (0,0), (1,0), (1,1), (1,2), (1,3), (2,3), (3,3), (4,3),
    (4,4), (4,5), (5,5), (6,5), (7,5), (7,6), (7,7), (8,7),
    (9,7), (10,7), (10,8), (10,9), (11,9), (12,9), (13,9), (14,9) ]


-- | Move um inimigo no mapa, considerando os efeitos dos projéteis ativos.
moveUmInimigo :: Float -> [(Int, Int)] -> Inimigo -> Inimigo
moveUmInimigo tempo caminho inimigo =
  let  
    -- Índice atual no caminho que o inimigo está a seguir.
    indiceAtual = pathIndex inimigo
    -- Calcula a velocidade: 0 se houver projétil de Gelo ativo ou a velocidade original (se não houver gelo)
    velocidadeEfetiva = if any (\p -> tipoProjetil p == Gelo) (projeteisInimigo inimigo) 
                          then 0 
                          else velocidadeInimigo inimigo
  in 
    -- Verifica se o inimigo já chegou ao final do caminho
    if indiceAtual >= length caminho - 1
      then inimigo  -- O inimigo chegou ao destino, não é necessário ser movido mais
      else
        let 
           -- Posição atual do inimigo.
           (posXAtual, posYAtual) = posicaoInimigo inimigo

           -- Próxima célula do caminho que o inimigo deve alcançar.
           (proxX, proxY) = caminho!! (indiceAtual + 1)

           -- Coordenadas do centro da próxima célula. (isto foi colocado para ser mais rapido fazer os caminhos e está no let para ser mais "visível")
           coordXCentro = fromIntegral proxX + 0.5
           coordYCentro = fromIntegral proxY + 0.5

           -- Diferença entre a posição atual e a posição da próxima célula.
           difX = coordXCentro - posXAtual
           difY = coordYCentro - posYAtual

           -- Distância entre a posição atual e a próxima célula.
           distancia = sqrt (difX * difX + difY * difY)

           -- Distância máxima que o inimigo pode percorrer nesse intervalo de tempo.
           distanciaMaxima = velocidadeEfetiva * tempo

        in
          -- Verifica se o inimigo pode alcançar a próxima célula.
           if distancia <= distanciaMaxima
              then 
                -- O inimigo alcançou a próxima célula, logo atualiza a posição e aumenta o índice.
                inimigo { 
                  posicaoInimigo = (coordXCentro, coordYCentro), -- Atualiza para o centro da célula.
                  pathIndex = indiceAtual + 1                    -- Avança para o próximo ponto no caminho.
                }
            else
              -- Proporção da distância máxima em relação à distância total.
              let proporcao = if distancia == 0 then 0 else distanciaMaxima / distancia 
              in 
                inimigo { 
                    posicaoInimigo = (posXAtual + difX * proporcao, 
                                      posYAtual + difY * proporcao) -- Atualiza a posição parcial do inimigo.
                }

-- | Moves os inimgios no jogo com base no tempo decorrido
moveInimigos :: Float -> Jogo -> Jogo
moveInimigos tempo jogo =
  let
    inimigosMovimentados = map (moveUmInimigo tempo (caminhoJogo jogo)) (inimigosJogo jogo)
    inimigosVivos = filter (\inimigo -> vidaInimigo inimigo > 0) inimigosMovimentados
  in 
    jogo { inimigosJogo = inimigosVivos }


------------------------------------------------------------------------------------------------------
--------------------------------- Comportamento da base ----------------------------------------------
------------------------------------------------------------------------------------------------------

-- | Deteta e processa colisões entre os inimigos e a base
colisoesBase :: Jogo -> Jogo
colisoesBase jogo =
  let
    -- Usa o foldr para acumular: Os inimigos que não colidiram (restantes) e o dano total causado pelos inimigos que colidiram.
      (restantes, danoBase)= foldr (\inimigo (acumulado,dano)->
                        let  
                            -- Posições do inimigo e da base no jogo
                            (posicaoXInimigo,posicaoYInimigo)= posicaoInimigo inimigo
                            (posicaoXBase,posicaoYBase)= posicaoBase (baseJogo jogo)
                            -- Calcula a distância entre o inimigo e o centro da base.
                            distancia= sqrt((posicaoXInimigo- (posicaoXBase+ 0.5))^2 + (posicaoYInimigo- (posicaoYBase+ 0.5))^2)
                        in 
                            -- Verifica se o inimigo está próximo o suficiente para colidir com a base.
                            if distancia<0.3
                             -- Caso haja colisão, não adiciona o inimigo à lista acumulada e aumenta o dano causado à base pelo ataque desse inimigo.
                             then (acumulado, dano+ ataqueInimigo inimigo)
                             -- Caso contrário, mantém o inimigo na lista acumulada.
                             else (inimigo:acumulado, dano)
                      ) ([],0) (inimigosJogo jogo)
     -- Atualiza a base, reduzindo da vida da base o dano acumulado (achamos que ficou mais compreensivel estando no let)
      baseAtualizada= (baseJogo jogo) { vidaBase= vidaBase (baseJogo jogo) - danoBase}
  in 
    -- Retorna o jogo atualizado, ou seja, a base com a vida reduzida e a lista de inimigos filtrada (apenas os que não colidiram)
    jogo{ baseJogo= baseAtualizada, inimigosJogo= restantes }
