{-|
Module      : Tarefa1
Description : Invariantes do Jogo
Copyright   : Gabriel Matos Rodrigues <a110987@alunos.uminho.pt>
              Simão Pedro Ribeiro Santos <a109767@alunos.uminho.pt>


Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
-}
module Tarefa1 where

import LI12425

-- | Verifica se o estado de um jogo é válido
validaJogo :: Jogo -> Bool
validaJogo jogo = 
    validaPortais jogo &&
    validaInimigos jogo &&
    validaTorres jogo &&
    validaBase jogo

{-|
=== Alínea 1: Valida os portais do jogo, garantindo que seguem todas as regras descritas.
-}

validaPortais :: Jogo -> Bool                        
validaPortais jogo = 
    length portais > 0 &&                               -- 1a. Pelo menos um portal
    portaisSobreTerra portais mapa &&                   -- 1b. Estão sobre terra
    caminhoParaBase portais mapa (posicaoBase base) &&  -- 1c. Caminho para a base
    portaisNaoSobrepostos portais torres base &&        -- 1d. Não estão sobrepostos a torres/base
    umaOndaPorPortal portais                            -- 1e. No máximo uma onda ativa por portal
 where 
    portais = portaisJogo jogo
    mapa = mapaJogo jogo
    torres = torresJogo jogo
    base = baseJogo jogo

------------------------------------------------
-- Funções auxiliares de validação de portais -- 
------------------------------------------------

-- | Verifica se todos os portais estão sobre terrenos do tipo "Terra". (1.b)
portaisSobreTerra :: [Portal] -> Mapa -> Bool             
portaisSobreTerra [] _ = True
portaisSobreTerra (portal:portais) mapa = sobreTerra mapa (posicaoPortal portal) && portaisSobreTerra portais mapa

-- | Verifica se existe um caminho de terra que liga os portais à base. (1.c)
caminhoParaBase :: [Portal] -> Mapa -> Posicao -> Bool                                         
caminhoParaBase [] _ _ = True
caminhoParaBase (portal:portais) mapa posBase =
    existeCaminho (posicaoPortal portal) posBase mapa && caminhoParaBase portais mapa posBase

-- | Verifica se existe um caminho de terra entre duas posições no mapa
existeCaminho :: Posicao -> Posicao -> Mapa -> Bool
existeCaminho inicio destino mapa = 
    posicaoValida mapa [] inicio && posicaoValida mapa [] destino && procuraCaminhoTerra [inicio] [] destino mapa

-- | Procura o caminho de terra existente
procuraCaminhoTerra :: [Posicao] -> [Posicao] -> Posicao -> Mapa -> Bool
procuraCaminhoTerra [] _ _ _ = False                                  -- Caso base : Não há mais posições para explorar
procuraCaminhoTerra (atual:resto) explorados destino mapa
    | atual == destino = True                                         -- Chegou ao destino
    | otherwise = 
        let vizinhosValidos = filter (posicaoValida mapa explorados) (posicoesVizinhas atual)
        in  procuraCaminhoTerra (resto ++ vizinhosValidos) (atual : explorados) destino mapa

-- | Verifica se uma posição é válida para continuar o caminho
posicaoValida :: Mapa -> [Posicao] -> Posicao -> Bool
posicaoValida mapa visitados (x, y) =
    notElem (x, y) visitados && sobreTerra mapa (x, y)

-- | Obtém as posições vizinhas
posicoesVizinhas :: Posicao -> [Posicao]
posicoesVizinhas (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

-- | Verifica se os portais não estão sobrepostos às torres ou à base. (1.d)
portaisNaoSobrepostos :: [Portal] -> [Torre] -> Base -> Bool              
portaisNaoSobrepostos [] _ _ = True
portaisNaoSobrepostos (portal:portais) torres base = 
    naoCoincide torres base (posicaoPortal portal) && portaisNaoSobrepostos portais torres base

-- | Verifica se uma posição não coincide com posições de torres ou posição da base
naoCoincide :: [Torre] -> Base -> Posicao -> Bool
naoCoincide torres base posicao = posicao `notElem` (map posicaoTorre torres ++ [posicaoBase base])

-- | Verifica se todos os portais têm no máximo uma onda ativa. (1.e)
umaOndaPorPortal :: [Portal] -> Bool                                   
umaOndaPorPortal [] = True
umaOndaPorPortal (portal:portais) = umaOndaAtiva portal && umaOndaPorPortal portais

-- | Função que garante haver no máximo uma onda ativa por portal
umaOndaAtiva :: Portal -> Bool
umaOndaAtiva portal = 
    let ondas = ondasPortal portal
    in  contarOndasAtivas ondas <= 1

-- | Função auxiliar para contar as ondas ativas
contarOndasAtivas :: [Onda] -> Int
contarOndasAtivas [] = 0
contarOndasAtivas (onda:ondas)
    | tempoOnda onda > 0 = 1 + contarOndasAtivas ondas    -- Se a onda estiver com tempo positivo, conta como ativa
    | entradaOnda onda <= 0 = 1 + contarOndasAtivas ondas -- Se a onda estiver pronta para iniciar, conta como ativa
    | otherwise = contarOndasAtivas ondas                 -- Caso contrário, não conta


{-|
=== Alínea 2: Valida os inimigos do jogo, garantindo que seguem todas as regras descritas.
-}

validaInimigos :: Jogo -> Bool                                                  
validaInimigos jogo = 
    inimigosPortaisValidos (portaisJogo jogo) &&                                 -- Verifica os inimigos por lançar
    inimigosEmJogoValidos (inimigosJogo jogo) (torresJogo jogo) (mapaJogo jogo)  -- Verifica os inimigos em jogo
  where
    mapa = mapaJogo jogo
    torres = torresJogo jogo

-------------------------------------------------
-- Funções auxiliares de validação de inimigos -- 
-------------------------------------------------

-- | Valida os inimigos que ainda não foram lançados pelos portais. (2.a)
inimigosPortaisValidos :: [Portal] -> Bool
inimigosPortaisValidos [] = True
inimigosPortaisValidos (portal:portais) = 
    todosValidos (inimigosOnda =<< ondasPortal portal) && 
    inimigosPortaisValidos portais
  where
    todosValidos [] = True
    todosValidos (inimigo:inimigos) =
        posicaoInimigo inimigo == posicaoPortal portal &&    -- Verifica se a posição do inimigo (por lançar) está no mesmo portal
        vidaInimigo inimigo > 0 &&                           -- Verifica se a vida do inimigo (por lançar) é positiva
        null (projeteisInimigo inimigo) &&                   -- Verifica se não há projéteis ativos no inimigo (por lançar)
        todosValidos inimigos                                -- Verifica os inimigos restantes (por lançar)

-- | Valida os inimigos que já estão em jogo (2.b, 2.c, 2.d, 2.e)
inimigosEmJogoValidos :: [Inimigo] -> [Torre] -> Mapa -> Bool
inimigosEmJogoValidos [] _ _ = True
inimigosEmJogoValidos (inimigo:inimigos) torres mapa = 
    sobreTerra mapa (posicaoInimigo inimigo) &&                  -- 2.b) Verifica se o inimigo está sobre terra
    posicaoInimigo inimigo `notElem` map posicaoTorre torres &&  -- 2.c) Verifica se o inimigo não está sobreposto a torres
    velocidadeInimigo inimigo >= 0 &&                            -- 2.d) Verifica se a velocidade do inimigo não é negativa
    listaProjeteisValida (projeteisInimigo inimigo) &&           -- 2.e) Verifica se a lista de projéteis do inimigo é válida
    inimigosEmJogoValidos inimigos torres mapa                   -- Verifica os próximos inimigos

-- | Valida a lista de projéteis do inimigo (2.e)
listaProjeteisValida :: [Projetil] -> Bool
listaProjeteisValida projeteis = 
    naoHaMaisDeUmTipoDeProjetil projeteis &&            -- 2.e.i)  Verifica se não há mais de um projétil do mesmo tipo
    naoConflito projeteis                               -- 2.e.ii) Verifica se não há conflitos entre projéteis de Fogo + Resina ou Fogo + Gelo

-- | Função que verifica se há mais de um projétil do mesmo tipo
naoHaMaisDeUmTipoDeProjetil :: [Projetil] -> Bool
naoHaMaisDeUmTipoDeProjetil [] = True
naoHaMaisDeUmTipoDeProjetil (projetil:ps) = 
    not (projetilRepetido projetil ps) && naoHaMaisDeUmTipoDeProjetil ps
  where
    projetilRepetido projetil ps = any (\x -> tipoProjetil x == tipoProjetil projetil) ps

-- | Função que verifica se há conflito entre projéteis do tipo Fogo e Resina ou Fogo e Gelo
naoConflito :: [Projetil] -> Bool
naoConflito projeteis = 
    not (temConflito Fogo Resina projeteis || temConflito Fogo Gelo projeteis)
  where
    temConflito projetil1 projetil2 ps = 
        any (\projetil -> tipoProjetil projetil == projetil1) ps && 
        any (\projetil -> tipoProjetil projetil == projetil2) ps


{-|
=== Alínea 3: Valida as torres do jogo, garantindo que seguem todas as regras descritas.
-}
                                                    
validaTorres :: Jogo -> Bool
validaTorres jogo = todasTorresValidas torres mapa base
  where mapa = mapaJogo jogo
        torres = torresJogo jogo
        base = baseJogo jogo

-- | Verifica se todas as torres são válidas
todasTorresValidas :: [Torre] -> Mapa -> Base -> Bool
todasTorresValidas [] _ _ = True
todasTorresValidas (torre:torres) mapa base = 
    sobreRelva mapa (posicaoTorre torre) &&          -- 3.a) A torre deve estar sobre Relva
    alcanceTorre torre > 0 &&                        -- 3.b) A torre deve ter alcance positivo
    rajadaTorre torre > 0 &&                         -- 3.c) A torre deve ter rajada positiva
    cicloTorre torre > 0 &&                          -- 3.d) A torre deve ter ciclo positivo
    naoCoincide torres base (posicaoTorre torre) &&  -- 3.e) A torre não pode estar sobreposta a outras torres ou à base
    todasTorresValidas torres mapa base              -- Valida as torres restantes

{-|
=== Alínea 4: Valida a base do jogo, garantindo que segue todas as regras descritas.
-}
                                                
validaBase :: Jogo -> Bool
validaBase jogo = 
    sobreTerra mapa (posicaoBase base) &&                  -- 4.a) A base deve estar colocada sobre terra
    creditosBase base >= 0 &&                              -- 4.b) A base não pode ter crédito negativo
    notElem (posicaoBase base) (map posicaoTorre torres)   -- 4.c) A base não pode estar sobreposta a uma torre ou portal
  where
    base = baseJogo jogo
    mapa = mapaJogo jogo 
    torres = torresJogo jogo



-------------------------------
-- Funções auxiliares gerais --
-------------------------------

-- | Verifica se a posição está sobre terra
sobreTerra :: Mapa -> Posicao -> Bool                                       
sobreTerra mapa (x, y) = case posicaoTerreno mapa (x, y) of
    Just Terra -> True
    _          -> False

-- | Verifica se a posição está sobre relva
sobreRelva :: Mapa -> Posicao -> Bool
sobreRelva mapa (x, y) = case posicaoTerreno mapa (x, y) of
    Just Relva -> True
    _          -> False

-- | Verifica o tipo de terreno numa posição
posicaoTerreno :: Mapa -> Posicao -> Maybe Terreno
posicaoTerreno mapa (x, y) 
    | x >= 0 && y >= 0 && y < fromIntegral (length mapa) && x < fromIntegral (length (head mapa)) = Just ((mapa !! floor y) !! floor x)
    | otherwise = Nothing

