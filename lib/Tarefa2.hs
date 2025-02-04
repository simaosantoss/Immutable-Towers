{-|
Module      : Tarefa2
Description : Simulação de um turno do jogo
Copyright   : Gabriel Matos Rodrigues <a110987@alunos.uminho.pt>
              Simão Pedro Ribeiro Santos <a109767@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 2024/25.
-}
module Tarefa2 where 

import LI12425
import Data.Maybe


{-|
=== Alínea 1: Determina os inimigos no alcance de uma torre
-}

-- | Esta função filtra os inimigos que estão dentro do alcance de uma torre
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance torre = filter (estaDentroDoAlcance (posicaoTorre torre) (alcanceTorre torre))

-- | Verifica se um inimigo está dentro do alcance de uma torre
estaDentroDoAlcance :: (Float, Float) -> Float -> Inimigo -> Bool
estaDentroDoAlcance (xT, yT) alcance Inimigo { posicaoInimigo = (xI, yI) } =
    (xT - xI) ** 2 + (yT - yI) ** 2 <= alcance ** 2

{-|
=== Alínea 2: Atualiza o estado de um inimigo atingido por uma torre
Aplica dano ao inimigo e atualiza os projéteis ativos com base nas sinergias descritas
-}

-- | Atualiza o inimigo, após este ser atingido por um projétil de uma torre
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo torre inimigo =
  let novaVida = vidaInimigo inimigo - danoTorre torre                              -- 2.a) Perde vida consoante o dano da torre
      novosProj= atualizaProjeteis (projetilTorre torre) (projeteisInimigo inimigo) -- 2.b) Atualiza lista de projéteis
  in  inimigo { vidaInimigo= max 0 novaVida
              , projeteisInimigo = novosProj
              }

-- | Atualiza a lista de projéteis ativos no inimigo
atualizaProjeteis :: Projetil -> [Projetil] -> [Projetil]
atualizaProjeteis novoProjetil projeteis =
    let combinados = combinaProjeteis novoProjetil projeteis
    in  filter naoCancelados combinados

-- | Combina projéteis com base nas regras das sinergias
combinaProjeteis :: Projetil -> [Projetil] -> [Projetil]
combinaProjeteis novoProjeteil projeteis =
    let (fogo, outros1) = separaProjetil Fogo projeteis
        (gelo, outros2) = separaProjetil Gelo outros1
        (resina, outros) = separaProjetil Resina outros2
    in case tipoProjetil novoProjeteil of
        Fogo
            | isJust gelo -> outros                                           -- 2.b.i) Fogo e Gelo cancelam-se
            | isJust resina -> duplicaDuracaoProj novoProjeteil : outros      
            | otherwise -> somaDuracaoProjetil novoProjeteil fogo ++ outros
        Gelo
            | isJust fogo -> outros                                           -- 2.b.i) Gelo e Fogo cancelam-se
            | otherwise -> somaDuracaoProjetil novoProjeteil gelo ++ outros
        Resina
            | isJust fogo -> duplicaDuracaoProj (head (somaDuracaoProjetil novoProjeteil fogo)) : outros
            | otherwise -> somaDuracaoProjetil novoProjeteil resina ++ outros

-- | Função que separa os projéteis de um determinado tipo de uma lista de projéteis
separaProjetil :: TipoProjetil -> [Projetil] -> (Maybe Projetil, [Projetil])
separaProjetil tipo [] = (Nothing, [])
separaProjetil tipo (p:ps)
    | tipoProjetil p == tipo = (Just p, ps)
    | otherwise = let (proj, resto) = separaProjetil tipo ps in (proj, p : resto)

-- | Soma a duração de projéteis do mesmo tipo
somaDuracaoProjetil :: Projetil -> Maybe Projetil -> [Projetil]
somaDuracaoProjetil proj Nothing = [proj]
somaDuracaoProjetil proj (Just existente) = [existente { duracaoProjetil = somarDuracao (duracaoProjetil proj) (duracaoProjetil existente) }]

-- | Duplica a duração de um projétil
duplicaDuracaoProj :: Projetil -> Projetil
duplicaDuracaoProj projetil =
    projetil { duracaoProjetil = duplicarDuracao (duracaoProjetil projetil) }

-- | Soma duas durações
somarDuracao :: Duracao -> Duracao -> Duracao
somarDuracao (Finita t1) (Finita t2) = Finita (t1 + t2)
somarDuracao _ _ = Infinita

-- | Duplica uma duração
duplicarDuracao :: Duracao -> Duracao
duplicarDuracao (Finita t) = Finita (2 * t)
duplicarDuracao Infinita  = Infinita

-- | Verifica se um projétil nao foi cancelado
naoCancelados :: Projetil -> Bool
naoCancelados projetil = case duracaoProjetil projetil of
                            Finita t -> t > 0
                            Infinita -> True

--------------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------Sinergias aplicadas------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------------


-- | Aplica dano e sinergias de projétils a um inimigo, com base no tipo de projétil da torre
atingeInimigoSinergias :: Torre -> Inimigo -> Inimigo
atingeInimigoSinergias torre inimigo = 
    case tipoProjetil (projetilTorre torre) of
        -- Tipo Fogo: Aplica dano inicial e adiciona um projétil para dano contínuo.
        Fogo ->
            if estaSobGelo inimigo then
                -- Se o inimigo já está sob efeito de Gelo, remove ambos os efeitos.
                inimigo { projeteisInimigo = removeFogoEGelo inimigo }
            else
                let
                    danoInicial = danoTorre torre  -- Dano inicial fixo
                    -- Cria um único projétil de Fogo com a duração especificada pela torre.
                    projeteisFogo = [Projetil Fogo (duracaoProjetil (projetilTorre torre))]
                    -- Atualiza a lista de projétils ativos com o novo projétil de Fogo.
                    novosProjeteis = projeteisFogo ++ projeteisInimigo inimigo
                in
                    inimigo { 
                        vidaInimigo = max 0 (vidaInimigo inimigo - danoInicial),  -- Aplica dano inicial
                        projeteisInimigo = novosProjeteis                         -- Adiciona projétil para dano contínuo
                    }

        -- Tipo Gelo: Adiciona projétil de Gelo ou remove ambos os efeitos se já estiver sob Fogo.
        Gelo ->
            if estaSobFogo inimigo then
                -- Se o inimigo já está sob efeito de Fogo, remove ambos os efeitos.
                inimigo { projeteisInimigo = removeFogoEGelo inimigo }
            else
                let
                    -- Adiciona o projétil de Gelo com a duração especificada pela torre.
                    projeteisGelo = [Projetil Gelo (duracaoProjetil (projetilTorre torre))]
                    novosProjeteis = projeteisGelo ++ projeteisInimigo inimigo
                in
                    inimigo { 
                        projeteisInimigo = novosProjeteis  -- Adiciona projétil de Gelo
                    }

        -- Tipo Resina: Reduz permanentemente a velocidade e adiciona o projétil de Resina.
        Resina ->
            let
                -- Aplica o abrandamento da Resina apenas se ainda não estiver aplicado
                inimigoComResina = if not (estaSobResina inimigo) then
                                        inimigo { velocidadeInimigo = max 0 (velocidadeInimigo inimigo * 0.7) }
                                    else
                                        inimigo

                -- Adiciona o projétil de Resina com a duração especificada pela torre
                novosProjeteis = [Projetil Resina (duracaoProjetil (projetilTorre torre))] ++ projeteisInimigo inimigoComResina

                -- Duplica a duração dos projétils de Fogo ou Gelo, se estiverem ativos
                projeteisDuplicados = 
                    if estaSobFogo inimigoComResina then
                        duplicarDuracaoProjetilTipo Fogo novosProjeteis
                    else
                        novosProjeteis

                projeteisDuplicadosFinal =
                    if estaSobGelo inimigoComResina then
                        duplicarDuracaoProjetilTipo Gelo projeteisDuplicados
                    else
                        projeteisDuplicados
            in
                -- Retorna o inimigo com Resina aplicada e durações duplicadas de Fogo/Gelo, se aplicável
                inimigoComResina { projeteisInimigo = projeteisDuplicadosFinal }

--------------------------------------
-- Funções auxiliares das sinergias --
--------------------------------------

-- | Duplica a duração de projéteis do mesmo tipo
duplicarDuracaoProjetilTipo :: TipoProjetil -> [Projetil] -> [Projetil]
duplicarDuracaoProjetilTipo tipo = map (\p -> if tipoProjetil p == tipo then p { duracaoProjetil = duplicarDuracao (duracaoProjetil p) } else p)

-- | Verifica se o inimigo está sob efeito de Resina
estaSobResina :: Inimigo -> Bool
estaSobResina inimigo = any (\p -> tipoProjetil p == Resina) (projeteisInimigo inimigo)

-- | Verifica se o inimigo está sob efeito de Fogo
estaSobFogo :: Inimigo -> Bool
estaSobFogo inimigo = any (\p -> tipoProjetil p == Fogo) (projeteisInimigo inimigo)

-- | Verifica se o inimigo está sob efeito de Gelo
estaSobGelo :: Inimigo -> Bool
estaSobGelo inimigo = any (\p -> tipoProjetil p == Gelo) (projeteisInimigo inimigo)

-- | Remove todos os projétils de Fogo e Gelo do inimigo
removeFogoEGelo :: Inimigo -> [Projetil]
removeFogoEGelo inimigo = filter (\p -> tipoProjetil p /= Fogo && tipoProjetil p /= Gelo) (projeteisInimigo inimigo)


{-|
=== Alínea 3: Move o próximo inimigo de um portal para a lista de inimigos ativos
Esta função remove o primeiro inimigo da onda ativa do portal e adiciona-o à lista de inimigos ativos.
-}
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal inimigos = 
  case ondasPortal portal of
    (onda:restoOndas)
            -- Se a entrada da onda ainda não aconteceu (entradaOnda > 0), diminui a entradaOnda e mantém as ondas.
            | entradaOnda onda > 0 ->
                (portal { ondasPortal = (onda{ entradaOnda = entradaOnda onda - 1 } : restoOndas) }, inimigos)
            -- Se não há inimigos na onda, remove a onda do portal.
            | null (inimigosOnda onda) ->
                (portal { ondasPortal = restoOndas }, inimigos)
            -- Caso contrário, ativa o primeiro inimigo da onda.
            | otherwise ->
                let
                    inimigoAtivo = head (inimigosOnda onda)
                    restoInimigos = tail (inimigosOnda onda)
                    ondaAtualizada = onda {inimigosOnda = restoInimigos, tempoOnda = cicloOnda onda }
                    novasOndas = if null restoInimigos
                                    then restoOndas 
                                    else (ondaAtualizada : restoOndas)
                in
                    ( portal { ondasPortal = novasOndas }, inimigoAtivo : inimigos)
        -- Não há ondas no portal
    [] -> (portal, inimigos)
    

{-|
=== Alínea 4: Verifica se o jogo terminou
Determina se o jogador venceu ou perdeu o jogo com base nas condições:
- Vitória: Não há inimigos nem ondas, e a base tem vida positiva.
- Derrota: A base tem vida menor ou igual a 0.
-}
terminouJogo :: Jogo -> Bool
terminouJogo jogo = perdeuJogo jogo || ganhouJogo jogo

-- | Verifica se o jogador perdeu (base destruída)
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0

-- | Verifica se o jogador ganhou (todos os inimigos e ondas eliminados)
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo =
    null (inimigosJogo jogo) && all portalSemInimigos (portaisJogo jogo) && vidaBase (baseJogo jogo) > 0

-- | Verifica se um portal não tem inimigos inativos
portalSemInimigos :: Portal -> Bool
portalSemInimigos portal = all (null . inimigosOnda) (ondasPortal portal)
