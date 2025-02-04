module Tarefa1Spec where

import Test.HUnit
import Tarefa1        
import LI12425


------------
-- jogo01 --
------------

mapa01 :: Mapa
mapa01 = 
    [ [t, t, r, a, a, a],
      [r, t, r, a, r, r],
      [r, t, r, a, r, t],
      [r, t, r, a, r, t],
      [r, t, t, t, t, t],
      [a, a, a, a, r, r]
    ]
  where
    t = Terra
    r = Relva
    a = Agua

pathJogo01 :: [(Int, Int)]
pathJogo01 =
  [ (0, 0), (1, 0), (1, 1), (1, 2), (1, 3),
    (1, 4), (2, 4), (3, 4), (4, 4), (5, 4),
    (5, 3), (5, 2)
  ]


jogo01 :: Jogo
jogo01 = Jogo {
    baseJogo = Base {
        posicaoBase = (5.5, 2.5),
        vidaBase = 1000,
        creditosBase = 100
    },
    portaisJogo = [
        Portal {
            posicaoPortal = (0.5, 0.5),
            ondasPortal = [
                Onda {
                    inimigosOnda = [
                        Inimigo {
                            posicaoInimigo = (0.5, 0.5),
                            direcaoInimigo = Sul,
                            vidaInimigo = 50.0,
                            velocidadeInimigo = 1.0,
                            ataqueInimigo = 10,
                            butimInimigo = 5,
                            projeteisInimigo = [],
                            pathIndex = 0
                        },
                        Inimigo {
                            posicaoInimigo = (0.5, 0.5),
                            direcaoInimigo = Este,
                            vidaInimigo = 60.0,
                            velocidadeInimigo = 1.5,
                            ataqueInimigo = 12,
                            butimInimigo = 8,
                            projeteisInimigo = [],
                            pathIndex = 0
                        }
                    ],
                    cicloOnda = 3,
                    tempoOnda = 5,
                    entradaOnda = 10
                }
            ]
        }
    ],
    torresJogo = [
        Torre {
            posicaoTorre = (4.5, 3.5),
            projetilTorre = Projetil { tipoProjetil = Resina, duracaoProjetil = Infinita },
            danoTorre = 10,
            alcanceTorre = 7,
            rajadaTorre = 1,
            cicloTorre = 1,
            tempoTorre = 1
        },
        Torre {
            posicaoTorre = (2.5, 0.5),
            projetilTorre = Projetil { tipoProjetil = Gelo, duracaoProjetil = Finita 1 },
            danoTorre = 5,
            alcanceTorre = 9,
            rajadaTorre = 2,
            cicloTorre = 2,
            tempoTorre = 1
        }
    ],
    mapaJogo = mapa01,
    inimigosJogo = [
        Inimigo {
            posicaoInimigo = (1.5, 1.5),
            direcaoInimigo = Sul,
            vidaInimigo = 50.0,
            velocidadeInimigo = 1.0,
            ataqueInimigo = 50,
            butimInimigo = 10,
            projeteisInimigo = [],
            pathIndex = 0
        },
        Inimigo {
            posicaoInimigo = (2.5, 4.5),
            direcaoInimigo = Este,
            vidaInimigo = 25.0,
            velocidadeInimigo = 2.0,
            ataqueInimigo = 20,
            butimInimigo = 5,
            projeteisInimigo = [],
            pathIndex = 0
        }
    ],
    lojaJogo = [
        (20, Torre {
            posicaoTorre = (0.0, 0.0),
            projetilTorre = Projetil { tipoProjetil = Resina, duracaoProjetil = Finita 2 },
            danoTorre = 7,
            alcanceTorre = 5,
            rajadaTorre = 1,
            cicloTorre = 3,
            tempoTorre = 0
        })
    ],
    torreSelecionada = 0,
    ondasRestantes = 1,
    caminhoJogo = pathJogo01
}

-----------------------
-- Exemplos de Mapas --
-----------------------

mapaSimples :: Mapa
mapaSimples =
  [ [Terra, Terra, Relva],
    [Relva, Terra, Terra],
    [Agua, Relva, Terra]
  ]

pathMapaSimples :: [(Int,Int)]
pathMapaSimples =
  [ (0, 0), (1, 0), (1, 1), (2, 1), (2, 2) ]


mapaSemCaminho :: Mapa
mapaSemCaminho =
  [ [Terra, Relva, Relva],
    [Relva, Relva, Relva],
    [Agua, Relva, Terra]
  ]

---------------------------------
-- Exemplos de Jogos "simples" --
---------------------------------

jogoSimples :: Jogo
jogoSimples = Jogo
  { baseJogo = Base { posicaoBase = (2.5, 2.5), vidaBase = 100, creditosBase = 50 },
    portaisJogo = [Portal { posicaoPortal = (0.5, 0.5), ondasPortal = [] }],
    torresJogo = [],
    mapaJogo = mapaSimples,
    inimigosJogo = [],
    lojaJogo = [],
    torreSelecionada = 0,
    ondasRestantes = 0,
    caminhoJogo = pathMapaSimples
  }

jogoSemCaminho :: Jogo
jogoSemCaminho = jogoSimples { mapaJogo = mapaSemCaminho }


torresTeste :: [Torre]
torresTeste =
  [ Torre { posicaoTorre = (0.5, 0.5), projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Infinita },
            danoTorre = 10, alcanceTorre = 5, rajadaTorre = 1, cicloTorre = 3, tempoTorre = 0 }
  ]

baseTeste :: Base
baseTeste = Base { posicaoBase = (2.5, 1.5), vidaBase = 100, creditosBase = 50 }


-------------------------------
-- Testes funções principais --
-------------------------------

-- | Testa a validação de um jogo com base válida
testValidaBaseValida :: Test
testValidaBaseValida = "Base válida" ~: True ~=? validaBase jogo01
  
-- | Testa a validação de portais válidos
testPortaisValidos :: Test
testPortaisValidos = "Portais válidos" ~: True ~=? validaPortais jogo01

-- | Testa a validação de torres válidas-
testTorresValidas :: Test
testTorresValidas = "Torres válidas" ~: True ~=? validaTorres jogo01


-- | Testa a validação de inimigos válidos
testInimigosValidos :: Test
testInimigosValidos = "Inimigos válidos" ~: True ~=? validaInimigos jogo01

-- | Conjunto de todos os testes das funções principais
testsTarefa1Principais :: Test
testsTarefa1Principais = TestList
    [ testValidaBaseValida
    , testPortaisValidos
    , testTorresValidas
    , testInimigosValidos
    ]


---------------------------------
-- Testes para Funções Auxiliares
---------------------------------

-- | Testes para `caminhoParaBase`
testCaminhoParaBase :: Test
testCaminhoParaBase =
  TestLabel "Testes caminhoParaBase" $
    test
      [ "Caminho existe" ~: True ~=? caminhoParaBase (portaisJogo jogoSimples) (mapaJogo jogoSimples) (posicaoBase (baseJogo jogoSimples)),
        "Caminho não existe" ~: False ~=? caminhoParaBase (portaisJogo jogoSemCaminho) (mapaJogo jogoSemCaminho) (posicaoBase (baseJogo jogoSemCaminho))
      ]

-- | Testa a função `sobreTerra`
testSobreTerra :: Test
testSobreTerra =
  TestLabel "Testes sobreTerra" $
    test
      [ "Posição (0.5, 0.5) está sobre Terra" ~: True ~=? sobreTerra mapaSimples (0.5, 0.5),
        "Posição (0.5, 1.5) não está sobre Terra" ~: False ~=? sobreTerra mapaSimples (0.5, 1.5),
        "Posição fora do mapa" ~: False ~=? sobreTerra mapaSimples (10.5, 10.5)
      ]

-- | Testa a função `sobreRelva`
testSobreRelva :: Test
testSobreRelva =
  TestLabel "Testes sobreRelva" $
    test
      [ "Posição (0.5, 1.5) está sobre Relva" ~: True ~=? sobreRelva mapaSimples (0.5, 1.5),
        "Posição (1.5, 1.5) não está sobre Relva" ~: False ~=? sobreRelva mapaSimples (1.5, 1.5),
        "Posição fora do mapa" ~: False ~=? sobreRelva mapaSimples (10.5, 10.5)
      ]

-- | Testa a função `posicaoTerreno`
testPosicaoTerreno :: Test
testPosicaoTerreno =
  TestLabel "Testes posicaoTerreno" $
    test
      [ "Posição (0.5, 0.5) é Terra" ~: Just Terra ~=? posicaoTerreno mapaSimples (0.5, 0.5),
        "Posição (2.5, 0.5) é Relva" ~: Just Relva ~=? posicaoTerreno mapaSimples (2.5, 0.5),
        "Posição fora do mapa" ~: Nothing ~=? posicaoTerreno mapaSimples (10.5, 10.5)
      ]

-- | Testa a função `naoCoincide`
testNaoCoincide :: Test
testNaoCoincide =
  TestLabel "Testes naoCoincide" $
    test
      [ "Torre em (0.5, 0.5) coincide com outra torre" ~: False ~=? naoCoincide torresTeste baseTeste (0.5, 0.5),
        "Base em (2.5, 1.5) coincide com a base" ~: False ~=? naoCoincide torresTeste baseTeste (2.5, 1.5),
        "Posição (1.5, 1.5) não coincide com torres nem base" ~: True ~=? naoCoincide torresTeste baseTeste (1.5, 1.5)
      ]


-- | Conjunto de todos os testes das funções auxiliares
testsTarefa1Auxiliares :: Test
testsTarefa1Auxiliares = TestList
    [ testCaminhoParaBase
    , testSobreTerra
    , testSobreRelva
    , testPosicaoTerreno
    , testNaoCoincide
    ]


-- | Conjunto de todos os testes da tarefa1
testesTarefa1 :: Test
testesTarefa1 = TestList 
    [ testsTarefa1Principais
    , testsTarefa1Auxiliares
    ]


