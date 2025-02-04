module Tarefa3Spec (testesTarefa3) where

import Test.HUnit
import Tarefa3 
import LI12425


-----------------------------------------
--- Dados para testes para as funções ---
-----------------------------------------

-- | Exemplo de mapa 
mapaTeste :: Mapa
mapaTeste =
  [ [Terra, Terra, Relva],
    [Relva, Terra, Terra],
    [Agua, Relva, Terra]
  ]

-- | Definição do pathUnicoTest
pathUnicoTest :: [(Int, Int)]
pathUnicoTest =
  [ (0, 0)
  , (1, 0)
  , (2, 2)
  , (3, 2)
  , (3, 3)
  ]

-- | Exemplo de torres para teste

torre1 :: Torre
torre1 = Torre 
  { posicaoTorre = (0.5, 1.5), 
    alcanceTorre = 10, 
    danoTorre = 15, 
    cicloTorre = 1, 
    rajadaTorre = 1, 
    tempoTorre = 0,
    projetilTorre = Projetil Fogo (Finita 2)
  } 

torre2 :: Torre
torre2 = Torre 
  { posicaoTorre = (1.5, 2.5), 
    alcanceTorre = 10, 
    danoTorre = 15, 
    cicloTorre = 1, 
    rajadaTorre = 1, 
    tempoTorre = 1,
    projetilTorre = Projetil Gelo (Finita 3)
  }


-- | Exemplos de inimigos

inimigo1 :: Inimigo
inimigo1 = Inimigo 
  { posicaoInimigo = (2.5, 2.5), 
    vidaInimigo = 10, 
    projeteisInimigo = [Projetil Fogo (Finita 5)], 
    velocidadeInimigo = 1.0, 
    direcaoInimigo = Este, 
    ataqueInimigo = 10, 
    butimInimigo = 5,
    pathIndex = 0 
  }

inimigo2 :: Inimigo
inimigo2 = Inimigo 
  { posicaoInimigo = (0.5, 0.5), 
    vidaInimigo = 90, 
    projeteisInimigo = [], 
    velocidadeInimigo = 1.0, 
    direcaoInimigo = Este, 
    ataqueInimigo = 15, 
    butimInimigo = 5,
    pathIndex = 0
  }

-- | Exemplos de ondas 

onda1 :: Onda
onda1 = Onda 
  { entradaOnda = 5, 
    tempoOnda = 3, 
    cicloOnda = 6, 
    inimigosOnda = [inimigo1] 
  }

onda2 :: Onda
onda2 = Onda 
  { entradaOnda = 0, 
    tempoOnda = 2, 
    cicloOnda = 4, 
    inimigosOnda = [inimigo2] 
  }

-- | Exemplo de portal

portal1 :: Portal
portal1 = Portal 
  { posicaoPortal = (0.5, 0.5), 
    ondasPortal = [onda1, onda2] 
  }


-- | Exemplo de Jogo 

jogoTeste :: Jogo
jogoTeste = Jogo 
  { baseJogo = Base { posicaoBase = (2.5, 2.5), vidaBase = 100, creditosBase = 0 },
    portaisJogo = [portal1], 
    inimigosJogo = [inimigo1],
    mapaJogo = mapaTeste,
    lojaJogo = [],
    torresJogo = [torre1], 
    torreSelecionada = 1,
    ondasRestantes = 2,
    caminhoJogo = pathUnicoTest
  }


-- | Testa se são atualizados as torres e os seus ataques aos inimigos
testeAtualizaTorres :: Test
testeAtualizaTorres =
  TestLabel "Teste atualiza torres" $
    test
      [ "Atualiza torre, recarregando-a" 
         ~: tempoTorre torre2 ~=? (tempoTorre (fst (disparaTorre torre1 1 []))),
        "Torre dispara em inimigo no alcance" 
         ~: 75 ~=? (vidaInimigo . head $ snd (disparaTorre torre1 1 [inimigo2]))
      ]

-- | Testa se os portais são atualizados (lançam inimigos)
testeAtualizaPortais :: Test
testeAtualizaPortais =
  TestLabel "Teste atualiza portais" $
    test
      [ "Atualiza portais para o valor de tempo igual 1" 
          ~: 1 ~=? (length (inimigosJogo (atualizaPortais 1 jogoTeste))),
        "Verifica estado das ondas do portal após atualização"
          ~: 0.0 ~?= (tempoOnda . head . ondasPortal . head $ portaisJogo (atualizaPortais 3 jogoTeste))
      ]

      
-- | Testa se os inimigos são atualizados 
testeAtualizaInimigos :: Test
testeAtualizaInimigos = 
  TestLabel "Teste atualiza inimigos" $
    test
      [ "Base perde vida corretamente após os ataques" 
         ~: 90.0 ~=? (vidaBase (baseJogo (atualizaInimigos 1 jogoTeste))),
        "Inimigo é derrotado" 
         ~: [] ~=? (inimigosJogo (atualizaInimigos 2 jogoTeste))
      ]


-- | Conjunto de todos os testes da tarefa3
testesTarefa3 :: Test
testesTarefa3 =  
  TestLabel "Testes Tarefa 3" $
    test
      [ testeAtualizaTorres
      , testeAtualizaInimigos
      , testeAtualizaPortais
      ]