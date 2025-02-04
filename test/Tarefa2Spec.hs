module Tarefa2Spec (testesTarefa2) where

import Test.HUnit

import Tarefa2           

import LI12425          

---------------------------
---- Exemplo de Dados -----
---------------------------

-- | Exemplo de mapa
mapaTeste :: Mapa
mapaTeste =
  [ [Terra, Terra, Relva],
    [Relva, Terra, Terra],
    [Agua, Relva, Terra]
  ]

-- | Caminho do mapa anterior
pathMapaTeste :: [(Int, Int)]
pathMapaTeste = 
  [ (0, 0), (1, 0), (1, 1), (2, 1), (2, 2) ]

-- | Exemplos de inimigos para teste

inimigo1 :: Inimigo
inimigo1 = Inimigo
  { posicaoInimigo = (5.5, 3.5),
    direcaoInimigo = Sul,
    vidaInimigo = 50,
    velocidadeInimigo = 1.0,
    ataqueInimigo = 10,
    butimInimigo = 5,
    projeteisInimigo = [],
    pathIndex = 0
  }

inimigo2 :: Inimigo
inimigo2 = Inimigo
  { posicaoInimigo = (4.5, 3.5),
    direcaoInimigo = Este,
    vidaInimigo = 25,
    velocidadeInimigo = 1.5,
    ataqueInimigo = 12,
    butimInimigo = 8,
    projeteisInimigo = [],
    pathIndex = 0
  }


-- | Exemplo de torre para teste
torreTeste :: Torre
torreTeste = Torre
  { posicaoTorre = (1.5, 1.5),
    projetilTorre = Projetil { tipoProjetil = Fogo, duracaoProjetil = Infinita },
    danoTorre = 30,
    alcanceTorre = 5,
    rajadaTorre = 1,
    cicloTorre = 3,
    tempoTorre = 0
  }

-- | Exemplo de portais para teste

-- | Portal com uma onda
portalComOndas :: Portal
portalComOndas = Portal
  { posicaoPortal = (0.5, 0.5),
    ondasPortal =
      [ Onda { inimigosOnda = [inimigo1], cicloOnda = 3, tempoOnda = 0, entradaOnda = 0 }
      ]
  }

-- | Portal com duas ondas
portalComDuasOndas :: Portal
portalComDuasOndas = Portal
  { posicaoPortal = (1.0, 1.0),
    ondasPortal =
      [ Onda { inimigosOnda = [inimigo1], cicloOnda = 3, tempoOnda = 0, entradaOnda = 0 },
        Onda { inimigosOnda = [inimigo2], cicloOnda = 2, tempoOnda = 0, entradaOnda = 0 }
      ]
  }

-- | Portal sem ondas
portalSemOndas :: Portal
portalSemOndas = Portal
  { posicaoPortal = (0.5, 0.5),
    ondasPortal = []
  }

-- | Lista de inimigos ativos vazia
inimigosAtivosVazio :: [Inimigo]
inimigosAtivosVazio = []


-- | Exemplo de jogo
jogoTeste :: Jogo
jogoTeste = Jogo
  { baseJogo = Base { posicaoBase = (3.5, 2.5), vidaBase = 500, creditosBase = 100 },
    portaisJogo = [portalComOndas],
    torresJogo = [torreTeste],
    mapaJogo = mapaTeste,
    inimigosJogo = [inimigo1],
    lojaJogo = [], 
    torreSelecionada = 0,
    ondasRestantes = 0,
    caminhoJogo = pathMapaTeste
  }


------------------------------
--- Testes para as funções ---
------------------------------

-- | Testa se o(s) inimigo(s) estão no alcance da torre
testeInimigosNoAlcance :: Test
testeInimigosNoAlcance =
  TestLabel "Teste inimigos no alcance" $
    test
      [ "Nenhum inimigo no alcance" ~: [] ~=? map posicaoInimigo (inimigosNoAlcance torreTeste []),
        "Um inimigo no alcance" ~: [posicaoInimigo inimigo1] ~=? map posicaoInimigo (inimigosNoAlcance torreTeste [inimigo1]),
        "Vários inimigos no alcance" ~: [posicaoInimigo inimigo1, posicaoInimigo inimigo2] ~=? map posicaoInimigo (inimigosNoAlcance torreTeste [inimigo1, inimigo2])
      ]

-- | Testa se o inimigo foi atingido
testeInimigoAtingido :: Test
testeInimigoAtingido =
  TestLabel "Teste inimigo atingido" $
    test
      [ "Inimigo sobrevive após ataque" ~: 20 ~=? vidaInimigo (atingeInimigo torreTeste inimigo1),
        "Inimigo é eliminado após ataque" ~: 0 ~=? vidaInimigo (atingeInimigo torreTeste inimigo2)
      ]

-- | Testa a ativação de um inimigo de um portal
testeAtivaInimigo :: Test
testeAtivaInimigo =
  TestLabel "Teste ativa inimigo" $
    test
      [ "Ativa um inimigo de um portal com uma única onda"
          ~: (0,1)
          ~=? (length (ondasPortal (fst (ativaInimigo portalComOndas inimigosAtivosVazio))), length (snd (ativaInimigo portalComOndas inimigosAtivosVazio))),
        
        "Ativa um inimigo do portal com múltiplas ondas"
          ~: (1, 1)
          ~=? (length (ondasPortal (fst (ativaInimigo portalComDuasOndas inimigosAtivosVazio))), length (snd (ativaInimigo portalComDuasOndas inimigosAtivosVazio))),
        
        "Portal sem ondas restantes"
          ~: (0, 0) 
          ~=? (length (ondasPortal (fst (ativaInimigo portalSemOndas inimigosAtivosVazio))), length (snd (ativaInimigo portalSemOndas inimigosAtivosVazio)))
      ]

-- | Testa o término do jogo
testeTerminoJogo :: Test
testeTerminoJogo =
  TestLabel "Teste término do jogo" $
    test
      [ "Jogo termina com vitória" ~: True ~=? ganhouJogo jogoTeste { inimigosJogo = [], portaisJogo = [portalSemOndas] },
        "Jogo termina com derrota" ~: True ~=? perdeuJogo jogoTeste { baseJogo = Base { posicaoBase = (3.5, 2.5), vidaBase = 0, creditosBase = 100 } },
        "Jogo ainda em progresso" ~: False ~=? terminouJogo jogoTeste
      ]


-- | Conjunto de todos os testes para a Tarefa 2
testesTarefa2 :: Test
testesTarefa2 =
  TestLabel "Testes Tarefa 2" $
    test
      [ testeInimigosNoAlcance,
        testeInimigoAtingido,
        testeAtivaInimigo,
        testeTerminoJogo
      ]

