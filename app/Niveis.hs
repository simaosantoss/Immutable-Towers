module Niveis where

import Graphics.Gloss
import ImmutableTowers
import LI12425

{- | 
 Módulo criado para colocar os dados, como mapas, caminhos definidos para o 
 inimigo seguir e dados do jogo em si, dos 3 níveis que criamos para o nosso jogo. 
-}

--------------------------------------------------------------------------------
------------------------------- Jogo 1 -----------------------------------------
--------------------------------------------------------------------------------

-- | Mapa jogo1
mapaJogo1 :: Mapa
mapaJogo1 =
  [ [Terra, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Relva]
  , [Terra, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Relva]
  , [Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra]
  , [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  , [Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  , [Agua,  Agua,  Agua,  Agua,  Agua,  Agua,  Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  , [Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  , [Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  , [Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  , [Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Relva, Relva, Relva, Relva, Relva, Agua,  Relva, Terra]
  ]

-- | Caminho seguido pelos inimigos
pathMapa1 :: [(Int, Int)]
pathMapa1 =
  [ (0,0), (0,1), (0,2), (1,2), (2,2), (3,2), (4,2), (5,2),
    (6,2), (7,2), (8,2), (9,2), (10,2), (11,2), (12,2), (13,2),
    (14,2), (14,3), (14,4), (14,5), (14,6), (14,7), (14,8), (14,9) ]

-- | Jogo 1
jogo1 :: Jogo
jogo1 =
  Jogo
    { baseJogo =
        Base
          { vidaBase     = 10
          , posicaoBase  = (14.5,9.5)   -- Combina com pathUnico
          , creditosBase = 1000         -- para testar sinergias
          }
    , portaisJogo =
        [ Portal
            { posicaoPortal = (0.5,0.5)
            , ondasPortal =
                [ Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Norte 
                         , vidaInimigo = 80.0      -- esta vida está feita para demonstrar as sinergias mais facilmente
                         , velocidadeInimigo = 0.7 
                         , ataqueInimigo = 5.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 30.0
                         , velocidadeInimigo = 0.7 
                         , ataqueInimigo = 8.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 4 
                    , tempoOnda   = 2 
                    , entradaOnda = 0 
                    }
                , Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este 
                         , vidaInimigo = 50.0
                         , velocidadeInimigo = 0.8
                         , ataqueInimigo = 30.0
                         , butimInimigo = 25
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 50.0
                         , velocidadeInimigo = 0.8 
                         , ataqueInimigo = 30.0
                         , butimInimigo = 15
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 10  -- tempo entre inimigos
                    , tempoOnda   = 10
                    , entradaOnda = 10
                    }
                ]
            }
        ]
    , torresJogo = []
    , mapaJogo   = mapaJogo1
    , inimigosJogo = []
    , lojaJogo =
        [ (20, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 5
                , alcanceTorre   = 4
                , rajadaTorre    = 1
                , cicloTorre     = 10
                , tempoTorre     = 5  
                , projetilTorre  = Projetil Fogo (Finita 3)
                }
          )
        , (30, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 3
                , alcanceTorre   = 4
                , rajadaTorre    = 5
                , cicloTorre     = 10
                , tempoTorre     = 5
                , projetilTorre  = Projetil Gelo (Finita 5)
                }
          )
        , (25, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 4
                , alcanceTorre   = 2.5
                , rajadaTorre    = 1
                , cicloTorre     = 9
                , tempoTorre     = 5
                , projetilTorre  = Projetil Resina Infinita
                }
          )
        ]
    , torreSelecionada = 0
    , ondasRestantes   = 2
    , caminhoJogo = pathMapa1
    }


--------------------------------------------------------------------------------
------------------------------- Jogo 2 -----------------------------------------
--------------------------------------------------------------------------------

-- | Mapa jogo2
mapaJogo2 :: Mapa
mapaJogo2 =
  [ [Terra, Terra,  Terra,  Relva,  Agua,  Relva, Relva, Agua,  Agua,  Relva, Relva, Relva, Relva, Relva, Relva]
  , [Relva, Relva,  Terra,  Relva,  Agua,  Relva, Relva, Agua,  Agua,  Relva, Relva, Relva, Agua,  Agua,  Relva]
  , [Relva, Relva,  Terra,  Relva,  Agua,  Relva, Terra, Terra, Terra, Terra, Relva, Relva, Agua,  Agua,  Relva]
  , [Relva, Relva,  Terra,  Relva,  Agua,  Relva, Terra, Agua,  Agua,  Terra, Relva, Relva, Agua,  Agua,  Relva]
  , [Relva, Relva,  Terra,  Relva,  Agua,  Relva, Terra, Agua,  Agua,  Terra, Relva, Relva, Relva, Relva, Relva]
  , [Relva, Relva,  Terra,  Terra,  Terra, Terra, Terra, Agua,  Agua,  Terra, Relva, Relva, Relva, Relva, Relva]
  , [Relva, Relva,  Relva,  Relva,  Agua,  Relva, Relva, Agua,  Agua,  Terra, Relva, Relva, Terra, Terra, Terra]
  , [Relva, Relva,  Relva,  Relva,  Agua,  Relva, Relva, Agua,  Agua,  Terra, Relva, Relva, Terra, Relva, Relva]
  , [Relva, Relva,  Relva,  Relva,  Agua,  Relva, Relva, Agua,  Agua,  Terra, Terra, Terra, Terra, Relva, Relva]
  , [Relva, Relva,  Relva,  Relva,  Agua,  Relva, Relva, Agua,  Agua,  Relva, Relva, Relva, Relva, Relva, Relva]
  ]

-- Caminho seguido pelos inimigos
pathMapa2 :: [(Int, Int)]
pathMapa2 =
  [ (0,0), (1,0), (2,0), (2,1), (2,2), (2,3), (2,4), (2,5),
    (3,5), (4,5), (5,5), (6,5), (6,4), (6,3), (6,2), (7,2),
    (8,2), (9,2), (9,3), (9,4), (9,5), (9,6), (9,7), (9,8), 
    (10,8), (11,8), (12,8), (12,7), (12,6), (13,6), (14,6) ]


-- | Jogo2
jogo2 :: Jogo
jogo2 =
  Jogo
    { baseJogo =
        Base
          { vidaBase     = 50.0
          , posicaoBase  = (14.5,6.5)   
          , creditosBase = 60
          }
    , portaisJogo =
        [ Portal
            { posicaoPortal = (0.5,0.5)
            , ondasPortal =
                [ Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Norte 
                         , vidaInimigo = 40.0
                         , velocidadeInimigo = 1.3
                         , ataqueInimigo = 10.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 60.0
                         , velocidadeInimigo = 1.2 
                         , ataqueInimigo = 12.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 3  
                    , tempoOnda   = 2  
                    , entradaOnda = 0  
                    }
                , Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este 
                         , vidaInimigo = 150.0
                         , velocidadeInimigo = 1.4
                         , ataqueInimigo = 25.0
                         , butimInimigo = 30
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 90.0
                         , velocidadeInimigo = 1.5 
                         , ataqueInimigo = 20.0
                         , butimInimigo = 25
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 3  
                    , tempoOnda   = 8
                    , entradaOnda = 8
                    }
                , Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Norte 
                         , vidaInimigo = 50.0
                         , velocidadeInimigo = 1.3
                         , ataqueInimigo = 8.0
                         , butimInimigo = 15
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 70.0
                         , velocidadeInimigo = 1.2 
                         , ataqueInimigo = 10.0
                         , butimInimigo = 18
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 80.0
                         , velocidadeInimigo = 1.5 
                         , ataqueInimigo = 12.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,0.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 100.0
                         , velocidadeInimigo = 1.2 
                         , ataqueInimigo = 10.0
                         , butimInimigo = 18
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 2  
                    , tempoOnda   = 16
                    , entradaOnda = 16
                    }
                ]
            }
        ]
    , torresJogo = []
    , mapaJogo   = mapaJogo2
    , inimigosJogo = []
    , lojaJogo =
        [ (25, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 5
                , alcanceTorre   = 4
                , rajadaTorre    = 2
                , cicloTorre     = 1.5
                , tempoTorre     = 3
                , projetilTorre  = Projetil Fogo (Finita 1)
                }
          )
        , (40, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 3
                , alcanceTorre   = 4
                , rajadaTorre    = 3
                , cicloTorre     = 2
                , tempoTorre     = 5
                , projetilTorre  = Projetil Gelo (Finita 2)
                }
          )
        , (35, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 5
                , alcanceTorre   = 2.5
                , rajadaTorre    = 1
                , cicloTorre     = 1.5
                , tempoTorre     = 4
                , projetilTorre  = Projetil Resina Infinita
                }
          )
        ]
    , torreSelecionada = 0
    , ondasRestantes   = 3
    , caminhoJogo = pathMapa2
    }


--------------------------------------------------------------------------------
------------------------------- Jogo 3 -----------------------------------------
--------------------------------------------------------------------------------

-- Mapa jogo3
mapaJogo3 :: Mapa
mapaJogo3 =
  [ [Relva, Relva, Relva, Relva, Relva, Terra, Terra, Terra, Terra, Terra, Relva, Relva, Relva, Relva, Relva]
  , [Agua,  Agua,  Agua,  Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Relva, Relva]
  , [Agua,  Agua,  Agua,  Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Relva, Terra]
  , [Agua,  Agua,  Agua,  Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Relva, Terra]
  , [Relva, Relva, Terra, Terra, Terra, Terra, Relva, Relva, Relva, Terra, Agua,  Agua,  Agua,  Relva, Terra]
  , [Relva, Relva, Terra, Relva, Relva, Relva, Relva, Relva, Relva, Terra, Agua,  Agua,  Agua,  Relva, Terra]
  , [Agua,  Agua,  Terra, Agua,  Agua,  Agua,  Relva, Terra, Terra, Terra, Agua,  Agua,  Agua,  Relva, Terra]
  , [Relva, Relva, Terra, Relva, Relva, Agua,  Relva, Terra, Relva, Relva, Agua,  Agua,  Agua,  Relva, Terra]
  , [Relva, Relva, Terra, Relva, Relva, Agua,  Relva, Terra, Relva, Relva, Agua,  Agua,  Agua,  Relva, Terra]
  , [Terra, Terra, Terra, Relva, Relva, Agua,  Relva, Terra, Terra, Terra, Terra, Terra, Terra, Terra, Terra]
  ]

-- | Caminho seguido pelos inimigos 
pathMapa3 :: [(Int, Int)]
pathMapa3 =
  [ (0,9), (1,9), (2,9), (2,8), (2,7), (2,6), (2,5), (2,4),
    (3,4), (4,4), (5,4), (5,3), (5,2), (5,1), (5,0), (6,0),
    (7,0), (8,0), (9,0), (9,1), (9,2), (9,3), (9,4), (9,5),
    (9,6), (8,6), (7,6), (7,7), (7,8), (7,9), (8,9), (9,9),
    (10,9), (11,9) ,(12,9), (13,9), (14,9), (14,8), (14,7),
    (14,6), (14,5), (14,4), (14,3), (14,2)]

-- | Jogo3
jogo3 :: Jogo
jogo3 =
  Jogo
    { baseJogo =
        Base
          { vidaBase     = 30
          , posicaoBase  = (14.5, 2.5) 
          , creditosBase = 60
          }
    , portaisJogo =
        [ Portal
            { posicaoPortal = (0.5, 9.5)
            , ondasPortal =
                [ Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Norte 
                         , vidaInimigo = 100.0
                         , velocidadeInimigo = 2.0
                         , ataqueInimigo = 5.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 150.0
                         , velocidadeInimigo = 1.5
                         , ataqueInimigo = 8.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 4   -- tempo entre inimigos da mesma onda
                    , tempoOnda   = 2   -- inicia em 2s
                    , entradaOnda = 0   -- começa imediatamente
                    }
                , Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este 
                         , vidaInimigo = 189.0
                         , velocidadeInimigo = 1.5
                         , ataqueInimigo = 25.0
                         , butimInimigo = 25
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 240.0
                         , velocidadeInimigo = 1.5 
                         , ataqueInimigo = 20.0
                         , butimInimigo = 15
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 2   -- tempo entre inimigos da mesma onda
                    , tempoOnda   = 8   -- inicia em 8s
                    , entradaOnda = 0   -- começa imediatamente
                    }
                , Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Norte 
                         , vidaInimigo = 100.0
                         , velocidadeInimigo = 1.0 
                         , ataqueInimigo = 8.0
                         , butimInimigo = 5
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 200.0
                         , velocidadeInimigo = 1.0 
                         , ataqueInimigo = 6.0
                         , butimInimigo = 10
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 90.0
                         , velocidadeInimigo = 1.2 
                         , ataqueInimigo = 4.0
                         , butimInimigo = 25
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 200.0
                         , velocidadeInimigo = 1.4
                         , ataqueInimigo = 10.0
                         , butimInimigo = 25
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Norte
                         , vidaInimigo = 199.0
                         , velocidadeInimigo = 1.5
                         , ataqueInimigo = 8.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 3   -- tempo entre inimigos da mesma onda
                    , tempoOnda   = 12  -- inicia em 12s
                    , entradaOnda = 0   -- começa imediatamente
                    }
                , Onda
                    { inimigosOnda =
                        [ Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Norte 
                         , vidaInimigo = 350.0
                         , velocidadeInimigo = 2.0
                         , ataqueInimigo = 5.0
                         , butimInimigo = 25
                         , projeteisInimigo = []
                         , pathIndex = 0
                         } 
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 200.0
                         , velocidadeInimigo = 1.4
                         , ataqueInimigo = 8.0
                         , butimInimigo = 10
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 90.0
                         , velocidadeInimigo = 1.4 
                         , ataqueInimigo = 8.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 130.0
                         , velocidadeInimigo = 2.2
                         , ataqueInimigo = 8.0
                         , butimInimigo = 20
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 180.0
                         , velocidadeInimigo = 2.0
                         , ataqueInimigo = 8.0
                         , butimInimigo = 15
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                         , Inimigo 
                         { posicaoInimigo = (0.5,9.5)
                         , direcaoInimigo = Este
                         , vidaInimigo = 300.0
                         , velocidadeInimigo = 2.3
                         , ataqueInimigo = 8.0
                         , butimInimigo = 30
                         , projeteisInimigo = []
                         , pathIndex = 0
                         }
                        ]
                    , cicloOnda   = 3   -- tempo entre inimigos da mesma onda
                    , tempoOnda   = 25  -- inicia em 25s
                    , entradaOnda = 0   -- começa imediatamente
                        
                    }
                ]
            }
        ]
    , torresJogo = []
    , mapaJogo   = mapaJogo3
    , inimigosJogo = []
    , lojaJogo =
        [ (20, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 5
                , alcanceTorre   = 4
                , rajadaTorre    = 2
                , cicloTorre     = 1
                , tempoTorre     = 3  
                , projetilTorre  = Projetil Fogo (Finita 1)
                }
          )
        , (30, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 3
                , alcanceTorre   = 4
                , rajadaTorre    = 2
                , cicloTorre     = 2
                , tempoTorre     = 5
                , projetilTorre  = Projetil Gelo (Finita 2)
                }
          )
        , (25, Torre
                { posicaoTorre   = (0,0)
                , danoTorre      = 4
                , alcanceTorre   = 2.5
                , rajadaTorre    = 1
                , cicloTorre     = 1.5
                , tempoTorre     = 4
                , projetilTorre  = Projetil Resina Infinita
                }
          )
        ]
    , torreSelecionada = 0
    , ondasRestantes   = 4
    , caminhoJogo = pathMapa3
    }
