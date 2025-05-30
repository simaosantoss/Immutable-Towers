# Immutable Towers - LI1

Um jogo baseado nos jogos da série Tower Defense, criado em Haskell no âmbito da cadeira de Laboratórios de Informática I (1º ano, 1º semestre), na licenciatura em Engenharia Informática da Universidade do Minho, no ano letivo 2024/25.

**O enunciado completo do projeto está disponível no ficheiro [`Enunciado.pdf`](Enunciado.pdf).**

## 📦 Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do Cabal.

```bash
cabal run --verbose=0
```

## 🎮 Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## 🧪 Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Execute os testes com o comando `test` do Cabal e utilize a flag `--enable-coverage` para gerar um relatório de cobertura de testes.

```bash
cabal test --enable-coverage
```

Execute os exemplos da documentação como testes com a biblioteca
[`doctest`](https://hackage.haskell.org/package/doctest). Para instalar o
executavel utilize o comando `cabal install doctest`.

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest --verbose=0
```

## 📚 Documentação

A documentação do projeto pode ser gerada recorrendo ao [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```

### 👥 Realizado por:
- Gabriel Rodrigues
- Simão Santos
