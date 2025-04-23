## Immutable Towers - LI1

Um jogo baseado nos jogos da série Tower Defense, criado em Haskell no âmbito da cadeira de Laboratórios de Informática I, na licenciatura em Engenharia Informática da Universidade do Minho, no ano letivo 2024/25.


## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do Cabal.

```bash
cabal run --verbose=0
```

## Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Execute os testes com o comando `test` do Cabal e utilize a flag `--enable-coverage` para gerar um relatório de cobertura de testes.

```bash
cabal test --enable-coverage
```
