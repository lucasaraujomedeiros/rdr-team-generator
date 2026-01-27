module Main where

import Test.Hspec

-- módulos do projeto
import Jogadores as J
import Sorteador
import MataMata
import ListarJogadores  as LJ
import Controller (pegaAleatorio)


main :: IO ()
main = hspec $ do

  ----------------------------------------------------------------
  -- Jogadores.hs
  ----------------------------------------------------------------
  describe "split" $ do
    it "separa string por ;" $ do
      split ';' "Ana;5;true"
        `shouldBe` ["Ana","5","true"]

    it "funciona com string vazia" $ do
      split ';' ""
        `shouldBe` [""]

  describe "formatarJogador" $ do
    it "marca jogador selecionado" $ do
      J.formatarJogador "Ana;5;true"
        `shouldBe` "( X ) Ana\t ⭐ 5"

    it "marca jogador não selecionado" $ do
      J.formatarJogador "Ana;5;false"
        `shouldBe` "(   ) Ana\t ⭐ 5"

  describe "toggleSelecao" $ do
    it "troca true para false" $ do
      toggleSelecao "Ana;5;true"
        `shouldBe` "Ana;5;false"

    it "troca false para true" $ do
      toggleSelecao "Ana;5;false"
        `shouldBe` "Ana;5;true"

  describe "toggleLinha" $ do
    it "altera a linha correta" $ do
      toggleLinha ["A;1;true","B;2;false"] 2
        `shouldBe` ["A;1;true","B;2;true"]

    it "ignora índice inválido" $ do
      toggleLinha ["A;1;true"] 0
        `shouldBe` ["A;1;true"]

  describe "filtrarSelecionados" $ do
    it "retorna apenas linhas com true" $ do
      filtrarSelecionados
        [ "Ana;5;true"
        , "Bruno;3;false"
        , "Carlos;4;true"
        ]
        `shouldBe`
        [ "Ana;5;true"
        , "Carlos;4;true"
        ]

  describe "parseJogador" $ do
    it "converte string em Jogador" $ do
      parseJogador "Ana;5;true"
        `shouldBe` Jogador "Ana" 5


  ----------------------------------------------------------------
  -- Sorteador.hs
  ----------------------------------------------------------------
  describe "totalEstrelas" $ do
    it "soma corretamente as estrelas" $ do
      totalEstrelas [Jogador "A" 2, Jogador "B" 3]
        `shouldBe` 5

  describe "estaBalanceado" $ do
    it "retorna True quando balanceado" $ do
      estaBalanceado
        [ [Jogador "A" 3]
        , [Jogador "B" 4]
        ]
        2 `shouldBe` True

    it "retorna False quando não balanceado" $ do
      estaBalanceado
        [ [Jogador "A" 1]
        , [Jogador "B" 10]
        ]
        3 `shouldBe` False

  describe "atribuiElenco" $ do
    it "atribui jogadores e calcula força" $ do
      let time = Time "Time 1" [] 0
          jogadores = [Jogador "A" 2, Jogador "B" 3]
      atribuiElenco time jogadores
        `shouldBe` Time "Time 1" jogadores 5

  describe "montarTimesEquilibrados" $ do
    it "cria a quantidade correta de times" $ do
      let jogadores =
            [ Jogador "A" 5
            , Jogador "B" 4
            , Jogador "C" 3
            , Jogador "D" 2
            ]
      length (montarTimesEquilibrados 2 2 jogadores)
        `shouldBe` 2

    describe "ListarJogadores.formatarJogador" $ do
        it "formata corretamente nome e estrelas" $ do
            LJ.formatarJogador "Ana;5"
            `shouldBe` "Ana - ⭐ 5"

        it "mantém a linha se o formato for inválido" $ do
            LJ.formatarJogador "LinhaInvalida"
            `shouldBe` "LinhaInvalida"

        it "funciona com estrelas grandes" $ do
            LJ.formatarJogador "Carlos;10"
            `shouldBe` "Carlos - ⭐ 10"

    describe "Controller.pegaAleatorio" $ do
        it "retorna um elemento que pertence à lista" $ do
            let xs = [1,2,3,4,5]
            x <- pegaAleatorio xs
            x `shouldSatisfy` (`elem` xs)
