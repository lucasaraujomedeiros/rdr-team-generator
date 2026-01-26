module Main where

import System.IO (hFlush, stdout)
import Controller
import Sorteador (Jogador(..), Time(..))

-- LIMPAR TELA

limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"

-- BANNER INICIAL

banner :: String
banner = unlines
    [ "                    ██████╗ ██████╗ ██████╗ "
    , "                    ██╔══██╗██╔══██╗██╔══██╗"
    , "                    ██████╔╝██║  ██║██████╔╝"
    , "                    ██╔══██╗██║  ██║██╔══██╗"
    , "                    ██║  ██║██████╔╝██║  ██║"
    , "                    ╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝"
    , ""
    , "          ═════════════════════════════════════════════"
    , "                NON-DETERMINISTIC TEAM GENERATOR"
    , "          ═════════════════════════════════════════════"
    , ""
    ]

-- PROMPT PADRÃO

prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine

-- PROMPT ENTER PISCANDO

promptEnterPiscando :: IO ()
promptEnterPiscando = do
    putStrLn ""
    putStr (blinkOn ++ "                      ENTER para iniciar..." ++ resetAll)
    hFlush stdout
    _ <- getLine
    return ()
  where
    blinkOn  = "\ESC[5m"
    resetAll = "\ESC[0m"

-- MAIN

main :: IO ()
main = do
    limparTela
    putStrLn banner
    promptEnterPiscando
    limparTela
    menuJogadores
    putStrLn "FIM DO PROGRAMA"

-- MENU JOGADORES

processarJogadores :: String -> IO ()
processarJogadores "0" = putStrLn "Saindo..."
processarJogadores "1" = do
    putStrLn "Chama Função cadastrar jogadores"
    cadastrarJogadoresController
    menuJogadores 
processarJogadores "2" = do
    putStrLn "Chama listar jogadores"
    listarJogadoresController
    menuJogadores
processarJogadores "3" = do
    putStrLn "Chama selecionar jogadores para sorteio"
    jogadores <- selecionarJogadoresController
    menuSorteio jogadores
processarJogadores _ = do
    putStrLn "Opção Inválida!"
    menuJogadores

menuJogadores :: IO ()
menuJogadores = do
    putStrLn "\n--- MENU JOGADORES ---"
    putStrLn "1. Cadastrar Jogadores"
    putStrLn "2. Listar Jogadores Cadastrados" 
    putStrLn "3. Iniciar Sorteio" 
    putStrLn "0. Sair do Sistema"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarJogadores opcao

-- MENU SORTEIO

processarSorteio :: [Jogador] -> String -> IO ()
processarSorteio js "0" = menuJogadores
processarSorteio js "1" = do
    putStrLn "Sorteio Rápido realizado!"
    times <- sorteioRapidoController js
    menuCampeonato times
processarSorteio js "2" = do
    putStrLn "Sorteio Não Deterministico realizado!"
    times <- sorteioNaoDeterministicoController js
    menuCampeonato times
processarSorteio js _ = do
    putStrLn "Opção Inválida!"
    menuSorteio js

menuSorteio :: [Jogador] -> IO ()
menuSorteio js = do
    putStrLn "\n--- MENU SORTEIO ---"
    putStrLn "1. Sortear Times Equilibrados (16 jogadores)"
    putStrLn "2. Sortear Times Equilibrados (Rápido)" 
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarSorteio js opcao

-- MENU CAMPEONATO

processarCampeonato :: [Time] -> String -> IO ()
processarCampeonato ts "1" = do
    putStrLn "Iniciando Mata-Mata..."
    mataMataController ts
processarCampeonato ts "2" = do
    putStrLn "Iniciando Pontos Corridos..."
    pontosCorridosController ts
processarCampeonato ts "3" = do
    putStrLn "Iniciando Campeonato Completo..."
    campeonatoCompletoController ts
processarCampeonato ts _ = do
    putStrLn "Opção Inválida!"
    menuCampeonato ts

menuCampeonato :: [Time] -> IO ()
menuCampeonato ts = do
    putStrLn "\n--- MENU CAMPEONATO ---"
    putStrLn "1. Mata-Mata"
    putStrLn "2. Pontos Corridos"
    putStrLn "3. Completo"
    putStrLn "0. Retornar Menu Sorteio"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarCampeonato ts opcao

