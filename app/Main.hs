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
    cadastrarJogadoresController
    menuJogadores 
processarJogadores "2" = do
    listarJogadoresController
    menuJogadores
processarJogadores "3" = do
    selecionarJogadoresController
    menuJogadores
processarJogadores "4" = menuSorteio
processarJogadores _ = do
    putStrLn "Opção Inválida!"
    menuJogadores

menuJogadores :: IO ()
menuJogadores = do
    putStrLn "\n--- MENU JOGADORES ---"
    putStrLn "1. Cadastrar Jogadores"
    putStrLn "2. Listar Jogadores Cadastrados" 
    putStrLn "3. Selecionar Jogadores" 
    putStrLn "4. Iniciar Sorteio" 
    putStrLn "0. Sair do Sistema"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarJogadores opcao

-- MENU SORTEIO

processarSorteio :: String -> IO ()
processarSorteio "0" = menuJogadores
processarSorteio "1" = do
    times <- sorteioRapidoController
    menuCampeonato times
processarSorteio "2" = do
    putStrLn "Sorteio Não Deterministico realizado!"
    times <- sorteioNaoDeterministicoController
    menuCampeonato times
processarSorteio "0" = do
    
    menuJogadores
processarSorteio _ = do
    putStrLn "Opção Inválida!"
    menuSorteio

menuSorteio :: IO ()
menuSorteio = do
    putStrLn "\n--- MENU SORTEIO ---"
    putStrLn "1. Sortear Times Equilibrados (Rápido)" 
    putStrLn "2. Sortear Times Equilibrados (16 jogadores)"
    putStrLn "0. Retornar ao Menu Jogadores"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarSorteio opcao

-- MENU CAMPEONATO

processarCampeonato :: String -> [Time] -> IO ()
processarCampeonato "1" ts = do
    putStrLn "      ===== MATA-MATA ====="
    mataMataController ts
    putStr "feature: fazer outra interação com enter, para voltar para o menu-jogadores por exemṕlo"
processarCampeonato "0" _ = do
    menuJogadores
processarCampeonato _ ts = do
    putStrLn "Opção Inválida!"
    menuCampeonato ts

menuCampeonato :: [Time] -> IO ()
menuCampeonato ts = do
    putStrLn "\n--- MENU CAMPEONATO ---"
    putStrLn "1. Jogar Mata-Mata"
    putStrLn "0. Retornar Menu Jogadores"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarCampeonato opcao ts

