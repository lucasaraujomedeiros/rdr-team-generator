module Main where

import System.IO (hFlush, stdout)
import Controller
import Sorteador (Jogador(..), Time(..))

-- =========================
-- ANSI
-- =========================

reset, bold, cyan, green, yellow, blink :: String
reset  = "\ESC[0m"
bold   = "\ESC[1m"
cyan   = "\ESC[36m"
green  = "\ESC[32m"
yellow = "\ESC[33m"
blink  = "\ESC[5m"

-- =========================
-- LIMPAR TELA
-- =========================

limparTela :: IO ()
limparTela = putStr "\ESC[2J\ESC[H"

-- =========================
-- BANNER
-- =========================

banner :: String
banner = unlines
    [ cyan ++ "           ██████╗ ██████╗ ██████╗ " ++ reset
    , cyan ++ "           ██╔══██╗██╔══██╗██╔══██╗" ++ reset
    , cyan ++ "           ██████╔╝██║  ██║██████╔╝" ++ reset
    , cyan ++ "           ██╔══██╗██║  ██║██╔══██╗" ++ reset
    , cyan ++ "           ██║  ██║██████╔╝██║  ██║" ++ reset
    , cyan ++ "           ╚═╝  ╚═╝╚═════╝ ╚═╝  ╚═╝" ++ reset
    , ""
    , bold ++ " ════════════════════════════════════════════" ++ reset
    , bold ++ "       NON-DETERMINISTIC TEAM GENERATOR" ++ reset
    , bold ++ " ════════════════════════════════════════════" ++ reset
    ]

-- =========================
-- PROMPTS
-- =========================

prompt :: String -> IO String
prompt txt = do
    putStr (yellow ++ txt ++ reset)
    hFlush stdout
    getLine

promptEnterPiscando :: IO ()
promptEnterPiscando = do
    putStrLn ""
    putStr (blink ++ green ++ "      Pressione ENTER para iniciar..." ++ reset)
    hFlush stdout
    _ <- getLine
    return ()

-- =========================
-- MAIN
-- =========================

main :: IO ()
main = do
    limparTela
    putStrLn banner
    promptEnterPiscando
    limparTela
    menuJogadores
    putStrLn "FIM DO PROGRAMA"

-- =========================
-- MENU JOGADORES (lógica preservada)
-- =========================

menuJogadores :: IO ()
menuJogadores = do
    putStrLn (bold ++ "\n╔═════════ MENU JOGADORES ════════╗" ++ reset)
    putStrLn "║ 1. Cadastrar Jogadores          ║"
    putStrLn "║ 2. Listar Jogadores             ║"
    putStrLn "║ 3. Selecionar Jogadores         ║"
    putStrLn "║ 4. Iniciar Sorteio              ║"
    putStrLn "║ 0. Sair                         ║"
    putStrLn "╚═════════════════════════════════╝"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarJogadores opcao

processarJogadores :: String -> IO ()
processarJogadores "0" = putStrLn "Saindo..."
processarJogadores "1" = cadastrarJogadoresController >> menuJogadores
processarJogadores "2" = listarJogadoresController >> menuJogadores
processarJogadores "3" = selecionarJogadoresController >> menuJogadores
processarJogadores "4" = menuSorteio
processarJogadores _   = putStrLn "Opção inválida!" >> menuJogadores

-- =========================
-- MENU SORTEIO (lógica preservada)
-- =========================

menuSorteio :: IO ()
menuSorteio = do
    putStrLn (bold ++ "\n╔════════ MENU SORTEIO ════════╗" ++ reset)
    putStrLn "║ 1. Sorteio Rápido              ║"
    putStrLn "║ 2. Sorteio Não Determinístico  ║"
    putStrLn "║ 0. Voltar                      ║"
    putStrLn "╚════════════════════════════════╝"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarSorteio opcao

processarSorteio :: String -> IO ()
processarSorteio "0" = menuJogadores
processarSorteio "1" = do
    times <- sorteioRapidoController
    menuCampeonato times
processarSorteio "2" = do
    times <- sorteioNaoDeterministicoController
    menuCampeonato times
processarSorteio _ = putStrLn "Opção inválida!" >> menuSorteio

-- =========================
-- MENU CAMPEONATO (lógica preservada)
-- =========================

menuCampeonato :: [Time] -> IO ()
menuCampeonato ts = do
    putStrLn (bold ++ "\n╔════════ MENU CAMPEONATO ════════╗" ++ reset)
    putStrLn "║ 1. Jogar Mata-Mata              ║"
    putStrLn "║ 0. Voltar                       ║"
    putStrLn "╚═════════════════════════════════╝"
    opcao <- prompt "Escolha uma opção: "
    limparTela
    processarCampeonato opcao ts

processarCampeonato :: String -> [Time] -> IO ()
processarCampeonato "1" ts = do
    mataMataController ts
    _ <- prompt "\nPressione ENTER para voltar..."
    menuJogadores
processarCampeonato "0" _ = menuJogadores
processarCampeonato _ ts  = putStrLn "Opção inválida!" >> menuCampeonato ts

