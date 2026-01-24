module Main where

import Sorteador
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down (Down))
import System.IO (hFlush, stdout)

-- Função para ler tudo numa mesma linha
prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine


-- Construir lista com os jogadores cadastrados
cadastrarJogadores :: Int -> IO [Jogador]
cadastrarJogadores contador = do
    putStrLn ("\n--- Jogador " ++ show contador ++ " ---")
    
    nome <- prompt "Nome (Enter vazio para encerrar): "
    
    if null nome
        then return []
        else do
            estrelasStr <- prompt "Estrelas (1-5): "
            let estrelasInt = read estrelasStr :: Int 
            
            restoDaLista <- cadastrarJogadores (contador + 1)
            
            return (Jogador nome estrelasInt : restoDaLista)

main :: IO ()
main = do
    putStrLn "--- GERADOR DE TIMES ---"

    qtdTimesStr <- prompt "Quantos times? "
    let qtdTimes = read qtdTimesStr :: Int

    maxPorTimeStr <- prompt "Maximo de jogadores por time? "
    let maxPorTime = read maxPorTimeStr :: Int

    putStrLn "\n>>> Iniciando cadastro..."
    todosJogadores <- cadastrarJogadores 1

    putStrLn "\n=========================================="
    putStrLn "          RESULTADO DOS TIMES             "
    putStrLn "=========================================="

    let times = montarTimesEquilibrados qtdTimes maxPorTime todosJogadores

    mapM_ (\t -> do
        putStrLn $ ">> " ++ nomeTime t ++ " (Força Total: " ++ show (forcaTotal t) ++ ")"
        putStrLn $ "   Elenco: " ++ show [ (nomeJogador j, estrelas j) | j <- elenco t ]
        putStrLn ""
        ) times
