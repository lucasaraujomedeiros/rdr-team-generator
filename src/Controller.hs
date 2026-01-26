module Controller where

import Jogadores (cadastrarJogadores, listarJogadores, getJogadoresSelecionados, selecionarJogadores, prompt)
import Sorteador (montarTimesEquilibrados, Time(..), Jogador(..)) -- importar depois tambem o não deterministico
import MataMata (torneio)

mostrarTimes :: [Time] -> IO ()
mostrarTimes times = 
    mapM_ (\t -> do
        putStrLn $ ">> " ++ nomeTime t ++ " (Força Total: " ++ show (forcaTotal t) ++ ")"
        putStrLn $ "   Elenco: " ++ show [ (nomeJogador j, estrelas j) | j <- elenco t ]
        putStrLn ""
        ) times



cadastrarJogadoresController :: IO ()
cadastrarJogadoresController = cadastrarJogadores 1

listarJogadoresController :: IO ()
listarJogadoresController = do 
    listarJogadores

selecionarJogadoresController :: IO ()
selecionarJogadoresController = do
    selecionarJogadores
    
sorteioRapidoController :: IO [Time]
sorteioRapidoController = do
    putStrLn "\n--- Sorteio Rápido ---"

    qtdTimes_str <- prompt "Quantidade de Times: "
    let qtdTimes = read qtdTimes_str :: Int

    maxJog_str <- prompt "Máximo de jogadores por time: "
    let maxJog = read maxJog_str :: Int

    jogadores <- getJogadoresSelecionados
    let times = montarTimesEquilibrados qtdTimes maxJog jogadores
    mostrarTimes times 
    return times

sorteioNaoDeterministicoController :: IO [Time]
sorteioNaoDeterministicoController = do 
    putStrLn "rodando o não deterministico" -- TODO:  implementar a função que roda o não deterministico
    return []


mataMataController :: [Time] -> IO ()
mataMataController ts = torneio [nomeTime t | t <- ts]






