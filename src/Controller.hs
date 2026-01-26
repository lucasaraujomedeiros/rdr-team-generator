module Controller where

import Jogadores (cadastrarJogadores, listarJogadores, getJogadoresSelecionados, selecionarJogadores, prompt)
import Sorteador
import MataMata (torneio)
import System.Random (randomRIO)

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
   
 
    limiteEstrelasStr <- prompt "Limite da maior diferença de estrelas entre os times: "
    let limiteEstrelas = read limiteEstrelasStr :: Int

    jogadores <- getJogadoresSelecionados
    let configuracoes = geraPossibilidades jogadores limiteEstrelas

    configuracaoAleatoria <- pegaAleatorio configuracoes
    
    
    let inicializarTimes = map (\n -> Time { nomeTime = "Time " ++ show n, elenco = [], forcaTotal = 0 }) [1..4]
    let times = zipWith atribuiElenco inicializarTimes  configuracaoAleatoria
     
    mostrarTimes times
    return times  

pegaAleatorio :: [a] -> IO a
pegaAleatorio xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i)

mataMataController :: [Time] -> IO ()
mataMataController ts = torneio [nomeTime t | t <- ts]






