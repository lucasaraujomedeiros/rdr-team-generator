module Controller where

import Jogadores (cadastrarJogadores, listarJogadores, getJogadoresSelecionados, selecionarJogadores, prompt)
import Sorteador
import MataMata (torneio)
import System.Random (randomRIO)


limpaTela :: IO ()
limpaTela = putStr "\ESC[2J\ESC[H"

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
    jogadores <- getJogadoresSelecionados

    if length jogadores /= 16
        then do
            putStrLn "é necessário ter exatamente 16 jogadores para realizar o sorteio"
            return []
        else do
            putStrLn "rodando o não deterministico"

            limiteEstrelasStr <- prompt "Limite da maior diferença de estrelas entre os times: "
            let limiteEstrelas = read limiteEstrelasStr :: Int

            let configuracoes = geraPossibilidades jogadores limiteEstrelas
            times <- pegaTimesAleatorio configuracoes

            refresh 1 configuracoes times


pegaTimesAleatorio :: [[[Jogador]]] -> IO [Time]
pegaTimesAleatorio configuracoes = do
    
    configuracaoAleatoria <- pegaAleatorio configuracoes

    let inicializarTimes =
          map (\n -> Time { nomeTime = "Time " ++ show n
                          , elenco = []
                          , forcaTotal = 0
                          }) [1..4]

    let times = zipWith atribuiElenco inicializarTimes configuracaoAleatoria
 
    return times

refresh :: Int -> [[[Jogador]]]-> [Time] -> IO [Time]
refresh 1 configuracoes times = do
    limpaTela
    mostrarTimes times

    refreshStr <- prompt "(1) Refresh\n(2) Ok "
    let opcaoRefresh = read refreshStr :: Int

    novosTimes <- pegaTimesAleatorio configuracoes
    refresh opcaoRefresh configuracoes novosTimes

refresh 2 _ times =
    return times

pegaAleatorio :: [a] -> IO a
pegaAleatorio xs = do
    i <- randomRIO (0, length xs - 1)
    return (xs !! i)

mataMataController :: [Time] -> IO ()
mataMataController ts = torneio [nomeTime t | t <- ts]






