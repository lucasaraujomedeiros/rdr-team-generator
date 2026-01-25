module Controller where

import ListarJogadores (listarJogadores)
import CadastraJogadores (cadastrarJogadores)
import Sorteador (montarTimesEquilibrados, Time(..), Jogador(..)) -- importar depois tambem o não deterministico
import MataMata (torneio)

cadastrarJogadoresController :: IO ()
cadastrarJogadoresController = cadastrarJogadores 1

listarJogadoresController :: IO ()
listarJogadoresController = do 
    putStrLn "Listando Jogadores"
    listarJogadores

selecionarJogadoresController :: IO [Jogador]
selecionarJogadoresController = do
    putStr "jogadores selecionados -> " -- TODO: chamar função selecionar jogadores do 'CadastraJogadores'
    return []
    
sorteioRapidoController :: [Jogador] -> IO [Time]
sorteioRapidoController js = do
    let numTimes = 3 -- fazer prompt para pegar esses dados
    let numJogPorTime = 2
    let times = montarTimesEquilibrados numTimes numJogPorTime js 
    putStrLn (show times)
    return []

sorteioNaoDeterministicoController :: [Jogador] -> IO [Time]
sorteioNaoDeterministicoController js = do 
    putStrLn "rodando o não deterministico" -- TODO:  implementar a função que roda o não deterministico
    return []

pontosCorridosController :: [Time] -> IO ()
pontosCorridosController ts = do
    putStrLn "rodando os pontos corridos" -- TODO: implementar a função que roda o pontos corridos

mataMataController :: [Time] -> IO ()
mataMataController ts = torneio [nomeTime t | t <- ts]

campeonatoCompletoController :: [Time] -> IO ()
campeonatoCompletoController ts = do
    let timesAposPontosCorridos = [] -- chamar os pontos corridos
    torneio [nomeTime t | t <- timesAposPontosCorridos]





