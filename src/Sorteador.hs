module Sorteador where

import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down (Down))

-- Importar implementação do sorteador do main para este arquivo. Integrar main com ele, deixando apenas interface e chamada de funções no main.
--

data Jogador = Jogador {
    nomeJogador :: String,
    estrelas :: Int
} deriving (Show, Eq)

data Time = Time {
    nomeTime :: String,
    elenco :: [Jogador],
    forcaTotal :: Int
} deriving (Show)


montarTimesEquilibrados :: Int -> Int -> [Jogador] -> [Time]
montarTimesEquilibrados qtdTimes maxPorTime jogadores =
    let
        -- Ordenar decrescente por estrelas do melhor pro pior
        jogadoresOrdenados = sortBy (comparing (Down . estrelas)) jogadores

        -- Inicializar times vazios
        inicializarTimes = map (\n -> Time { nomeTime = "Time " ++ show n, elenco = [], forcaTotal = 0 }) [1..qtdTimes]

        -- Atribui um jogador ao time disponível (len < maxPorTime) com menor força total e, em caso de empate, menor elenco
        assignPlayer :: Jogador -> [Time] -> [Time]
        assignPlayer jog timesList =
            let availIdxs = [ i | (t,i) <- zip timesList [0..], length (elenco t) < maxPorTime ]
            in if null availIdxs
               then timesList -- todos os times cheios; jogador não é atribuído
               else
                   let candidates = [ (forcaTotal (timesList !! i), length (elenco (timesList !! i)), i) | i <- availIdxs ]
                       (_, _, idx) = minimumBy (comparing (\(ft,len,i) -> (ft,len,i))) candidates
                       (before, t:after) = splitAt idx timesList
                       t' = t { elenco = elenco t ++ [jog], forcaTotal = forcaTotal t + estrelas jog }
            in before ++ (t' : after)

        -- Construir times aplicando o algoritmo guloso
        timesFinal = foldl (flip assignPlayer) inicializarTimes jogadoresOrdenados
        in timesFinal


-- Segunda forma de sortear os times (gerar possibilidades e filtrar as ruins)

newtype Configuracao = Configuracao ([Time], [Jogador])
    deriving Show

transformaLista :: a -> [a]
transformaLista x = [x]

adicionaCapitao :: [Time] -> [[Jogador]] -> [Time]
adicionaCapitao [] _ = []
adicionaCapitao (Time nome elenco forca:xs) (y:ys) = Time nome y forca : adicionaCapitao xs ys 

atribuiCapitaes :: ([Time], [Jogador]) -> ([Time], [Jogador])
atribuiCapitaes (times, jogadores) = (adicionaCapitao times capitaes, drop n jogadores)
    where
        n = length times
        capitaes = take n (map transformaLista jogadores)


time1 = Time "time1" [] 0
time2 = Time "time2" [] 0
time3 = Time "time3" [] 0
time4 = Time "time4" [] 0
j1 = Jogador "lucas" 5
j2 = Jogador "araujo" 5
j3 = Jogador "bruno" 5 
j4 = Jogador "sereyh" 5
j5 = Jogador "hushcak" 5
times = [time1, time2, time3, time4]
jogadoress = [j1, j2, j3, j4, j5]
