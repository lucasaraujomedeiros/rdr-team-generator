import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down (Down))

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


main :: IO ()
main = do

    let j1 = Jogador "Pelé" 5
    let j2 = Jogador "Zico" 5
    let j3 = Jogador "Neymar" 4
    let j4 = Jogador "Ronaldo" 4
    let j5 = Jogador "Bagre 1" 2
    let j6 = Jogador "Bagre 2" 2
    let j7 = Jogador "Perna de Pau 1" 1
    let j8 = Jogador "Perna de Pau 2" 1
    let j9 = Jogador "Pedro" 4
    let j10 = Jogador "Gerson" 3
    let j11 = Jogador "Pulgar" 5
    let j12 = Jogador "Arrascaeta" 1
    let j13 = Jogador "David" 2
    let j14 = Jogador "Rossi" 4
    let j15 = Jogador "Nicolas" 3
    let j16 = Jogador "Ronaldo" 3

    let todos = [j1, j6, j3, j8, j2, j5, j4, j7, j9, j10, j11, j12, j13, j16, j15, j14]

    putStrLn "--- Times Equilibrados (4 Times) ---"

    -- Exemplo: 4 times com até 4 jogadores cada
    let times = montarTimesEquilibrados 4 3 todos

    mapM_ (\t -> print t >> putStrLn "") times
