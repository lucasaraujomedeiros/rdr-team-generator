import Data.List (sortBy, transpose)
import Data.Ord (comparing)

data Jogador = Jogador {
    nomeJogador :: String,
    estrelas :: Int
} deriving (Show, Eq)

data Time = Time {
    nomeTime :: String,
    elenco :: [Jogador],
    forcaTotal :: Int 
} deriving (Show)

-- Função Auxiliar: Corta uma lista em pedaços de tamanho N
-- chunksOf 2 [1,2,3,4] -> [[1,2], [3,4]]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n lista = take n lista : chunksOf n (drop n lista)



montarTimesEquilibrados :: Int -> [Jogador] -> [Time]
montarTimesEquilibrados qtdTimes jogadores =
    let
        -- Ordenar decrescente por estrelas (do melhor pro pior)
       
        jogadoresOrdenados = sortBy (flip (comparing estrelas)) jogadores
    
        -- Agrupar em níveis 
        gruposPorNivel = chunksOf qtdTimes jogadoresOrdenados
        
        -- Distribuir Transposição
        -- Transforma [[Melhor1, Melhor2], [Pior1, Pior2]] 
        -- em        [[Melhor1, Pior1],   [Melhor2, Pior2]]
        elencosDistribuidos = transpose gruposPorNivel
        
        -- Criar os objetos Time finais com nomes
        -- 'zip' junta o numero do time (1, 2...) com a lista de jogadores dele
        criarTime (numero, listaJogadores) = Time {
            nomeTime = "Time " ++ show numero,
            elenco = listaJogadores,
            forcaTotal = sum (map estrelas listaJogadores) -- Soma as estrelas
        }
        
    in
        map criarTime (zip [1..qtdTimes] elencosDistribuidos)

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

    let todos = [j1, j6, j3, j8, j2, j5, j4, j7, j8, j9, j10, j11, j12, j13, j16, j15, j14]

    putStrLn "--- Times Equilibrados (2 Times) ---"
 
    let times = montarTimesEquilibrados 4 todos
    
    mapM_ print times
