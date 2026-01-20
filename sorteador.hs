import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down (Down))
import System.IO (hFlush, stdout)
import Chaveamento (torneio, numJogosTorneio)

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

    putStr "Deseja realizar o chaveamento do torneio? (s/n): "
    hFlush stdout
    opcao <- getLine

    if opcao == "s" || opcao == "S"
        then do
            putStrLn "\n=== Torneio Mata-Mata ==="
            putStrLn $ "Quantidade de jogos: " ++ show (numJogosTorneio times)
            torneio [nomeTime t | t <- times]
        else do
            putStrLn "\nEncerrando programa..."