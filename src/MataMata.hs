module MataMata (torneio, numJogosTorneio) where

import System.IO (hFlush, stdout)

-- Próxima potência de 2, usado para normalizar torneio
proximaPotenciaDe2 :: Int -> Int
proximaPotenciaDe2 n
    | n <= 0    = 1
    | otherwise = 2 ^ expoente
    where
        expoente = ceiling (logBase 2 (fromIntegral n)) -- usa logaritmo e arredonda para o teto

-- Executa uma rodada de confrontos (como oitavas, quartas...)
rodada :: [String] -> IO [String]
rodada [] = return []
rodada [_] = error "A rodada precisa de um número par de times!"
rodada times = do
    let timeA = head times
    let timeB = last times
    let meio = init (tail times)

    putStr (timeA ++ " X " ++ timeB ++ "\nQual time venceu? (A ou B) ")
    hFlush stdout
    vencedor <- getLine

    let ganhador = if vencedor == "A" || vencedor == "a" then timeA else timeB

    -- Recursão para os próximos pares
    vencedoresRestantes <- if null meio then return [] else rodada meio
    return (ganhador : vencedoresRestantes)

-- Normaliza se não for potência de 2
primeiraRodada :: [String] -> IO [String]
primeiraRodada times = do
    let n = length times
    let pot2 = proximaPotenciaDe2 n
    let numPassamDireto = pot2 `mod` n

    let timesPassamDireto = take numPassamDireto times
    let timesComPartida = drop numPassamDireto times

    vencedoresPartida <- rodada timesComPartida
    return (timesPassamDireto ++ vencedoresPartida)

-- Função principal do Torneio
torneio :: [String] -> IO ()
torneio [] = putStrLn "Nenhum time no torneio."
torneio [vencedor] = putStrLn (" > O grande vencedor é: " ++ vencedor)
torneio times = do
    let n = length times
    let pot2 = proximaPotenciaDe2 n

    -- Se não for potência de 2, executa a primeira rodada que normaliza
    timesNovaRodada <- if n /= pot2
                       then primeiraRodada times
                       else rodada times

    if length timesNovaRodada > 1
        then do
            putStrLn ("---------" ++ " Nova Rodada " ++ "---------")
            putStrLn ("Times da Rodada: " ++ show timesNovaRodada)
            torneio timesNovaRodada
        else torneio timesNovaRodada

-- Calcula o total de jogos mata-mata
numJogosTorneio :: [t] -> Int
numJogosTorneio times = length times - 1