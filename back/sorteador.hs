{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (sortBy, minimumBy)
import Data.Ord (comparing, Down(Down))


data Jogador = Jogador {
    nomeJogador :: String,
    estrelas :: Int
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Time = Time {
    nomeTime :: String,
    elenco :: [Jogador],
    forcaTotal :: Int
} deriving (Show, Generic, ToJSON, FromJSON)


data InputDados = InputDados {
    qtdTimes :: Int,
    maxPorTime :: Int,
    jogadores :: [Jogador]
} deriving (Show, Generic, ToJSON, FromJSON)


montarTimesEquilibrados :: Int -> Int -> [Jogador] -> [Time]
montarTimesEquilibrados qtdTimes maxPorTime jogadores =
    let
        jogadoresOrdenados = sortBy (comparing (Down . estrelas)) jogadores
        inicializarTimes = map (\n -> Time { nomeTime = "Time " ++ show n, elenco = [], forcaTotal = 0 }) [1..qtdTimes]

        assignPlayer :: Jogador -> [Time] -> [Time]
        assignPlayer jog timesList =
            let availIdxs = [ i | (t,i) <- zip timesList [0..], length (elenco t) < maxPorTime ]
            in if null availIdxs
               then timesList 
               else
                   let candidates = [ (forcaTotal (timesList !! i), length (elenco (timesList !! i)), i) | i <- availIdxs ]
                       (_, _, idx) = minimumBy (comparing (\(ft,len,i) -> (ft,len,i))) candidates
                       (before, t:after) = splitAt idx timesList
                       t' = t { elenco = elenco t ++ [jog], forcaTotal = forcaTotal t + estrelas jog }
                   in before ++ (t' : after)

        timesFinal = foldl (flip assignPlayer) inicializarTimes jogadoresOrdenados

    in timesFinal

main :: IO ()
main = do
    
    inputRaw <- B.getContents
    

    case decode inputRaw :: Maybe InputDados of
        Just dados -> do
            
            let resultado = montarTimesEquilibrados (qtdTimes dados) (maxPorTime dados) (jogadores dados)
            
            B.putStr (encode resultado)
            
        Nothing -> do
            
            BC.putStrLn "{\"erro\": \"Falha ao ler JSON no Haskell. Verifique o formato enviado.\"}"