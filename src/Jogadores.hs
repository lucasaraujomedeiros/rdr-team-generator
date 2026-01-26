module Jogadores where

import System.IO (readFile, hFlush, stdout)
import Sorteador (Jogador)

formatarJogador :: String -> String
formatarJogador linha =
    case break (== ';') linha of
        (nome, ';' : estrelas) ->
            nome ++ " - ⭐ " ++ estrelas
        _ -> linha 

listarJogadores :: IO ()
listarJogadores = do
    conteudo <- readFile "jogadores.txt"

    putStrLn "\n--- Jogadores cadastrados ---"

    if null conteudo
        then putStrLn "Nenhum jogador cadastrado."
        else mapM_ (putStrLn . formatarJogador) (lines conteudo)




-- Função para ler tudo numa mesma linha
prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine

salvarJogador :: String -> Int -> IO ()
salvarJogador nome estrelas =
    appendFile "jogadores.txt" (nome ++ ";" ++ show estrelas ++ "\n")


-- TODO: implementar uso do arquivo txt 
-- TODO: implementar listagem dos jogadores
-- TODO: implementar seleção de jogadores (recebe lista de nomes, e retorna um [Jogador(nome, estrelas)])

-- Construir lista com os jogadores cadastrados
cadastrarJogadores :: Int -> IO ()
cadastrarJogadores contador = do
    putStrLn ("\n--- Jogador " ++ show contador ++ " ---")
    
    nome <- prompt "Nome (Enter vazio para encerrar): "
    
    if null nome
        then putStrLn "Fim do cadastro..."
        else do
            estrelasStr <- prompt "Estrelas (1-5): "
            let estrelasInt = read estrelasStr :: Int
	    salvarJogador nome estrelasInt
            cadastrarJogadores (contador + 1)
