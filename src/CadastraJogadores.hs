module CadastraJogadores where

import System.IO (hFlush, stdout)
import Sorteador (Jogador)

-- Função para ler tudo numa mesma linha
prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine

-- TODO: implementar uso do arquivo txt 
-- TODO: implementar listagem dos jogadores
-- TODO: implementar seleção de jogadores (recebe lista de nomes, e retorna um Jogador(nome, estrelas))

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
             -- aqui deve ser salvo nos arquivo txt 
            cadastrarJogadores (contador + 1)