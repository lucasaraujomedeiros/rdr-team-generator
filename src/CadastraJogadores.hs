module CadastraJogadores where

-- Função para ler tudo numa mesma linha
prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine


--- TODO implementar a adição no arquivo txt 


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