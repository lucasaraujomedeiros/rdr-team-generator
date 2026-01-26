module ListarJogadores where

import System.IO (readFile)

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

