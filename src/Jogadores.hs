module Jogadores where

import System.IO (readFile, hFlush, stdout)
import Sorteador (Jogador(..))

-- Função para ler tudo numa mesma linha
prompt :: String -> IO String
prompt texto = do
    putStr texto
    hFlush stdout
    getLine

split :: Char -> String -> [String]
split _ [] = [""]
split sep (c:cs)
    | c == sep  = "" : resto
    | otherwise = (c : head resto) : tail resto
  where
    resto = split sep cs

formatarJogador :: String -> String
formatarJogador linha =
    case split ';' linha of
        [nome, estrelas, isSelect] ->
            marcador ++ " " ++ nome ++ "\t" ++ " ⭐ " ++ estrelas
          where
            marcador =
                if isSelect == "true"
                    then "( X )"
                    else "(   )"
        _ -> linha

listarJogadores :: IO ()
listarJogadores = do
    conteudo <- readFile "jogadores.txt"
    let ls = lines conteudo
    length ls `seq` return () -- resolve bug de arquivo não totalmente consumido

    putStrLn "\n--- Jogadores cadastrados ---"

    if null ls
        then putStrLn "Nenhum jogador cadastrado."
        else
            mapM_ putStrLn
                [ show i ++ ". " ++ formatarJogador linha
                | (i, linha) <- zip [1..] ls
                ]


salvarJogador :: String -> Int -> IO ()
salvarJogador nome estrelas =
    appendFile "jogadores.txt" (nome ++ ";" ++ show estrelas ++ ";true\n")


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
            estrelasStr <- prompt "Estrelas (1-10): "
            if read estrelasStr > 10
                then putStrLn "estrelas devem ser no máximo até 10"
            else do
                let estrelasInt = read estrelasStr :: Int
                salvarJogador nome estrelasInt
                cadastrarJogadores (contador + 1)



--- atualização de jogadores

toggleSelecao :: String -> String
toggleSelecao linha =
    case split ';' linha of
        [nome, estrelas, isSelect] ->
            nome ++ ";" ++ estrelas ++ ";" ++ novoValor
          where
            novoValor =
                if isSelect == "true"
                    then "false"
                    else "true"
        _ -> linha


toggleLinha :: [String] -> Int -> [String]
toggleLinha [] _ = []
toggleLinha xs i | i <= 0 = xs
toggleLinha (x:xs) 1 = toggleSelecao x : xs
toggleLinha (x:xs) i = x : toggleLinha xs (i - 1)



modificarSelecaoJogador :: Int -> IO ()
modificarSelecaoJogador indice = do
    conteudo <- readFile "jogadores.txt"
    let ls = lines conteudo
    length ls `seq` return ()  -- resolve bug de arquivo aberto
    let novoConteudo = toggleLinha ls indice
    writeFile "jogadores.txt" (unlines novoConteudo)


selecionarJogadores :: IO ()
selecionarJogadores = do 
    listarJogadores
    entrada <- prompt "Digite o índice (q para sair): "
    if entrada == "q"
        then putStrLn "Saindo da seleção..."
        else do 
            let indice = read entrada :: Int
            modificarSelecaoJogador indice
            putStr "\ESC[2J\ESC[H" -- limpar tela
            selecionarJogadores



-- pegar Jogadores

filtrarSelecionados :: [String] -> [String]
filtrarSelecionados =
    filter (\linha -> case split ';' linha of
                        [_, _, isSelect] -> isSelect == "true"
                        _               -> False)

parseJogador :: String -> Jogador
parseJogador linha =
    let [nome, estrelasStr, _] = split ';' linha
        estrelas = read estrelasStr :: Int
    in Jogador nome estrelas


getJogadoresSelecionados :: IO [Jogador]
getJogadoresSelecionados = do
    conteudo <- readFile "jogadores.txt"
    let linhasSelecionados = filtrarSelecionados (lines conteudo)
    return [parseJogador j | j <- linhasSelecionados]


