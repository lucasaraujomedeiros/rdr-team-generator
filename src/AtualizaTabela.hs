type Linha = (String, Int, Int, Int, Int, Int)

arquivo :: FilePath
arquivo = "tabela.txt"

header :: String
header = "time;pontos;vitorias;partidas;golsPro;golsContra\n"

joinBy :: Char -> [String] -> String
joinBy _ []     = ""
joinBy _ [x]    = x
joinBy c (x:xs) = x ++ [c] ++ joinWith c xs

coletaValido :: (a -> Maybe b) -> [a] -> [b]
coletaValido _ [] = []
coletaValido f (x:xs) =
    case f x of
        Just v  -> v : coletaValido f xs
        Nothing -> coletaValido f xs

resultado :: Int -> Int -> (Int, Int)
resultado g a
    | g > a     = (3, 1)
    | g < a     = (0, 0)
    | otherwise = (1, 0)

atualizaTabela :: (String, String) -> (Int, Int) -> IO ()
atualizaTabela (t1, t2) (g1, g2) = do
    conteudo <- readFile arquivo
    let linhas = parseLinhas (lines conteudo)

    let (p1, v1) = resultado g1 g2
    let (p2, v2) = resultado g2 g1

    let linhas' =
            atualizaTime t2 g2 g1 p2 v2 $
            atualizaTime t1 g1 g2 p1 v1 linhas

    writeFile arquivo (header ++ unlines (map linhaToString linhas'))

atualizaTime :: String -> Int -> Int -> Int -> Int -> [Linha] -> [Linha]
atualizaTime nome gp gc pontos vitorias linhas =
    map atualiza linhas
  where
    atualiza (t,p,v,pa,gp',gc')
        | t == nome = (t, p+pontos, v+vitorias, pa+1, gp'+gp, gc'+gc)
        | otherwise = (t,p,v,pa,gp',gc')

parseLinhas :: [String] -> [Linha]
parseLinhas = coletaValido parseLinha . drop 1

parseLinha :: String -> Maybe Linha
parseLinha s =
    case splitBy ';' s of
        [t,p,v,pa,gp,gc] ->
            Just (t, read p, read v, read pa, read gp, read gc)
        _ -> Nothing

linhaToString :: Linha -> String
linhaToString (t,p,v,pa,gp,gc) =
    joinWith ';' [t, show p, show v, show pa, show gp, show gc]

splitBy :: Char -> String -> [String]
splitBy _ "" = [""]
splitBy c (x:xs)
    | x == c    = "" : rest
    | otherwise = (x : head rest) : tail rest
  where
    rest = splitBy c xs

