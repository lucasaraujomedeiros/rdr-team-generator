type Linha = (String, Int, Int, Int, Int, Int)

arquivo :: FilePath
arquivo = "tabela.txt"

header :: String
header = "time;pontos;vitorias;partidas;golsPro;golsContra\n"

joinBy :: Char -> [String] -> String
joinBy _ []     = ""
joinBy _ [x]    = x
joinBy c (x:xs) = x ++ [c] ++ joinBy c xs

linhaToString :: Linha -> String
linhaToString (t,p,v,pa,gp,gc) =
    joinBy ';' [t, show p, show v, show pa, show gp, show gc]

linhaInicial :: String -> Linha
linhaInicial nome = (nome, 0, 0, 0, 0, 0)

criarTabela :: [String] -> IO ()
criarTabela times = do
    let linhas = map (linhaToString . linhaInicial) times
    writeFile arquivo (header ++ unlines linhas)

