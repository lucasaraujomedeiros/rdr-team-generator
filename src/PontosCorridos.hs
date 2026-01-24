module PontosCorridos where
-- adiciona resultado numa classificacao

criaTupla :: a -> b -> (a, b)
criaTupla x y = (x, y)


-- gera as possibilidades
listaJogos :: [String] -> [(String, String)] 
listaJogos [] = []
listaJogos (x:xs) = map (criaTupla x) xs ++ listaJogos xs


-- rearranja a lista 
ordenaJogos :: [(String, String)] -> [(String, String)]
ordenaJogos [] = []
ordenaJogos [x] = [x]
ordenaJogos (x:xs) = [x] ++ [last xs] ++ ordenaJogos (init xs)


-- funcao para gerar os jogos numa ordem boa
defineJogos :: [String] -> [(String, String)]
defineJogos times = ordenaJogos (listaJogos times)




-- recebe os jogos em ordem, tira o primeiro jogo, adiciona na tabela o rsltd e retorna os prox jogos
