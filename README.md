# rdr-team-generator

O rdr-team-generator é um programa criado para resolver o balanceamento do sorteio de times no Racha dos Ruins (e também para outros rachas). Para isso, ele conta com duas funções de gerar times: sorteio rápido, onde é usado um algoritmo guloso que busca pela distribuição mais equilibrada de times, e o sorteio randômico, que gera milhares de combinações possíveis, filtra aquelas em que a distribução está desequilibrado e escolhe aleatoriamente uma das que sobraram.

O programa também conta com funções auxiliares para as outras necessidades do Racha dos Ruins, como o campeonato de pontos corridos e o mata-mata. 

## Como rodar o programa

* É necessário ter o [Haskell Stack](https://docs.haskellstack.org/en/stable/) instalado


Rode os seguintes comandos no terminal:
```bash
stack build
stack run
