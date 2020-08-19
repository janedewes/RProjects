

#********************** Mineração de Regra de Associação ***********************


# O Market Basket Analysis (MBA) (ou Mineração de Regras de Associação) 
# ainda pode ser uma técnica muito útil para obter insights em grandes conjuntos de dados transacionais.


# COMPREENDER O PROBLEMA------------------------------------------------------------------------------------------------------
# PERGUNTA / PROBLEMA A SER RESOLVIDO:
# Neste estudo, buscar a associação entre os clubes de futebol da Europa e responder a pergunta:
# Quais clubes mais realizam transações de compra e venda de jogadores, entre si?

# dataset oferecido pelo Kaggle: https://www.kaggle.com/hugomathien/soccer
# O dataset contém cerca de 25.000 partidas de onze ligas de futebol europeias a partir da temporada 2008/2009 
# até a temporada 2015/2016.

# 1) Data Wrangling (manipulação dos dados)
# 2) Gerar um conjunto de dados transacionais adequado para análise (compra e venda de atletas) 

# Encontrar padrões nesse database (conferir termos de uso)


# COMPREENDER OS DADOS -------------------------------------------------------------------------------------------------------
# Em R usar o pacote arules para MINERAÇÃO de regras de associação / MBA. 
# Alternativamente, quando a ordem das transações [e] importante, usar o pacote arulesSequences. 
# Todas as regras que obtemos da mineração de regras de associação formam um gráfico de rede. 
# Os clubes de futebol individuais são os nós do gráfico e cada regra "de ==> para" é uma aresta (edge) do gréfico de rede.
# Em R, os gráficos de rede podem ser visualizados bem por meio do pacote visNetwork. 



# def dir
setwd("C:/Users/paloma/Desktop/GitHub/R/05-TrabPratico3")
getwd()


# Pacotes
# Os dados estão do B.D. SQLite
install.packages("RSQLite")
# Pacotes para manipular os dados:
install.packages("dplyr")
install.packages("tidyr")
# Pacotes para o algoritmo de regra de associação:
install.packages("arules")
install.packages("arulesSequences")
# Pacote para leitura dos dados e gravação dos dados em arq txt:
install.packages("readr")
# Pacotes para viualização dos dados 
install.packages("visNetwork") #grafico de rede
install.packages("igraph") #suporte para o visNetwork
# Pacote para manipulação de data:
install.packages("lubridate")
# Pacote para criar uma tabela de resumo do trabalho
install.packages("DT")

library(RSQLite)
library(dplyr)
library(tidyr)
library(arules)
library(arulesSequences)
library(readr)
library(stringr)
library(visNetwork)
library(igraph)
library(lubridate)
library(DT)



# OBS: ANTES DE INICIAR O PROCESSAMENTO, ESTUDAR AS TABELAS!!!


# PREPARAÇÃO DOS DADOS  ----------------------------------------------------------------------------------------------------

# Carregar o conjunto de dados (Os dados estão do B.D. SQLite)
# Conectar ao banco de dados 
con = dbConnect(RSQLite::SQLite(), dbname="database.sqlite")


# Obter a lista de tabelas (do database)
alltables = dbListTables(con)
alltables


# Extrair as tabelas (conferir as tabelas no site do databse)
# gravar as tabelas em dataframes:
players       = dbReadTable(con, "Player")
players_stats = dbReadTable(con, "Player_Attributes")
teams         = dbReadTable(con, "Team")
league        = dbReadTable(con, "League")
Matches       = dbReadTable(con, "Match")


View(players)
View(players_stats)
View(teams)
View(league)
View(Matches)


# Substituir espaço(\\s) por _(underline) nos nomes muito longos
teams$team_long_name = str_replace_all(teams$team_long_name, "\\s", "_")
# substituir"." por "_"
teams$team_long_name = str_replace_all(teams$team_long_name, "\\.", "_")
# substituir"-" por "_"
teams$team_long_name = str_replace_all(teams$team_long_name, "-", "_")
View(teams)



# MANIPULAÇÃO DOS DADOS ----------------------------------------------------------------------------------------------------

# Agrupar as equipes por país
CountryClub = Matches %>% 
  group_by(home_team_api_id,country_id) %>% 
  summarise(n=n()) %>% 
  left_join(league) %>%
  left_join(teams, by=c("home_team_api_id" = "team_api_id"))
# obs: %>% = notação do dplyr



# Preparação dos dados para mineração das regras de associação

# Os jogadores estão em colunas separadas, mas precisam estar empilhados em uma coluna
tmp = Matches %>% 
  select(
    season, 
    home_team_api_id, 
    home_player_1:home_player_11
  )%>%
  gather(
    player, 
    player_api_id, 
    -c(season, home_team_api_id)
  ) %>%
  group_by(player_api_id, home_team_api_id ) %>% 
  summarise(season = min(season))
# Obs: acima como uma fç sql


# Unindo dados de jogador e clube
playerClubSequence = left_join(
  tmp,
  players
) %>% 
  left_join(
    teams, 
    by=c("home_team_api_id"="team_api_id")
  )

playerClubSequence = playerClubSequence %>% 
  filter(
    !is.na(player_name), !is.na(team_short_name)
  )  %>%
  arrange(
    player_api_id, 
    season
  )
# Obs: acima como uma uma fç sql

# Adicionando um número sequencial por jogador (para criar um ID único, com a fç ave())
playerClubSequence$seqnr = ave( playerClubSequence$player_api_id, playerClubSequence$player_api_id, FUN = seq_along)
playerClubSequence$size = 1



# Mineração de sequências com algoritmo cSPade do pacote arulesSequences

# Grava o conjunto de dados em um arq txt para facilitar a manipulação
# da fç read_basket em arulesSequence para criar um objeto de transação
write_delim( 
  playerClubSequence %>% select( c(player_api_id, seqnr, size, team_long_name)) ,
  delim ="\t", path = "player_transactions.txt", col_names = FALSE
)



# Importar as transações registradas no item anterior (ler com a fç read_baskets)
playerstrxs <- read_baskets("player_transactions.txt", sep = "[ \t]+",info =  c("sequenceID","eventID","size"))
summary(playerstrxs)



# Executar mineração de seq, por enquanto apenas com comprimento de duas seqs
# a fça que faz a mineração é a cspade():
?cspade


playersClubSeq <- cspade(
  playerstrxs, 
  parameter = list(support = 0.00010, maxlen=2), 
  control   = list(verbose = TRUE)
)

summary(playersClubSeq)


# Até aqui já foram encontradas as sequencias (já encontramos os padrões) -----------------------------





# Adiante plotar os padrões encontrados ----------------------------------------------------------------

# Visualização -----------------------------------------------------------------------------------------

# Fazer Data Wrangling(manipualção) para colocar os resultados do cspade em um organizado conjunto de dados 
# que é adequado para a visNetwork. A visNetwork precisa de dois conjuntos de dados:
# um conjunto de dados com as arestas "de --> para" e um conjunto de dados com os nós exclusivos
seqResult = as(playersClubSeq, "data.frame")
seqResult = seqResult %>% 
  mutate(
    sequence = as.character(sequence)
  )

seqResult = bind_cols(
  seqResult,
  as.data.frame(
    str_split_fixed(seqResult$sequence, pattern =",", 2), 
    stringsAsFactors = FALSE)
)

seqResult$from = str_extract_all(seqResult$V1,"\\w+", simplify = TRUE)[,1] 
seqResult$to   = str_extract_all(seqResult$V2,"\\w+",simplify = TRUE)[,1]


seqResult$width = exp(3000*seqResult$support)
seqResult = seqResult %>% filter(V2 !="")
seqResult$title = paste(seqResult$sequence, "<br>", round(100*seqResult$support,2), "%")

seqResult$support_perc = paste(sprintf("%.4f", 100*seqResult$support), "%")


# Criar o dataframe com os nodes
nodes = unique(c(seqResult$from, seqResult$to))
nodesData = data.frame(id = unique(nodes), title = unique(nodes), label = unique(nodes), stringsAsFactors = FALSE) %>%
  left_join(CountryClub, by = c("id"="team_long_name")) %>% 
  rename(group = name)

View(nodes)



# Calcula as medidas de centralidade de betweeness (se um determinado nó é central em relação aos nós em sua volta)
# usando o igraph, para que ter tamanhos diferentes de nós no gráfico de rede 
transferGraph = graph_from_data_frame(seqResult[,c(5,6)], directed = TRUE)


tmp = betweenness(transferGraph)
# Colocar tudo em um dataframe:
Clubs_betweenness = data.frame(id = names(tmp), value = tmp, stringsAsFactors = FALSE)
nodesData = nodesData %>% 
  left_join(Clubs_betweenness) %>%
  mutate(title = paste(id, "betweeness ", round(value))) %>%
  arrange(id)




# Criando a rede interativa

# Preparando o dataframe final e removendo duplicidades
nodes = nodesData
nodes = nodes[!duplicated(nodes$id),]


# Criar a rede
?visNetwork

visNetwork(nodes, edges = seqResult, width = 900, height = 700) %>%
  visNodes(size = 10) %>%
  visLegend() %>%
  visEdges(smooth = FALSE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visInteraction(navigationButtons = TRUE) %>%
  visEdges(arrows = 'from') %>%
  visPhysics(
    solver = "barnesHut",
    maxVelocity = 35,
    forceAtlas2Based = list(gravitationalConstant = -6000)
  )



# Criar a tabela final para suportar a análise
seqResult$Ntransctions = seqResult$support*10542
DT::datatable(
  seqResult[,c(5,6,9,10)], 
  rownames = FALSE,
  options = list(
    pageLength=25)
)



# Fim 







