library(dplyr)
library(tidyr)


# Criando um objeto tbl
?tbl_df
flights <- tbl_df(hflights)


# FAZENDO UM SLICE NO DATASET - 3 maneiras
# Filtrando os dados com slice
flights[1,1]
flights[flights$Month == 1 & flights$DayofMonth == 1, ]
# condicao de filtro antes da virgula é para colunas
# condição depois da virgula é para coluna
#
# Aplicando filter do dplyr
filter(flights, Month == 1, DayofMonth == 1)
filter(flights, UniqueCarrier == "AA" | UniqueCarrier == "UA")
filter(flights, UniqueCarrier %in% c("AA", "UA"))
# pipe | é ou/or
#
# Select 
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))
# range de colunas: onde contem "Taxi" e contem "Dalay"


# ORGANIZAR DADOS
# Organizando os dados
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)
#
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)
#
head(with(flights, tapply(ArrDelay, Dest, mean, na.rm = TRUE)))
# pego tapply aplico mean na coluna ArrDelay e mostro com head
head(aggregate(ArrDelay ~ Dest, flights, mean))
# agrego pro cada um dos tipo de destino
#
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)
#
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort = TRUE)



# PROCURANDO VALORES NULOS
### is.na
# sapply para verificar o número de valores ausentes (missing) em cada coluna. 
sapply(dataset, function(x) sum(is.na(x)))
# remover todas as linhas com valores ausentes.
dataset <- dataset[complete.cases(dataset), ]
#na.omit is nicer for just removing all NA's. complete.cases allows partial selection 
#by including only certain columns of the dataframe:
final[complete.cases(final[ , 5:6]),]
#                    gene hsap mmul mmus rnor cfam
#2 ENSG00000199674    0    2    2    2    2
#4 ENSG00000207604    0   NA   NA    1    2
#6 ENSG00000221312    0    1    2    3    2



### ALTERANDO FATORES
### mapvalues
# mudar "No internet service" para "No" por seis colunas, que são: 
# "OnlineSecurity", "OnlineBackup", "DeviceProtection", "TechSupport", "streamingTV", 
# "streamingMovies".
# http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
# mapvalues(x, from = c("beta", "gamma"), to = c("two", "three"))
str(dataset)
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  dataset[,cols_recode1][,i] <- as.factor(mapvalues
                                          (dataset[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}
# mudar "No phone service" para "No" para a coluna “MultipleLines”
dataset$MultipleLines <- as.factor(mapvalues(dataset$MultipleLines, 
                                             from=c("No phone service"),
                                             to=c("No")))
# Alterar os valores na coluna “SeniorCitizen” de 0 ou 1 para “No” ou “Yes”.
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))


### MODIFICANDO O NIVEL DE FATORES - Modifying factor levels
#OnlineSecurity
dataset %>% count(OnlineSecurity)
library(forcats)
dataset2 <- dataset %>%
  mutate(OnlineSecurity = fct_recode(OnlineSecurity,
                                     "No"    = "No internet service")) %>%
  count(OnlineSecurity)
str(dataset2)
head(dataset2)
# https://r4ds.had.co.nz/factors.html#modifying-factor-levels


# REARRANJANDO FATORES
# agrupando em cinco grupos, factors a coluna (tenure) e criando uma nova coluna (group_tenure) 
#isto é tempo do contrato: 
# “0-12 Mês”, “12–24 Mês”, “24–48 Meses”, “48–60 Mês” Mês ”,“> 60 Mês”
# vendo o minimo e maximo primeiro
min(dataset$tenure); max(dataset$tenure)
# funçao para agrupar
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}
dataset$tenure_group <- sapply(dataset$tenure,group_tenure)
dataset$tenure_group <- as.factor(dataset$tenure_group)
View(dataset)


# REMOVENDO COLUNAS
# Remover as colunas que não precisamos para a análise.
churn$customerID <- NULL
churn$tenure <- NULL
View(churn)


# REMOVENDO ELEMENTOS DUPLICADOS
#Find and drop duplicate elements
x <- c(1, 1, 4, 5, 4, 6)
duplicated(x)
## [1] FALSE  TRUE FALSE FALSE  TRUE FALSE
#Extract duplicate elements
x[duplicated(x)]
## [1] 1 4
#If you want to remove duplicated elements, use !duplicated(), where ! is a logical negation
x[!duplicated(x)]
## [1] 1 4 5 6
# Remove duplicates based on Sepal.Width columns
my_data[!duplicated(my_data$Sepal.Width), ]
# https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/


# RECONHECENDO ELEMENTOS DUPLICADOS
#If you want to see which elements are duplicated and how many times you can use table
# checar primeiro que não existem cartão repetido
treino[duplicated(treino)]
teste[duplicated(teste)]
# ount the total number of duplicates
sum(table(teste$card_id)-1)
sum(table(treino$card_id)-1)


# COMBINANDO DATASETS
#Merging Data 
#Adding Columns 
# merge two data frames by ID
total <- merge(dataframeA,dataframeB,by="ID")
#Adding Rows 
#must have the same variables
total <- rbind(dataframeA, dataframeB) 
#https://www.statmethods.net/management/merging.html
# acrescentar coluna
dados <- cbind(dados, numeracao)
dados[, "numeracao"] <- numeracao


# NUMERO DE ELEMENTOS DE UMA VARIAVEL
# numero de elementos de uma coluna
length(cartoes$card_id)
str_length(unique(cartoes$card_id))
#https://stringr.tidyverse.org/reference/str_length.html




# MANUPULACAO DE DADOS
# CADA VARIAVEL EM UMA COLUNA
# CADA OBSERVAÇÃO EM UMA LINHA
readr # Importacao de ados
read_csv
read_csv2 
read_delim 
read_fwf
read_tsv
tidyr # Remodelagem de Dados
gather
spread
separate
unite
dplyr # Transformação de Dados
select
filter
groub_by
summarise
arrange
join
mutate


# CRIANDO UM TBL
?tbl_df
flights <- tbl_df(hflights)


# CONTAGEM E HISTOGRAMA
count(sono_df, cidade)
hist(sono_df$sono_total)


# AMOSTRAGEM ALEATORIA APARTIR DO DATASET
sample_n(sono_df, size = 10)


#SELECT
# select()
select(sono_df, nome)
# 2 colunas
sleepData <- select(sono_df, nome, sono_total)
# select range
select(sono_df, nome:cidade)


#FILTER
# filter()
filter(sono_df, sono_total >= 16)
filter(sono_df, sono_total >= 16, peso >= 80)
filter(sono_df, cidade %in% c("Recife", "Curitiba"))


#ARRANGE
# arrange() ordenar por coluna
sono_df %>% arrange(cidade) %>% head
#
sono_df %>% 
  select(nome, cidade, sono_total) %>%
  arrange(cidade, sono_total) %>% 
  filter(sono_total >= 16)
# ordem decrescente
sono_df %>% 
  select(nome, cidade, sono_total) %>%
  arrange(cidade, desc(sono_total)) %>% 
  filter(sono_total >= 16)

# MUTATE
# mutate() nova coluna
sono_df %>% 
  mutate(novo_indice = sono_total / peso) %>%
  head
#
sono_df %>% 
  mutate(novo_indice = sono_total / peso, 
         peso_libras = peso / 0.45359237) %>%
  head


# SUMMARIZE
# summarize()
sono_df %>% 
  summarise(media_sono = mean(sono_total))
#
sono_df %>% 
  summarise(media_sono = mean(sono_total), 
            min_sono = min(sono_total),
            max_sono = max(sono_total),
            total = n())


# GROUP_BY
# group_by() agrupando por cidade
sono_df %>% 
  group_by(cidade) %>%
  summarise(avg_sono = mean(sono_total), 
            min_sono = min(sono_total), 
            max_sono = max(sono_total),
            total = n())


# Operador: %>%
head(select(sono_df, nome, sono_total))
#
sono_df %>% 
  select(nome, sono_total) %>% 
  head
#
sono_df %>%
  mutate(novo_indice = round(sono_total * peso)) %>%
  arrange(desc(novo_indice)) %>%
  select(cidade, novo_indice)
sono_df
View(sono_df)


# Gravando em um novo dataframe
sono_df2 <- sono_df %>%
  mutate(novo_indice = round(sono_total * peso)) %>%
  arrange(desc(novo_indice)) %>%
  select(cidade, novo_indice)
View(sono_df2)



# GATHER
# Criando dados
set.seed(10)
df2 <- data.frame(
  id = 1:4,
  acao = sample(rep(c('controle', 'tratamento'), each = 2)),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)
df2
#
#
# reshape1
dados %>%
  gather(Regiao, NotaFinal, Regiao_A:Regiao_B)
# cria Regiao e NotaFinal como o range entre Regiao_A:Regiao_B
#
# Reshape2
df2_organizado1 <- df2 %>%
  gather(key, time, -id, -acao)
# criar 2 novas colunas key e time
# vai pegar todas colunas MENOS as colunas id e acao



# SEPARATE
# Reshape 
df2_organizado2 <- df2_organizado1 %>%
  separate(key, into = c("localidade", "tempo"), sep = "\\.") 
# separamos o T1 em outra coluna, colocamos work e home da coluna key em localidade 
# e T1 na coluna tempo


# Mais um exemplo
set.seed(1)
df3 <- data.frame(
  participante = c("p1", "p2", "p3", "p4", "p5", "p6"), 
  info = c("g1m", "g1m", "g1f", "g2m", "g2m", "g2m"),
  day1score = rnorm(n = 6, mean = 80, sd = 15), 
  day2score = rnorm(n = 6, mean = 88, sd = 8)
)
print(df3)
#
# Reshape dos dados
df3 %>%
  gather(day, score, c(day1score, day2score))
#
df3 %>%
  gather(day, score, c(day1score, day2score)) %>%
  spread(day, score)
#
df3 %>%
  gather(day, score, c(day1score, day2score)) %>%
  separate(col = info, into = c("group", "gender"), sep = 2)
#
df3 %>%
  gather(day, score, c(day1score, day2score)) %>%
  separate(col = info, into = c("group", "gender"), sep = 2) %>%
  unite(infoAgain, group, gender)
#
library(ggplot2)
df3 %>%
  gather(day, score, c(day1score, day2score)) %>%
  separate(col = info, into = c("group", "gender"), sep = 2) %>%
  ggplot(aes(x = day, y = score)) + 
  geom_point() + 
  facet_wrap(~ group) +
  geom_smooth(method = "lm", aes(group = 1), se = F)



# -=- RESHAPE
# Distribuindo os dados verticalmente (long)
?reshape
head(iris)
plot(iris)
iris_modif <- reshape(iris, 
                      varying = 1:4, 
                      v.names = "Medidas", 
                      timevar = "Dimensoes", 
                      times = names(iris)[1:4], 
                      idvar = "ID", 
                      direction = "long") 
# recebe o conjunto de dados como parametro iris
# variacao, quais colunas quero fazer a transformação
# nomes das novas colunas 
# coluna usada para fazer a variação em termo de mudanca
# direção
head(iris_modif)
View(iris_modif)

bwplot(Medidas ~ Species | Dimensoes, data = iris_modif)
# interação entre as variaveis Medidas e Species que vão interagir com uma variavel
# O resultado da relação entre as 2 primeiras voamos relaconar com a terceira

iris_modif_sp <- reshape(iris, 
                         varying = list(c(1,3),c(2,4)),
                         v.names = c("Comprimento", "Largura"), 
                         timevar = "Parte", 
                         times = c("Sepal", "Petal"),
                         idvar = "ID", 
                         direction = "long")
head(iris_modif_sp)
View(iris_modif_sp)

xyplot(Comprimento ~  Largura | Species, groups = Parte, 
       data = iris_modif_sp, auto.key = list(space="right"))
xyplot(Comprimento ~  Largura | Parte, groups = Species, 
       data = iris_modif_sp, auto.key = list(space="right"))

# tentativas com dply
#iris_dplyrzado <- iris %>%
#  gather(Length, Comprimento, -Species, -Sepal.Width, -Petal.Width)
#head(iris_dplyrzado)
library(lattice)
bwplot(Medidas ~ Species | Dimensoes, data = iris_dplyrzado)

#iris_dplyrzado_modfic <- iris %>%
#  gather(Dimensoes, Medidas, -Species) %>%
#  separate(col = Dimensoes, into = c("Sepal", "Petal"), sep = 5)
#head(iris_dplyrzado_modfic)
#View(iris_dplyrzado_modfic)

#library(stringr)
#iris_sem_pt1 <- gsub(".", "", iris_dplyrzado_modfic$Petal)
#iris_sem_pt <- str_replace_all(iris_dplyrzado_modfic$Petal, ".", "")
#head(iris_sem_pt)

# the columns have point using this code
point <- function(x) any(str_detect(x, "."))
iris_dplyrzado_modfic['Petal'] %>% summarize_all(list(point))


### RESHAPE2
library(reshape2)
# Criando um dataframe
df = data.frame(nome = c("Zico", "Pele"), 
                chuteira = c(40, 42),
                idade = c(34,NA),
                peso = c(93, NA),
                altura = c(175, 178))
df
# "Derretendo" o dataframe - Função melt() 
# Separando as variaveis por colunas
# informações em colunas individuais
# tRANSFORMANDO LINHAS EM COLUNAS
df_wide = melt(df, id = c("nome", "chuteira"))
df_wide
# Removendo NA
df_wide = melt(df, id = c("nome", "chuteira"), na.rm = TRUE)
df_wide
# "Esticando" o dataframe
# TRANSFORMANDO COLUNAS EM LINHAS
dcast(df_wide, formula = chuteira + nome ~ variable)
dcast(df_wide, formula = nome + chuteira  ~ variable)
dcast(df_wide, formula = nome  ~ variable)
dcast(df_wide, formula = ...  ~ variable)



# PARSE 
#https://readr.tidyverse.org/reference/parse_number.html
#Parse numbers, flexibly
# This drops any non-numeric characters before or after the first number. 
#The grouping mark specified by the locale is ignored inside the number
parse_number("$1000")
#> [1] 1000
parse_number("1,234,567.78")
#> [1] 1234568
# use the str_detect() function to see that 
# the columns have commas using this code
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(list(commas))
# use the str_replace_all function
# to remove them using this code
test_1 <-  str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric((test_1))
test_1
View(test_1)
# We can then use the mutate_all to apply this operation to each column,
#since it won't affect the columns without commas
#It turns out that this operation is so common
#removing commas that readr includes the function parse_number()
test_2 <- parse_number(murders_raw$population)
test_2
identical(test_1, test_2)
# obtain our desired table using the following code.
murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head
View(murders_new)


# NA.OMIT.data.table
#From data.table v1.12.2
#by Matt Dowle
#A data.table with just the rows where the specified columns have no missing value in any of them.
# default behaviour
na.omit(DT)
# Criando subsets dos dados carregados
cidadesBrasil <- subset(df, Country == 'Brazil')
cidadesBrasil <- na.omit(cidadesBrasil)
# https://www.rdocumentation.org/packages/data.table/versions/1.12.2/topics/na.omit.data.table


# PLYR
# # Split-Apply-Combine
library(plyr)
library(gapminder)
# Split-Apply-Combine
# Max lifeExp por continent
?ddply
# Split data frame, apply function, and return results in a data frame
# ddply(.data, .variables, .fun = NULL, ..., .progress = "none",
# .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
df <- ddply(gapminder, ~ continent, 
            summarize, 
            max_le = max(lifeExp))
str(df)
head(df)
View(df)
# quantidade em continent - factor
levels(df$continent)
# Split-Apply-Combine
# Quantidade paises (unicos) em continent
ddply(gapminder, ~ continent, 
      summarize, 
      n_uniq_countries = length(unique(country)))
# criando uma funcao para Quantidade paises (unicos) em continent
ddply(gapminder, ~ continent, 
      function(x) c(n_uniq_countries = length(unique(x$country))))
# min, max, medium
ddply(gapminder, ~ continent, 
      summarize,
      min = min(lifeExp), 
      max = max(lifeExp),
      median = median(gdpPercap))
# Usando um dataset do ggplot
library(ggplot2)
data(mpg)
str(mpg)
?mpg
# Trabalhando com um subset do dataset
data <- mpg[,c(1,7:9)]
str(data)
View(data)
# Sumarizando e Agregando Dados
ddply(data, .(manufacturer), 
      summarize, 
      avgcty = mean(cty))
# Várias funções em uma única chamada
ddply(data, .(manufacturer), 
      summarize, 
      avgcty = mean(cty), 
      sdcty = sd(cty), 
      maxhwy = max(hwy))
# Sumarizando os dados pela combinação de variáveis/fatores
ddply(data, .(manufacturer, drv), 
      summarize, 
      avgcty = mean(cty), 
      sdcty = sd(cty),
      maxhwy = max(hwy))


# DATA TABLE
# Instalando os pacotes
install.packages("data.table")
library(data.table)
# Criando 2 vetores
vec1 <- c(1, 2, 3, 4)
vec2 <- c('Vermelho', 'Verde', 'Marrom', 'Laranja')
# Criando um data.table
?data.table
dt1 <- data.table(vec1, vec2)
dt1
class(dt1)
# Slicing do data.table
dt2 <- data.table(A = 1:9, B = c("Z", "W", "Q"), C = rnorm(9), D = TRUE)
dt2
class(dt2)
dt2[1,1]
dt2[3:5,]
dt2[, .(B, C)]
# O Ponto . é para representar a sixtaxe
# function (..., .env = parent.frame())
#
# Aplicando função ao data.table
dt2[, .(Total = sum(A), Mean = mean(C))]
dt2[, plot(A, C)]
dt2[, .(MySum = sum(A)), by = .(Grp = A%%2)]
# Definindo valores por grupos
dt3 <- data.table(B = c("a", "b", "c", "d", "e", "a", "b", "c", "d", "e"), 
                  val = as.integer(c(6:10, 1:5)))
dt3
# Operações com data.tables
# SOMA DE VALORES AGRUPADO POR OUTRA COLUNA
dt4 <- data.table(A = rep(letters[2:1], each = 4L), 
                  B = rep(1:4, each = 2L), 
                  C = sample(8))
dt4
new_dt4 <- dt4[, sum(C), by = A]
new_dt4
class(new_dt4)
# ORDENANDO POR A
new_dt4[order(A)]
# SOMANDO B AGRUPADO POR A E ORDENADO POR A
# ABRIR E FECHAR O SLICE
new_dt4 <- dt4[, sum(B), by = A][order(A)]
new_dt4
#
# Iris
# CONVERTENDO IRIS PARA DATA.TABLE
dt5 <- as.data.table(iris)
dt5
#  MEDIUM POR SPECIES - SLICE
# Antes da virgula vazio retorna todas a linha.
# Para coluna calulo as median por Species
dt5[, .(Sepal.Length = median(Sepal.Length),
        Sepal.Width = median(Sepal.Width),
        Petal.Length = median(Petal.Length),
        Petal.Width = median(Petal.Width)),
    by = Species]
# O parâmetro .SD significa Subset Data e um subset é criado considerando a coluna Species e depois 
# é calculda a mediana. O resultado deve ser igual ao comando anterior. 
# O .SD faz parte da notação do pacote data.table.
# ESSE CODIGO É O MESMO ACIMA POREM SIMPLIFICADO
dt5[, lapply(.SD, median), by = Species]
# .SD 


#WEBSCRAPING
# Formatando os dados de uma página web
library(rvest)
library(stringr)
library(tidyr)
# Leitura da url abaixo e grave no objeto pagina
# http://forecast.weather.gov/MapClick.php?lat=42.31674913306716&lon=-71.42487878862437&site=all&smap=1#.VRsEpZPF84I
pagina <- read_html("http://forecast.weather.gov/MapClick.php?lat=42.31674913306716&lon=-71.42487878862437&site=all&smap=1#.VRsEpZPF84I")
# extraia o texto que contenha as tags:
# "#detailed-forecast-body b  e .forecast-text"
previsao <- html_nodes(pagina, "#detailed-forecast-body b , .forecast-text")
# Transforme o item anterior em texto
texto <- html_text(previsao)
# juntando com espaço entre as palavras
paste(texto, collapse = " ")


# MANIPULAÇÃO DE DADOS
# Extraímos a página web abaixo para você. Agora faça a coleta da tag "table"
# usando outra maneira
url <- 'http://espn.go.com/nfl/superbowl/history/winners'
pagina <- read_html(url)
tabela <- html_nodes(pagina, 'table')
class(tabela)
# Converta o item anterior em um dataframe
tab <- html_table(tabela)[[1]] # Buscando elemento em lista
class(tab)
head(tab)
View(tab)
# Remova as duas primeiras linhas e adicione nomes as colunas
tab <- tab[-(1:2), ]
head(tab)
# nomeando as colunas
names(tab) <- c("number", "date", "site", "result")
head(tab)
# Converta de algarismos romanos para números inteiros
tab$number <- 1:53
# convertendo data para o formato correto
tab$date <- as.Date(tab$date, "%B. %d, %Y")
head(tab)
#Divida a coluna RESULT  em  colunas com vencedores e perdedores
tab <- separate(tab, result, c('winner', 'loser'), 
                sep = ', ', remove = TRUE)
head(tab)
# Inclua mais 2 colunas com o score dos vencedores e perdedores
# Dica: Você deve fazer mais uma divisão nas colunas
# \\d digito e expressão regular
pattern <- " \\d+$"
# extrair o pattern da coluna vencedor e coloca em winnerscore
tab$winnerScore <- as.numeric(str_extract(tab$winner, pattern))
tab$loserScore <- as.numeric(str_extract(tab$loser, pattern))
# apos extrair e gravar os digitos nas novas colunas
# faço limpeza em winner e loser tirando espaço que ficou (miami 14) (miami ) (miami)
tab$winner <- gsub(pattern, "", tab$winner)
tab$loser <- gsub(pattern, "", tab$loser)
head(tab)
# Grave o resultado em um arquivo csv
write.csv(tab, 'superbowlnew.csv', row.names = F)
dir()


#Crie um dataset contendo os dados das 2 amostras 
library('nycflights13')
pop_data = na.omit(flights) %>% 
  filter(carrier == 'UA' | carrier == 'DL', arr_delay >= 0) %>%
  select(carrier, arr_delay) %>%
  group_by(carrier) %>%
  sample_n(17000) %>%
  ungroup()
# 1o omito os valores missing
# atraso => 0
# sample com 17 mil registros
amostra1 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'DL') %>%
  mutate(sample_id = '1') %>%
  sample_n(1000)
# adicionando coluna com valor 1 para DL
View(amostra1)
amostra2 = na.omit(pop_data) %>% 
  select(carrier, arr_delay) %>%
  filter(carrier == 'UA') %>%
  mutate(sample_id = '2') %>%
  sample_n(1000)
# adicionando coluna com valor 2 para UA
View(amostra2)
samples = rbind(amostra1,amostra2)
# ligacao por linhas
View(samples)


#Intervalo de confiança (95%) da amostra1
# Usamos a fórmula: erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))
# nossa amostra é aleatória, normal, independente
# A condição para usar isso como 
# Como isso é apenas uma estimativa, é chamado de erro padrão. A condição para usar isso como 
# uma estimativa é que o tamanho da amostra n é maior que 30 (dado pelo teorema do limite central) 
# e atende a condição de independência n <= 10% do tamanho da população.
# Erro padrão
erro_padrao_amostra1 = sd(amostra1$arr_delay) / sqrt(nrow(amostra1))
# Limites inferior e superior
# 1.96 é o valor de z score para 95% de confiança
lower = mean(amostra1$arr_delay) - 1.96 * erro_padrao_amostra1  
upper = mean(amostra1$arr_delay) + 1.96 * erro_padrao_amostra1
# Intervalo de confiança
ic_1 = c(lower,upper)
# Media
mean(amostra1$arr_delay)
ic_1
#Calcule o intervalo de confiança (95%) da amostra2
erro_padrao_amostra2 = sd(amostra2$arr_delay) / sqrt(nrow(amostra2))
lower = mean(amostra2$arr_delay) - 1.96 * erro_padrao_amostra2
upper = mean(amostra2$arr_delay) + 1.96 * erro_padrao_amostra2
ic_2 = c(lower,upper)
mean(amostra2$arr_delay)
ic_2
#plot Visualizando os intervalos de confiança criados nos itens anteriores
# Dica: Use o geom_point() e geom_errorbar() do pacote ggplot2
toPlot = summarise(group_by(samples, sample_id), mean = mean(arr_delay))
toPlot = mutate(toPlot, lower = ifelse(toPlot$sample_id == 1,ic_1[1],ic_2[1]))
toPlot = mutate(toPlot, upper = ifelse(toPlot$sample_id == 1,ic_1[2],ic_2[2]))
ggplot(toPlot, aes(x = sample_id, y=mean, colour = sample_id )) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.1)









































