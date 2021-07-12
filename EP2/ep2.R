#################################################################
## AO PREENCHER ESSE CABEÇALHO COM O MEU NOME E O MEU NÚMERO USP,
## DECLARO QUE SOU O ÚNICO AUTOR E RESPONSÁVEL POR ESSE PROGRAMA.
## TODAS AS PARTES ORIGINAIS DESSE EXERCÍCIO PROGRAMA (EP) FORAM
## DESENVOLVIDAS E IMPLEMENTADAS POR MIM SEGUINDO AS INSTRUÇÕES
## DESSE EP E QUE PORTANTO NÃO CONSTITUEM DESONESTIDADE
## ACADÊMICA OU PLÁGIO.
## DECLARO TAMBÉM QUE SOU RESPONSÁVEL POR TODAS AS CÓPIAS
## DESSE PROGRAMA E QUE EU NÃO DISTRIBUI OU FACILITEI A
## SUA DISTRIBUIÇÃO. ESTOU CIENTE QUE OS CASOS DE PLÁGIO E
## DESONESTIDADE ACADÊMICA SERÃO TRATADOS SEGUNDO OS CRITÉRIOS
## DIVULGADOS NA PÁGINA DA DISCIPLINA.
## ENTENDO QUE EPS SEM ASSINATURA NÃO SERÃO CORRIGIDOS E,
## AINDA ASSIM, PODERÃO SER PUNIDOS POR DESONESTIDADE ACADÊMICA.

## Nome : Lucas da Silva Kairoff
## NUSP : 11373518
## Turma: 22
## Prof.: Roberto Hirata Jr.

## Referências: 
### Slides do professor
#############################################################

# EX 1
df <- read.table('ml-100k/u.data', 
                 col_names = c('user_id', 
                               'item_id', 
                               'rating', 
                               'timestamp'))

dados <- read.csv('ml-100k/u.data', 
                  sep = '\t', 
                  header = FALSE,
                  col.names =  c('user_id', 
                                 'item_id', 
                                 'rating', 
                                 'timestamp'))

userid <- dados$user_id
itemid <- dados$item_id
rating <- dados$rating

matriz_with_na <- matrix(nrow = max(userid), ncol = max(itemid))

k <- 1
while(k <= nrow(dados)){
  matriz_with_na[dados[k, 1], dados[k, 2]] <- dados[k, 3]
  
  k <- k + 1
}

# EX 2
m <- matrix(nrow = 943, ncol = 1682)

for(index in 1:nrow(df)){                 
  m[df[index,1], df[index,2]] <- df[index,3]   # incrementando a matriz com os dados de df
}

# EX 3
contaLinha <- function(matrix){
  q_rating_user <- c()
  for (i in 1:nrow(matrix)){
    q_rating_user <- c(q_rating_user, length(which(!is.na(matrix[i, ])))) # onde o valor na linha da matriz for NA, retorna o índice e depois calcula o tamanho de todos esses índices, que saem em de which vetorizado
  }
  q_rating_user
}

# EX 4
contaColuna <- function(matrix){
  q_rating_movie <- c()
  for (i in 1:ncol(matrix)){
    q_rating_movie <- c(q_rating_movie, length(which(!is.na(matrix[, i]))))  
  }
  q_rating_movie
}

contaLinha(matriz_with_na)



##  4
contaColuna <- function(m){
  num_avaliacoes <- c()
  for (i in 1:ncol(m)){
    num_avaliacoes <- c(num_avaliacoes, length(which(!is.na(m[, i]))))  

# EX 5
## Criação de uma função que calcula a média
mean_without_na <- function(vector){
  sum_elements <- 0
  len_new_vector <- c()
  
  for(element in vector){
    if (!is.na(element)){
      len_new_vector <- len_new_vector + 1
      sum_elements <- sum_elements + element
    }
  }
  mean_vector <- sum_elements / len_new_vector
  mean_vector
}

contaColuna(matriz_with_na)


## ex 5
# mediaColuna <- function(m){
#   media <- c()
#   for (i in 1:ncol(m)){
#     media <- c(media, mean(m[, i], na.rm = TRUE))
#   }
#   media
# }

fazMedia <- function(vetor){
  
  soma <- 0   
  tamanho <- 0
  
  for (i in vetor){
    if(!is.na(i)){
      soma <- soma + i
      tamanho <- tamanho + 1
    }
  }
  
  (1 / tamanho) * soma
  
}


mediaColuna <- function(m){
  media <- c()
  for (i in 1:ncol(m)){
    media <- c(media, fazMedia(m[, i]))

## Função 
mediaColuna <- function(matriz){
  mean_m <- c()
  for (index in 1:ncol(matriz)){
    mean_m <- c(mean_m, mean_without_na(matriz[, index], na.rm = TRUE))
  }
  mean_m
}

## ex6
mediaFilmes <- mediaColuna(matriz_with_na)

# EX 6
mediaFilmes <- mediaColuna(matrix_na)

# EX 7 
u_item <- read_delim('ml-100k/u.item', '|',
                     col_names = c(
                       'movie_id',
                       'movie_title',
                       'release_date',
                       'video_release_date',
                       'IMDb_url',
                       'unknown',
                       'action',
                       'crime',
                       'adventure',
                       'animation',
                       'childrens',
                       'comedy',
                       'documentary',
                       'drama',
                       'romance',
                       'sci-fi',
                       'thriller',
                       'war',
                       'western'))

data <- u_item[, c('movie_id', 'movie_title')]

dados <- u_item[, c('movie_id', 'movie_title')]

nomeFilmes <- c()
for (index in dados$movie_id){
  if (mediaFilmes[index] >= 4.3){
    nomeFilmes <- c(nomeFilmes, dados$movie_title[index])  
  }
}

# EX 8
contaUsers <- contaLinha(m)

# EX 9
contaFilmes <- contaColuna(m)

# EX 10
hist(contaUsers)
hist(contaFilmes)

## Experimentando outros resultados
hist(contaUsers,  breaks = 10)
hist(contaUsers,  breaks = 100)

hist(contaFilmes,  breaks = 10)
hist(contaFilmes, breaks = 100)
