library(tidyverse)
df <- read_delim('ml-100k/u.data', delim = '\t', 
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


for (i in 1:943){
  for (j in 1:1682){
    rating <- df[df$user_id == i & df$item_id == j, 'rating'][[1]]
    
    if (length(rating) != 0){
      matrix_na[i, j] <- rating
    } 
  }
}


## ex 3
contaLinha <- function(m){
  num_avaliacoes <- c()
  for (i in 1:nrow(m)){
    num_avaliacoes <- c(num_avaliacoes, length(which(!is.na(m[i, ]))))  
  }
  num_avaliacoes
}

contaLinha(matriz_with_na)



##  4
contaColuna <- function(m){
  num_avaliacoes <- c()
  for (i in 1:ncol(m)){
    num_avaliacoes <- c(num_avaliacoes, length(which(!is.na(m[, i]))))  
  }
  num_avaliacoes
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
  }
  media
}


## ex6
mediaFilmes <- mediaColuna(matriz_with_na)


##ex 7 
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

nomeFilmes <- c()
for (i in data$movie_id){
  if (mediaFilmes[i] >= 4.3){
    nomeFilmes <- c(nomeFilmes, data$movie_title[i])  
  }
}

## ex8
contaUsers <- contaLinha(matrix_na)

## ex9
contaFilmes <- contaColuna(matrix_na)

## ex10

hist(contaUsers)
hist(contaFilmes)


hist(contaUsers,  breaks = 100)
hist(contaFilmes, breaks = 100)
