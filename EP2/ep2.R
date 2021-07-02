library(tidyverse)
df <- read_delim('ml-100k/u.data', delim = '\t', 
                 col_names = c('user_id', 
                               'item_id', 
                               'rating', 
                               'timestamp'))


matrix_na <- matrix(nrow = 943, ncol = 1682)

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

contaLinha(matrix_na)


## ex 4
contaColuna <- function(m){
  num_avaliacoes <- c()
  for (i in 1:ncol(m)){
    num_avaliacoes <- c(num_avaliacoes, length(which(!is.na(m[, i]))))  
  }
  num_avaliacoes
}

contaColuna(matrix_na)


## ex 5
mediaColuna <- function(m){
  media <- c()
  for (i in 1:ncol(m)){
    media <- c(media, mean(m[, i], na.rm = TRUE))
  }
  media
}

## ex6
mediaFilmes <- mediaColuna(matrix_na)


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
