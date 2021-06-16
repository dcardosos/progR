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

## Nome :
## NUSP :
## Turma:
## Prof.: Roberto Hirata Jr.

## Referências: 
### https://stackoverflow.com/questions/40721960/insertion-sort-algorithm-in-r
### https://www.comp.nus.edu.sg/~stevenha/cs1020e/lectures/L10%20-%20Sorting.pdf
#################################################################

## Programa
## Exercício 1
### Leitura dos dados
data <- read.csv('MAC0113-EP1.csv', sep=';')

### Printando as cinco primeiras linhas
head(data)

## Exercício 2
### Definindo o vetor de circunferência
circunferencia <- data[,2]

### Definindo o vetor de diâmetro
diametro <- data[,3]

## Exercício 3
### Cálculo do pi de acordo com a fórmula: pi = circunferência / diâmetro
meupi <- c() # vetor  vazio

i <- 1       # contador

len_observacoes <- length( circunferencia ) # quantidade de linhas na tabela

while ( i <  len_observacoes + 1 ){    # +1 para incluir o elemento na posição 600
  
  meupi <- c( meupi, circunferencia[i] / diametro[i] ) # aplicação da fórmula e adicionando ao vetor inicialmente vazio 
  
  i = i + 1  # somando 1 ao contador para evitar looping infinito
  
}

### Printando o resultado
cat('Vetor meupi:', meupi)

### Verificando se calculou para todas as linhas
cat('Tamanhos iguais? TRUE ou FALSE? É' ,
    length ( meupi ) == length ( circunferencia ) )

## Exercício 4
### Cálculo a soma da variável meupi
soma <- 0   # soma em zero para ir somando os valores de meupi depois
n <- 1      # contador

while ( n < len_observacoes + 1){
  
  soma <- soma + meupi[n]       # agregando cada elemento de meupi 
  
  n <- n + 1                    # atualizando o contador
}

### Calculando o piMedio: (1 / número de observações ) * soma 
piMedio <- ( 1 / len_observacoes ) * soma

# Printando o piMedio
cat('Valor do piMedio:', piMedio)

## Exercício 5
soma_desvio_ao_quadrado <- 0   # vai ser o numerador da fórmula dada
p = 1                          # contador

while ( p < len_observacoes + 1) {
  
  soma_desvio_ao_quadrado <- soma_desvio_ao_quadrado + ( ( meupi[p] - piMedio )^2 )
  
  p <- p + 1
  
}

### Cálculo do piVar: (1 / (N - 1)) * Soma ( meupi[i] - piMedio)^2
piVar <- ( 1 / (len_observacoes - 1) ) * soma_desvio_ao_quadrado

### Printando o piVar
cat('Valor do piVar:', piVar)

## Exercício 6
### Gráfico da circunferência em função do diâmetro
#### valores muito altos! a variância chega perto de 1, isso explica o pi
#### com uma diferença de 0.1241 (3.2656 - 3.1415).

plot(x = diametro,               # abscissa
     y = circunferencia,         # coordenada
     ylab = 'Circunferência (cm)', 
     xlab = 'Diâmetro (cm)',
     main = 'Distribuição dos dados - Pi médio de 3.2656')

## Exercício 7
### Há muitas dispersões do pi, pra cima e pra baixo, tem pi que chega a 
### valores de quase 12, um desvio de quase 8 da média
plot(meupi, 
     ylab='Pi',
     xlab = 'Índices', 
     main = 'Pi calculado a partir da divisão do diâmetro pela circunferência')


## Exercício 8 
### Se usar a mediana, evita a influência da variância nos dados
### Primeiro foi utilizado um algortimo de ordenação dos vetores para poder
### calcular a mediana a partir dos dados ordenados em ordem crescente
meupi_ordenado <- c()

for (j in 2:len_observacoes) {     # começa do segundo elemento
  proximo = meupi[j]                     
  m = j - 1                        # define m sendo o elemento anterior ao j
  
  while (m > 0 && A[m] > proximo) {    # enquanto o elemento anterior for maior que zero
                                   # e maior que o elemento posterior    
    
    meupi_ordenado[(m + 1)] = A[m] # insere no vetor meupi_ordenado esse elemento
                                   # na posição posterior a dele (m + 1)
    m = m - 1 
  }
  meupi_ordenado[(m + 1)] = proximo # se o elemento for menor que zero ou maior
                                    # que o elemento anterior, ele está no lugar certo
}

### Como o tamanho das observações é par, pega-se os dois elmentos medianos
### na posição 300 e 301, soma e divide por 2 (faz a média deles)
medianaUm <- meupi_ordenado[len_observacoes / 2]
medianaDois <- meupi_ordenado[len_observacoes / 2 + 1]
piMediano <- (medianaUm + medianaDois) / 2

cat('Valor do piMediano:', piMediano)
