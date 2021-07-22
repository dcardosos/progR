library(tidyverse)
library(magrittr)
library(naniar)
library(patchwork)

# cadastro nacional de reclamações fundamentadas - sindec
df <- readr::read_csv('reclamacoes-fundamentadas-sindec-2016.csv')

df %<>%
  select(-c(
    AnoCalendario, CodigoRegiao, strNomeFantasia, NumeroCNPJ, 
    RadicalCNPJ, RazaoSocialRFB, NomeFantasiaRFB, CNAEPrincipal, 
    CodigoAssunto, CodigoProblema, CEPConsumidor))

# tem algum na?

df %>%
  naniar::any_na()

# proporcao de NA no df inteiro 
df %>%
  naniar::prop_miss_var()

# quantos tem
df %>%
  naniar::n_miss()

# summary na - na por coluna
df %>%
  naniar::miss_var_summary()

# replace
df %>%
  tidyr::replace_na(
    list(DescCNAEPrincipal = 'SEM DADO')) %>%
  naniar::any_na()


# claims groupby function

reclamacoes_por_col <- function(data, .col){

  data %>%
    group_by({{.col}}) %>%
    dplyr::summarise(reclamacoes = n()) %>%
    ggplot(aes(reclamacoes, reorder({{.col}}, reclamacoes))) +
    geom_bar(stat = 'identity', aes(fill = reclamacoes)) + 
    theme_minimal() +
    scale_fill_continuous(trans = 'reverse') +
    labs(
      x = 'Quantidade de reclamações',
      y = rlang::as_string(dplyr::ensym(.col)),
      title = glue::glue('Quantidade de reclamações por {rlang::as_string(dplyr::ensym(.col))}'),
      subtitle = '`Consumer Business Complaints in Brazil` dataset',
      caption = 'Fonte: dados.gov.br')
}

# claims por regiao

df %>%
  reclamacoes_por_col(Regiao)

# claims por estado
df %>%
  reclamacoes_por_col(UF)

# razoes sociais unicas

df %>%
  distinct(strRazaoSocial) # %>%
  # arrange(strRazaoSocial)

# claims que foram atentidas
df %>%
  reclamacoes_por_col(Atendida)

# claims por sexo
df %>%
  reclamacoes_por_col(SexoConsumidor)

## null values?
### N = não se aplica
df %>%
  distinct(SexoConsumidor)

### ue
df %>%
  filter(!is.null(SexoConsumidor)) %>%
  reclamacoes_por_col(SexoConsumidor)

# claims por faixa etária - sem tratamento
## procurar graficos de frequencia controlada
df %>%
  reclamacoes_por_col(FaixaEtariaConsumidor)

# descricao de problemas
df %>%
  distinct(DescricaoProblema) # mtos problemas pra plotar

# desc cnae principal
df %>%
  distinct(DescCNAEPrincipal) # mtos tbm

# tipo - pessoa juridica ou fisica: 1 ou 0

df %>%
  reclamacoes_por_col(Tipo)

# tratamento de dados

df %>%
  distinct(FaixaEtariaConsumidor)

dicionario <- c('entre 61 a 70 anos' = '61-70',
                'entre 31 a 40 anos' = '31-40',
                'entre 51 a 60 anos' = '51-60',
                'entre 21 a 30 anos' = '21-30',
                'entre 41 a 50 anos' = '41-50',
                'mais de 70 anos' = '+70',
                'Nao Informada' = 'NI',
                'até 20 anos' = ':20')

df %>%
  mutate(faixa_etaria = stringr::str_replace_all(
    string = FaixaEtariaConsumidor, 
    pattern = dicionario)) %>%
  distinct(faixa_etaria)


# lidando com as datas

df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>%
  ggplot(aes(TempoEspera, fill = Regiao)) +
  geom_density(alpha = .6) +
  theme_minimal() +
  
## histograma: entendendo outliers
df %>% 
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>%
  ggplot(aes(y = TempoEspera)) +
  geom_boxplot()

# vendo as estatísticas
df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>%
  summarise(
    media = mean(TempoEspera),
    mediana = median(TempoEspera),
    std = sd(TempoEspera),
    q25 = quantile(TempoEspera, 0.025),
    q95 = quantile(TempoEspera, 0.95),
    sd1_mean = mean(TempoEspera) + sd(TempoEspera),
    sd2_mean = mean(TempoEspera) + sd(TempoEspera) * 2)

## desconsiderando valores absurdos com quantil
df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>%
  mutate(TempoEsperaAbsurdo = TempoEspera %in% quantile(TempoEspera, 0.025):quantile(TempoEspera, 0.7)) %>%
  filter(TempoEsperaAbsurdo) %>%
  ggplot(aes(TempoEspera)) +
  geom_boxplot() 

## desconsiderando valores absurdos com desvio padrão
df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>%
  mutate(TempoEsperaAbsurdo = TempoEspera > quantile(TempoEspera, 0.25) & TempoEspera < (mean(TempoEspera) + sd(TempoEspera))) %>%
  select(TempoEsperaAbsurdo, TempoEspera) %>%
  filter(TempoEsperaAbsurdo) %>%
  ggplot(aes(TempoEspera)) +
  geom_boxplot()


## fazendo por regiao, por sexo, por uf, por atendida
df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>% 
  ggplot(aes(x = Regiao, y = TempoEspera)) +
  geom_boxplot() +
  theme_minimal()

#### norte tem variancia maior? SIMMM

df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>% 
  ggplot(aes(x = Regiao, y = TempoEspera, color = Atendida)) +
  geom_boxplot() +
  theme_minimal() +

gridExtra::tableGrob(
  df %>%
  mutate(
    DataAbertura = lubridate::as_date(DataAbertura),
    DataArquivamento = lubridate::as_date(DataArquivamento),
    TempoEspera = DataArquivamento - DataAbertura) %>% 
  group_by(Regiao) %>%
  dplyr::summarise(
    TempoMedioAtendimento = mean(TempoEspera),
    DesvPadraoAtendimento = sd(TempoEspera)) %>%
  arrange(desc(DesvPadraoAtendimento)))

# por regiao, por estado

df %>%
  group_by(Regiao, UF) %>%
  dplyr::summarise(quantidade = n(), .groups = 'drop') %>%
  ggplot(aes(UF, quantidade)) +
  geom_bar(stat = 'identity', aes(fill = Regiao)) +
  theme_minimal() +
  labs(
    x = 'UF',
    y = 'Quantidade de reclamações',
    title = 'Quantidade de reclamações por estado, por Região') +

# por regiao
df %>%
  group_by(Regiao, SexoConsumidor) %>%
  dplyr::summarise(quantidade = n(), .groups = 'drop') %>%
  filter(SexoConsumidor %in% c('F', 'M', 'N')) %>%
  ggplot(aes(Regiao, quantidade)) +
  geom_bar(stat = 'identity', aes(fill = SexoConsumidor), position= 'dodge') +
  theme_minimal() +
  labs(
    x = 'Região',
    y = 'Quantidade de reclamações',
    title = 'Quantidade de reclamações por Sexo, por Região') +

df %>%
  group_by(UF, SexoConsumidor) %>%
  dplyr::summarise(quantidade = n(), .groups = 'drop') %>%
  filter(SexoConsumidor %in% c('F', 'M')) %>%
  ggplot(aes(quantidade, UF)) +
  geom_bar(stat = 'identity', aes(fill = SexoConsumidor), position= 'dodge') +
  theme_minimal() +
  labs(
    x = 'Quantidade de reclamações',
    y = 'UF',
    title = 'Quantidade de reclamações por Sexo, por UF')



  


