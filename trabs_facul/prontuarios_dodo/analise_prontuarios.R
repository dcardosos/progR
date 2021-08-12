library(patchwork)
library(magrittr)
library(ggplot2)

# LENDO OS DADOS -----------------------------------------
dados <- readr::read_csv('dados.csv')
dplyr::glimpse(dados)


# FUNÇÕES -------------------------------------------------
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# ANÁLISE ------------------------------------------------------

## porcentagem de prontuários por sexo
dados %>% 
  dplyr::filter(!is.na(GÊNERO), DATA == 2019) %>% 
  dplyr::group_by(GÊNERO) %>%
  dplyr::summarise(quantidade =  dplyr::n()) %>% 
  dplyr::mutate(pct = (quantidade / purrr::reduce(quantidade, `+`)) * 100) %>%
  dplyr::mutate(pctg = glue::glue("{round(pct, 2)}%") %>% stringr::str_replace("[.]", ",")) %>% 
  ggplot(aes(x="", y = quantidade, fill = GÊNERO)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = c(250, 80), label = quantidade), color = "white", size = 6) +
  labs(
    title = "Porcentagem de gênero dos prontuários - 2019",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`") +

dados %>% 
  dplyr::filter(!is.na(GÊNERO), DATA != 2019) %>% 
  dplyr::group_by(GÊNERO) %>%
  dplyr::summarise(quantidade =  dplyr::n()) %>% 
  dplyr::mutate(pct = (quantidade / purrr::reduce(quantidade, `+`)) * 100) %>%
  dplyr::mutate(pctg = glue::glue("{round(pct, 2)}%") %>% stringr::str_replace("[.]", ",")) %>% 
  ggplot(aes(x="", y = quantidade, fill = GÊNERO)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = c(550, 200), label = quantidade), color = "white", size = 6) +
  labs(
    title = "Porcentagem de gênero dos prontuários - 2020 e 2021",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

## Estatísticas básicas sobre as mulheres
dados %>%
  dplyr::filter(GÊNERO == "F") %>%
  dplyr::summarise(MÉDIA = mean(IDADE, na.rm = TRUE),
                   MEDIANA = median(IDADE, na.rm = TRUE),
                   MODA = mode(IDADE),
                   "DESVIO PADRÃO" = sd(IDADE, na.rm = TRUE)) 


## Mulheres que relataram agressão física, acidente doméstico, agressão fisica doméstica
## 2019 - PORCENTAGEM
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA == 2019,
                ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::group_by(ETIOLOGIA) %>%
  dplyr::summarise(QUANTIDADE = dplyr::n()) %>% 
  dplyr::mutate(pct = (QUANTIDADE / purrr::reduce(QUANTIDADE, `+`)) * 100) %>%
  dplyr::mutate(pctg = glue::glue("{round(pct, 2)}%") %>% stringr::str_replace("[.]", ",")) %>% 
  ggplot(aes(x="", y = QUANTIDADE, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = c(10, 8.5, 4), label = pctg), color = "white", size = 6) +
  labs(
    title = "Divisão em grupos das mulheres que constaram sofrer agressão - 2019",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")
  

## 2019 - QUANTIDADE
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA == 2019,
                ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::group_by(ETIOLOGIA) %>%
  dplyr::summarise(QUANTIDADE = dplyr::n()) %>% 
  dplyr::mutate(pct = (QUANTIDADE / purrr::reduce(QUANTIDADE, `+`)) * 100) %>%
  dplyr::mutate(pctg = glue::glue("{round(pct, 2)}%") %>% stringr::str_replace("[.]", ",")) %>% 
  ggplot(aes(x="", y = QUANTIDADE, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = c(10, 8.5, 4), label = QUANTIDADE), color = "white", size = 6) +
  labs(
    title = "Divisão em grupos das mulheres que constaram sofrer agressão - 2019",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

## 2020 e 2021 - PORCENTAGEM
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA != 2019,
                ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::group_by(ETIOLOGIA) %>%
  dplyr::summarise(QUANTIDADE = dplyr::n()) %>% 
  dplyr::mutate(pct = (QUANTIDADE / purrr::reduce(QUANTIDADE, `+`)) * 100) %>%
  dplyr::mutate(pctg = glue::glue("{round(pct, 2)}%") %>% stringr::str_replace("[.]", ",")) %>% 
  ggplot(aes(x="", y = QUANTIDADE, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()  +
  ggrepel::geom_label_repel(aes(y = c(29.5, 28.3, 20), label = pctg),
                            size = 6,
                            nudge_x = 1,
                            show.legend = FALSE,
                            segment.color = c("#F9766E", "#01BA38","#619DFF"),
                            color = "white") +
  guides(fill = guide_legend(title = "Etiologia")) +
  labs(
    title = "Divisão em grupos das mulheres que constaram sofrer agressão - 2020/2021",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

## 2020/2021 - QUANTIDADE
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA != 2019,
                ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::group_by(ETIOLOGIA) %>%
  dplyr::summarise(QUANTIDADE = dplyr::n()) %>% 
  dplyr::mutate(pct = (QUANTIDADE / purrr::reduce(QUANTIDADE, `+`)) * 100) %>%
  dplyr::mutate(pctg = glue::glue("{round(pct, 2)}%") %>% stringr::str_replace("[.]", ",")) %>% 
  ggplot(aes(x="", y = QUANTIDADE, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(y = c(29.5, 28, 14),
                label = QUANTIDADE), color = "white", size = 6) +
  labs(
    title = "Divisão em grupos das mulheres que constaram sofrer agressão - 2020/2021",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

## AGRESSÕES PELO TOTAL
## 2019 - PORCENTAGEM
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA == 2019, !is.na(ETIOLOGIA)) %>%
  dplyr::group_by(ETIOLOGIA) %>% 
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::mutate(TOTAL = sum(N)) %>% 
  dplyr::filter(ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::mutate(PORCENTAGEM = N / TOTAL * 100) %>%
  dplyr::select(-TOTAL) %>%
  dplyr::add_row(ETIOLOGIA = "Outras", N = 116, PORCENTAGEM = 91.34) %>% 
  dplyr::mutate(STRING_PCT = paste0(round(PORCENTAGEM, 2), "%")) %>% 
  ggplot(aes(x="", y = N, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()  +
  ggrepel::geom_label_repel(aes(y = c(126.5, 124.5, 116.3, 50), label = STRING_PCT),
                            size = 6,
                            nudge_x = 1,
                            show.legend = FALSE,
                            segment.color = c("#F9766E", "#01BA38","#619DFF", "#C77BFF"),
                            color = "white") +
  guides(fill = guide_legend(title = "Etiologia")) +
  labs(
    title = "Mulheres que constaram agressão no total de prontuários - 2019",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

## 2019 - QUANTIDADE
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA == 2019, !is.na(ETIOLOGIA)) %>%
  dplyr::group_by(ETIOLOGIA) %>% 
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::mutate(TOTAL = sum(N)) %>% 
  dplyr::filter(ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::mutate(PORCENTAGEM = N / TOTAL * 100) %>%
  dplyr::select(-TOTAL) %>%
  dplyr::add_row(ETIOLOGIA = "Outras", N = 116, PORCENTAGEM = 91.34) %>% 
  dplyr::mutate(STRING_PCT = paste0(round(PORCENTAGEM, 2), "%")) %>% 
  ggplot(aes(x="", y = N, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()  +
  ggrepel::geom_label_repel(aes(y = c(126.5, 124.5, 116.3, 50), label = N),
                            size = 6,
                            nudge_x = 1,
                            show.legend = FALSE,
                            segment.color = c("#F9766E", "#01BA38","#619DFF", "#C77BFF"),
                            color = "white") +
  guides(fill = guide_legend(title = "Etiologia")) +
  labs(
    title = "Mulheres que constaram agressão no total de prontuários - 2019",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

## 2020/2021 - PORCENTAGEM
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA != 2019, !is.na(ETIOLOGIA)) %>%
  dplyr::group_by(ETIOLOGIA) %>% 
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::mutate(TOTAL = sum(N)) %>% 
  dplyr::filter(ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::mutate(PORCENTAGEM = N / TOTAL * 100) %>%
  dplyr::select(-TOTAL) %>%
  dplyr::add_row(ETIOLOGIA = "Outras", N = 189, PORCENTAGEM = 86.33) %>% 
  dplyr::mutate(STRING_PCT = paste0(round(PORCENTAGEM, 2), "%")) %>% 
  ggplot(aes(x="", y = N, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()  +
  ggrepel::geom_label_repel(aes(y = c(0, 216.5, 200, 100), label = STRING_PCT),
                            size = 6,
                            nudge_x = 1,
                            show.legend = FALSE,
                            segment.color = c("#F9766E", "#01BA38","#619DFF", "#C77BFF"),
                            color = "white") +
  guides(fill = guide_legend(title = "Etiologia")) +
  labs(
    title = "Mulheres que constaram agressão no total de prontuários - 2020/2021",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")


## 2020/2021 - QUANTIDADE
dados %>% 
  dplyr::filter(GÊNERO == "F", DATA != 2019, !is.na(ETIOLOGIA)) %>%
  dplyr::group_by(ETIOLOGIA) %>% 
  dplyr::summarise(N = dplyr::n()) %>%
  dplyr::mutate(TOTAL = sum(N)) %>% 
  dplyr::filter(ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
  dplyr::mutate(PORCENTAGEM = N / TOTAL * 100) %>%
  dplyr::select(-TOTAL) %>%
  dplyr::add_row(ETIOLOGIA = "Outras", N = 189, PORCENTAGEM = 86.33) %>% 
  dplyr::mutate(STRING_PCT = paste0(round(PORCENTAGEM, 2), "%")) %>% 
  ggplot(aes(x="", y = N, fill = ETIOLOGIA)) +
  geom_bar(stat="identity", width=1, color = "white") +
  coord_polar("y", start = 0) +
  theme_void()  +
  ggrepel::geom_label_repel(aes(y = c(0, 216.5, 200, 100), label = N),
                            size = 6,
                            nudge_x = 1,
                            show.legend = FALSE,
                            segment.color = c("#F9766E", "#01BA38","#619DFF", "#C77BFF"),
                            color = "white") +
  guides(fill = guide_legend(title = "Etiologia")) +
  labs(
    title = "Mulheres que constaram agressão no total de prontuários - 2020/2021",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")


## FRATURAS ASSOCIADAS ÀS AGRESSÕES
find_fraturas <- function(fratura) {
  
  dados %>% 
    dplyr::filter(GÊNERO == "F", DATA != 2019,
                  ETIOLOGIA %in% c("Agressão física", "Agressão doméstica", "Acidente doméstico")) %>% 
    dplyr::pull(FRATURAS) %>% 
    stringr::str_to_lower() %>% 
    stringr:::str_detect(fratura) %>% 
    sum(na.rm = TRUE)
}

tibble::tibble(
  "OPN" = find_fraturas("opn"),
  "ÂNGULO" = find_fraturas("ângulo|angulo"),
  "MANDÍBULA" = find_fraturas("mandibula|mandíbula"),
  "CZO" = find_fraturas("czo"),
  "NASAL" = find_fraturas("nasal"),
  "CÔNDILO" = find_fraturas("condilo|côndilo"),
  "ALVEOLAR" = find_fraturas("alveolar"),
  "PARASSÍNFISE" = find_fraturas("parassínfise"),
  "ÓRBITA" = find_fraturas("órbita"),
  "NÃO" = find_fraturas("não|nao")) %>%
  dplyr::select(- NÃO) %>% 
  pivot_longer(
    cols = OPN:ÓRBITA,
    names_to = "FRATURAS") %>% 
  ggplot(aes(reorder(FRATURAS, desc(value)), factor(value))) +
  geom_bar(stat = "identity", fill = "#9194cb") +
  theme_minimal() +
  labs(
    x = "Fraturas",
    y = "Quantidade",
    title = "Fraturas associadas às agressões sofridas por mulheres",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")


## Grafico da quantidade de mulheres atendidas por ano
dados %>%
  dplyr::filter(GÊNERO == "F") %>%
  dplyr::group_by(DATA) %>% 
  dplyr::summarise(quantidade = dplyr::n()) %>% 
  ggplot(aes(DATA, quantidade)) +
  geom_bar(stat = 'identity', fill = '#ebd234') +
  theme_minimal() +
  labs(
    x = "Ano de atendimento",
    y = "Quantidade",
    title = "Mulheres atendidas na Santa Casa da Misericórdia de Araçatuba",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")


## Gráfico distribuição da idade das mulheres
dados %>%
  ggplot(aes(IDADE, color = GÊNERO)) +
  geom_density() +
  theme_minimal() +
  labs(
    x = "Idade",
    y = "Densidade",
    title = "Distribuição da idade por gênero",
    subtitle = "Pacientes atendidos na Santa Casa de Araçatuba",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`") +
  
  ## Boxplot das idades das mulheres
  dados %>% 
  dplyr::filter(!is.na(GÊNERO)) %>% 
  ggplot(aes(x = GÊNERO, y = IDADE, fill = GÊNERO)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    x = "Gênero",
    y = "Idade",
    title = "Boxplot das idades por gênero",
    subtitle = "Pacientes atendidos na Santa Casa de Araçatuba",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")


## Por raça - total
da %>%
  dplyr::filter(!is.na(RAÇA)) %>% 
  dplyr::group_by(DATA, RAÇA) %>% 
  dplyr::summarise(quantidade = dplyr::n(), .groups = "drop") %>% 
  ggplot(aes(x = factor(DATA), y = quantidade, fill = RAÇA)) +
  geom_bar(stat = "identity", group = 1, position = "dodge") +
  theme_minimal() +
  labs(
    x = "Ano",
    y = "Quantidade",
    title = "Pessoas atendidas por ano",
    subtitle = "Atendidas na Santa Casa da Misericórdia de Araçatuba, dividas pela raça",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")


## Por raça - mulheres
da %>%
  dplyr::filter(!is.na(RAÇA), GÊNERO == "F") %>% 
  dplyr::group_by(DATA, RAÇA) %>% 
  dplyr::summarise(quantidade = dplyr::n(), .groups = "drop") %>% 
  ggplot(aes(x = factor(DATA), y = quantidade, fill = RAÇA)) +
  geom_bar(stat = "identity", group = 1, position = "dodge") +
  theme_minimal() +
  labs(
    x = "Ano",
    y = "Quantidade",
    title = "Mulheres atendidas por ano",
    subtitle = "Atendidas na Santa Casa da Misericórdia de Araçatuba, dividas pela raça",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")





## NÃO SEI
key_words <- "múltipla|multipla|rebordo|zigo|blow|complexa|nasal|opn|OPN" 

encontra_fratura <- function(fratura_associada){
  
  dados %>%
    dplyr::filter(GÊNERO == "F",
                  stringr::str_detect(FRATURAS, fratura_associada)) %>% 
    dplyr::summarise(quantidade = dplyr::n()) %>% 
    dplyr::pull(quantidade)
}


tibble::tibble(
  
  OPN = encontra_fratura("OPN|opn"),
  CZM = encontra_fratura("CZM|czm"),
  NASAL = encontra_fratura("nasal|Nasal"),
  ZIGO = encontra_fratura("zigo"),
  CZMO = encontra_fratura("czmo|CZMO")
)

## FEITO A MÃO PORQUE O CARA DISSE QUE SÃO ESSES DADOS
tibble::tibble(
  fraturas = c("Fratura nasal", 
               "Fratura de mandíbula", 
               "Fratura de zigomático",
               "Fratura em região frontal",
               "Fratura em região orbital",
               "Trauma dento alveolar",
               "Fratura de parietal e base de crânio (FAF)"),
  quantidade = c(10, 7, 5, 1, 1, 1, 1)) %>% 
  ggplot(aes(x = fraturas, y = factor(quantidade))) +
  geom_bar(stat = "identity", fill = "#9194cb") +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Fraturas",
    y = "Quantidade",
    title = "Fraturas associadas à agressão feminina - 2020/2021",
    caption = "`Fonte: Prontuários Cirurgiões-Dentistas da Santa Casa de Araçatuba`")

  
## DATAS
## datas (provavelmente) erradas!
## mto longo
dados %>% 
  dplyr::mutate(TEMPO = DATA_ALTA - DATA_TRAUMA) %>% 
  dplyr::filter(TEMPO > 0, !is.na(TEMPO)) %>%
  dplyr::top_n(1, TEMPO)

## voltou no tempo
dados %>% 
  dplyr::mutate(TEMPO = DATA_ALTA - DATA_TRAUMA) %>% 
  dplyr::filter(TEMPO < 0)


## durou mais dias == mais gasto do hospital
dados %>% 
  dplyr::mutate(TEMPO = DATA_ALTA - DATA_TRAUMA) %>%
  dplyr::filter(TEMPO > 0 , !is.na(TEMPO), TEMPO < 300) %>% 
  dplyr::group_by(ETIOLOGIA) %>%
  dplyr::summarise(TOTAL_DIAS = sum(TEMPO),
                   MÉDIA_DIAS = mean(TEMPO),
                   QUANTIDADE = dplyr::n()) %>%
  dplyr::filter(QUANTIDADE > 1) %>% 
  dplyr::top_n(10, MÉDIA_DIAS) %>% 
  dplyr::arrange(desc(MÉDIA_DIAS))

