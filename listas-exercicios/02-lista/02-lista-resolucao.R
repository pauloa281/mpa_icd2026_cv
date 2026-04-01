# ============================================================
# Disciplina: Introdução à Ciência de Dados
# ============================================================
# Arquivo: 02-lista-resolucao.R
# Autor(a): Paulo Aragão Daldegan
# Data: 19/03/2026
# Objetivo: Resolução da lista de exercícios 2

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here)      # para usar caminho relativo
library(tidyverse) # meta-pacote que inclui readr, dplyr..
library(gapminder) # contém os dados gapminder

# carrega os dados do pacote gapminder
data(gapminder)
dplyr::glimpse(gapminder)

## Exercício 1
# importando arquivos

caminho_csv <- here("data/raw/productionlog_sample.csv")
productionlog_sample <- read.csv(caminho_csv)

# usando a função glimpse
glimpse(productionlog_sample)


## Exercício 2
# selecionar apenas o país, o ano e a expectativa de vida

dados_expectativa <- gapminder |>
  select(country, year, lifeExp)

# Exibindo resultado
dados_expectativa


# Exercicio 3
# todas as variáveis EXCETO população e PIB per capita

variaveis_exceto_popepib <- gapminder |>
  select(-pop, -gdpPercap)

variaveis_exceto_popepib



# Exercício 4
# selecionar apenas as variáveis que começam com a letra “c”

variaveis_com_c <- gapminder |>
  select(starts_with("c"))

variaveis_com_c


# Exercicio 5
# selecionar todas as variáveis desde country até pop (em sequência na tabela)

variaveis_sequencia <- gapminder |>
  select(country:pop)

variaveis_sequencia


# Exercicio 6
# selecione variáveis que começam e terminam com "p"

variaveis_com_p <- gapminder |>
  select(starts_with("p") | ends_with("p"))

variaveis_com_p


# Exercicio 7
# filtrar apenas os países do continente Americas no ano de 2007

paises_america_2007 <- gapminder |>
  filter(continent == "Americas" & year == 2007)

paises_america_2007


# Exercicio 8
# Apenas os dados do Brasil (country == "Brazil") e salve o resultado em um objeto

pais_brasil <- gapminder |>
  filter(country == "Brazil")

pais_brasil


# Exercicio 9
# Paises que pertencem ao continente Asia, têm população acima de 50 milhões e 
# dados do ano 2007

paises_asia_pop50_2007 <- gapminder |>
  filter(continent == "Asia" & pop >50000000 & year == 2007) |>
  select(country) 

paises_asia_pop50_2007


# Exercicio 10
# Países com expectativa de vida acima de 75 anos,
# mas PIB per capita abaixo de 10.000 dólares em 2007
  
paises_exp75_pib10000_2007 <- gapminder |>
  filter(lifeExp > 75 & gdpPercap < 10000 & year == 2007) |>
  select(country)

paises_exp75_pib10000_2007


# Exercicio 11
# População em milhoes

gapminder |>
  mutate(pop_em_milhoes = pop / 1000000) |>
  select(country, year, pop_em_milhoes)


# Exercicio 12
# “receita total” (PIB per capita × população) de cada país

receita_total_pais <- gapminder |>
  group_by(country) |>
  mutate(receita_total = gdpPercap * pop) |>
  select(country, year, receita_total)

receita_total_pais
  

# Exercicio 13
# variável chamada economia_grande que seja “Sim” quando a população for maior que 
# 50 milhões e “Não” caso contrário

economia_grande <- gapminder|>
  mutate(
    economia_grande = ifelse(pop > 50000000, "Sim", "Não")
  ) 

economia_grande


# Exercicio 14
# variável que classifique os países em três categorias baseadas na expectativa de vida

exp_vida <- gapminder|>
  mutate(
    exp_vida = case_when(lifeExp < 60 ~ "Baixo",
                         lifeExp > 60 & lifeExp < 75 ~ "Medio",
                         lifeExp > 75 ~ "Alta")
  ) |>
  filter(year == 2007)

exp_vida


# Exercicio 15
# calcular a expectativa média de vida por continente

expectativa_por_continente <- gapminder |>
  group_by(continent) |>
  summarise(expectativa_media = mean(lifeExp))

expectativa_por_continente


# Exercicio 16
# população total por continente no ano de 2007

pop_total_continente <- gapminder |>
  filter(year == 2007) |>
  group_by(continent) |>
  summarise(pop_total = sum(pop))

pop_total_continente


# Exercicio 17
# objeto que mostre, para cada continente: O número de “filiais” (países),
# O PIB per capita médio (indicador de desempenho),
# O PIB per capita da melhor “filial” (máximo)

filiais <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(num_filiais = n(),
            desempenho_medio = mean(gdpPercap),
            melhor_filial = max(gdpPercap)
            )

filiais

# Exercicio 18
# Evolução da expectativa média de vida do continente americano ao longo dos anos 

Evol_expvida_continente <- gapminder |>
  group_by(continent, year) |>
  summarise(
    expec_media = mean(lifeExp)
  ) |> 
  arrange(year) |> 
  filter(continent == "Americas")
  
Evol_expvida_continente


# Exercicio 19
# ordenar os países por expectativa de vida (do maior para o menor)

paises_ordenados <- gapminder |> 
  filter(year == 2007) |> 
  arrange(desc(lifeExp))

paises_ordenados

  
# Exercicio 20
# 5 países com menor PIB per capita em 2007
  
paises_pib_2007 <- gapminder |> 
  filter(year == 2007) |> 
  select(country, continent, gdpPercap) |> 
  head(6)
  
paises_pib_2007


# Exercicio 21
# lista dos países das Américas ordenados por população 
# (do maior para o menor) em 2007

lista_pop_americas <- gapminder |> 
  filter( year == 2007, 
    continent == "Americas"
  ) |> 
  select(country, continent, pop) |> 
  arrange(desc(pop))
  
lista_pop_americas


# Exercicio 22
# expectativa média de vida de seus países em 2007

ranking_exp_vida <- gapminder |> 
  filter(year == 2007) |> 
  group_by(continent) |> 
  summarise(exp_media = mean(lifeExp)) |> 
  arrange(desc(exp_media))

ranking_exp_vida 











  
  