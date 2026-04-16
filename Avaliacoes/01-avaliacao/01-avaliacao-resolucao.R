# Arquivo: 01-avaliacao-resolucao.R
# Autor(a): Paulo Aragão Daldegan
# Data: 16/04/2026
# Objetivo: 
# Resolução da Avaliação 1 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(tidyverse)
library(here)


# Resolução da Questão 1


# 1.a) --------------------------------------------------------

# importando arquivos

agencias_csv <- here("data/raw/agencias.csv")
credito_csv <- here("data/raw/credito_trimestral.csv")
inadimplencia_csv <- here("data/raw/inadimplencia.csv")

# atribuindo a um objeto

agencias <- read.csv(agencias_csv)
credito <- read.csv(credito_csv)
inadimplencia <- read.csv(inadimplencia_csv)

# inspecionando cada objeto

glimpse(agencias)
glimpse(credito)
glimpse(inadimplencia)


# 1.b) ---------------------------------------------------------

# Reorganizando os dados de credito para o formato long

credito_long <- credito |> 
  pivot_longer(
    cols = c("T1", "T2", "T3", "T4"),
    names_to = "trimestre",
    values_to = "volume_credito"
  )

credito_long


# 1.c) ---------------------------------------------------------

# reunindo todas as informações em um único objeto

dados_completos <- credito_long |>
  left_join(agencias, by = "codigo_agencia") |>
  left_join(inadimplencia, by = c("codigo_agencia", "trimestre"))

dados_completos


# 1.d) ---------------------------------------------------------

# criando duas novas variáveis : credito_por_cooperado e risco

dados_analise <- dados_completos |> 
  mutate(
    credito_por_cooperado = volume_credito * 1000 / num_cooperados,
    risco = case_when(
      taxa_inadimplencia < 3.0 ~ "Baixo",
      taxa_inadimplencia >= 3.0 & taxa_inadimplencia < 4.5 ~ "Moderado",
      taxa_inadimplencia >= 4.5 ~ "Alto"
    )
  )

dados_analise

# 1.e) ---------------------------------------------------------

# Considerando todos os trimestres de 2025, qual foi o volume total de crédito
# concedido e a taxa média de inadimplência em cada cidade?

dados_analise |> 
  group_by(cidade) |> 
  summarise(
    volume_total = sum(volume_credito),
    media_inadimplencia = mean(taxa_inadimplencia)
  ) |> 
  arrange(desc(volume_total))


# Resolução da Questão 2


# 2.a) ---------------------------------------------------------

# função chamada calcular_prestacao() que calcula a prestação mensal de
# um financiamento pelo Sistema Price (Tabela Price)

calcular_prestacao <- function(valor, taxa_anual, num_meses) {
  i_m = taxa_anual / 12
  n = num_meses
  PMT = valor * ((i_m * (1 + i_m)^n) / ((1 + i_m)^n -1))
  return(PMT)
}

# Calculando a prestação de um financiamento de R$ 120.000,00 a uma taxa anual de 12% em 60 meses

calcular_prestacao(120000, 0.12, 60)

# 2.b) ---------------------------------------------------------

# Calcule a prestação mensal para 5 cenários de taxa de juros anual,
# considerando um financiamento de R$ 120.000,00 em 60 meses

# Criando o vetor
taxas <- c(0.08, 0.10, 0.12, 0.14, 0.16)

# usando map_dbl() para calcular as prestações
prestacao_5_cenarios <- map_dbl(
  taxas,
  \(taxa_anual) calcular_prestacao(120000, taxa_anual, 60)
)

# Organizando resultados em uma tibble

resultado <- tibble(
  taxa_anual = taxas,
  prestacao_mensal = prestacao_5_cenarios
)

resultado


# 2.c) ---------------------------------------------------------

# Adicionando colunas a Tibble

resultado |> 
  mutate(
    custo_total = prestacao_mensal * 60,
    juros_totais = custo_total - 120000,
    acessibilidade = case_when(
      prestacao_mensal < 2600 ~ "Acessível",
      prestacao_mensal >= 2600 & prestacao_mensal < 2800 ~ "Moderado",
      prestacao_mensal >= 2800 ~ "Elevado"
    )
  )











