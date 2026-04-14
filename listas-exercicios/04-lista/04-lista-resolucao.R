# Arquivo: 04-lista-resolucao.R
# Autor(a): Paulo Aragão Daldegan
# Data: 13/04/2026
# Objetivo: Resolução da lista de exercícios 4

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)


# carrega os pacotes necessários
library(here)      # para usar caminhos relativos
library(tidyverse) # inclui readr, dplyr, tidyr, ggplot2 etc.


# Exercício 1 ------------------------------------------------
# Função que calcule o montante (capital + juros)
# de uma aplicação com juros compostos e capitalização mensa

calcular_montante_mensal <- function(capital, taxa_anual, meses) {
  taxa_mensal = taxa_anual / 12
  montante = capital * (1 + taxa_mensal)^meses
  return(montante)
}

# Testando a funcao com capital = 5000, taxa_anual = 0.10, meses = 36.
calcular_montante_mensal(5000,0.10,36)


# Exercício 2 ------------------------------------------------
# Função que receba um retorno (em decimal) e retorne uma classificação textual

avaliar_investimento <- function(retorno) {
  if (retorno > 0.15) {
    return("excelente")
  } else if (retorno > 0.05){
    return("bom")  
  } else if (retorno > 0) {
    return("fraco")
  } else {
    return("negativo")
  }
}

# Testando a função com os valores: 0,20; 0,08; 0,02; -0,05
avaliar_investimento(0.2)
avaliar_investimento(0.08)
avaliar_investimento(0.02)
avaliar_investimento(-0.05)


# Exercício 3 ------------------------------------------------
# função analisar_carteira que receba uma tibble com as colunas ativo,
# preco_compra, preco_atual e quantidade, e retorne a mesma tibble acrescida de colunas

analisar_carteira <- function(dados) {
  dados |> 
    mutate(
      retorno = (preco_atual / (preco_compra - 1)) * 100,
      valor_investido = preco_compra * quantidade,
      valor_atual = preco_atual * quantidade,
      resultado = valor_atual - valor_investido,
      situacao = ifelse(resultado > 0, "Ganho", "Perda")
      )
}

# Tibble

carteira <- tibble(
  ativo        = c("PETR4", "VALE3", "ITUB4", "WEGE3"),
  preco_compra = c(28.50, 68.20, 32.00, 45.00),
  preco_atual  = c(31.00, 65.40, 33.60, 48.50),
  quantidade   = c(200, 100, 300, 150)
)

# Testando a função

analisar_carteira(carteira)

# Exercício 4 ------------------------------------------------
# calcular o valor futuro de R$ 10.000 investidos
# por 20 anos para as taxas anuais

# Taxas anuais
taxas_anuais <- c(0.04, 0.06, 0.08, 0.10, 0.12, 0.14, 0.16)

# Valor Futuro

calcular_valor_futuro <- function(valor_presente, taxa, periodos) {
  valor_futuro <- valor_presente * (1 + taxa)^periodos
  return(valor_futuro)
}

# Usando map_dbl()

vf_20_anos <- map_dbl(
  taxas_anuais,
  \(taxa) calcular_valor_futuro(10000, taxa, 20)
)

vf_20_anos

# Criando a tibble

comparacao_cenarios <- tibble(
  taxa = taxas_anuais,
  taxa_percentual = taxas_anuais * 100,
  valor_futuro = vf_20_anos,
  ganho_liquido = vf_20_anos - 10000
)

# Verificando a tibble

comparacao_cenarios


# Exercício 5 ------------------------------------------------
# Avaliando um projeto de investimento

# Calcula o VPL de um projeto de investimento
# Argumentos:
#   investimento   - valor do investimento inicial
#   fluxos         - vetor de fluxos de caixa futuros
#   taxa           - taxa de desconto por período
#   valor_residual - valor residual ao final (padrão = 0)
# Retorna:
#   valor numérico correspondente ao VPL
calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)     # número de períodos
  t <- seq_along(fluxos)  # sequência 1, 2, ..., n
  
  vpl <- -investimento +
    sum(fluxos / (1 + taxa)^t) +
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# Definindo fluxos de caixa
fluxos_de_caixa <- c(80000, 95000, 110000, 100000)

# Taxas
taxas_desconto <- c(0.08, 0.10, 0.12, 0.14, 0.16, 0.18)

# Calculando os VPLs por taxa
vpl_por_taxa <- map_dbl(
  taxas_desconto,
  \(taxa) calcular_vpl(300000, fluxos_de_caixa, taxa, 30000)
)

# Organizando resultados na tibble

analise_projeto <- tibble(
  taxa_pct = taxas_desconto * 100,
  vpl = vpl_por_taxa,
  decisao = ifelse(vpl > 0, "Viavel", "Inviavel")
)

analise_projeto

# Exercício 6 ------------------------------------------------
# (resolver em arquivo .qmd separado)


# Exercício 7 (Desafio) --------------------------------------
# simulação de Monte Carlo simplificada para avaliar o risco de um projeto de investimento

# investimento inicial:     R$ 200.000
# taxa de desconto:         10% ao ano
# valor residual:           R$ 20.000
# o projeto gera fluxos de caixa por 3 anos
# os fluxos de caixa não são conhecidos com certeza: estimamos que cada 
# fluxo anual tem média de R$ 80.000 e desvio-padrão de R$ 15.000

# Função calcular VPL

calcular_vpl <- function(investimento, fluxos, taxa, valor_residual = 0) {
  n <- length(fluxos)     # número de períodos
  t <- seq_along(fluxos)  # sequência 1, 2, ..., n
  
  vpl <- -investimento +
    sum(fluxos / (1 + taxa)^t) +
    valor_residual / (1 + taxa)^n
  
  return(vpl)
}

# define a semente para reprodutibilidade
set.seed(123)

# Usando map_dbl() para simular 1.000 cenários, com 3 fluxos de caixa aleatório

vpl_sim <- map_dbl(1:1000, \(i) {
  fluxos <- rnorm(3, mean = 80000, sd = 15000)
  calcular_vpl(200000, fluxos, 0.10, 20000)
})

# Avaliando o projeto de investimento

analise_monte_carlo <- tibble(vpl = vpl_sim) |> 
  summarise(
    vpl_medio = mean(vpl),
    prob_vpl_positivo = mean(vpl >0),
    percentil_5 = quantile(vpl, 0.05),
    percentil_95 = quantile(vpl, 0.95)
  )


# exibe o objeto
View(analise_monte_carlo)












