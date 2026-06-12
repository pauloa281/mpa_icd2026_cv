# Arquivo: 02-avaliacao-resolucao.R
# Autor(a): Paulo Aragão Daldegan
# Data: 11/06/2026
# Objetivo: Resolução da Avaliação 2 - Introdução à Ciência de Dados


# Configurações globais  ----------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# Carrega os pacotes necessários
library(tidyverse) # dplyr, purrr::pmap_dbl() etc.
library(EnvStats)  # distribuição triangular: rtri()
library(tidyquant) # tq_get()


# Resolução da Questão 1 ----------------------------------------

# Inserindo a semente

set.seed(2026)

# Números de cenarios a serem simulados

n_sim <- 20000

# Duração do projeto

duracao <- 5

# a1 - Criando a tibble de cenários 

cenarios <- tibble(
  cenario = seq_len(n_sim),
  investimento = rtri(n_sim, 850, 1200, 1000),
  receita_anual = rtri(n_sim, 230, 350, 290),
  valor_residual = rtri(n_sim, 100, 200, 150),
  taxa_desconto = rtri(n_sim, 0.11, 0.15, 0.13)
)

# a2 - criando a função de calcular VPL

calcular_VPL <- function(investimento, receita_anual, valor_residual, taxa_desconto, duracao = 5) {
  
  # Cria o vetor de duração
  anos <- seq_len(duracao)
  
  # Valor presente das receitas
  vp_receitas <- sum(receita_anual / (1 + taxa_desconto)^anos)
  
  # Valor presente do residual
  vp_residual <- valor_residual / (1 + taxa_desconto)^duracao
  
  # VPL do cenário
  vp_receitas + vp_residual - investimento
  
}

# a3 - Aplicando a função em todos os cenários

simulacoes <- cenarios |> 
  mutate(
    vpl = pmap_dbl(
      list(
        investimento = investimento,
        receita_anual = receita_anual,
        valor_residual = valor_residual,
        taxa_desconto = taxa_desconto
      ),
      calcular_VPL
    )
  )

# vetor de VPLs simulados

vpl_sim <- simulacoes$vpl

# b - Calculando a probabilidade de o projeto destruir valor

prob_vpl_neg <-  mean(vpl_sim < 0)

# Exibindo a probabilidade em percentual

prob_vpl_neg * 100

# Comentário
# A probabilidade de o investimento destruir valor dado as taxas, receitas, valor residual, período de 
# duração do projeto e investimento inicia é de aproximadamente 23%, demonstrando que há mais chances de
# criar valor do que destruir, sendo viável em muitos cenários.

# c - Calculando o VPL determinístico

vpl_det <- calcular_VPL(
  investimento = 1000,
  receita_anual = 290,
  valor_residual = 150,
  taxa_desconto = 0.13
)

# media dos vpls simulados

vpl_medio <- mean(vpl_sim)

# calculando desvio padrão dos vpls simulados

vpl_desvio <- sd(vpl_sim)

# exibindo vetores para comparação

vpl_medio
vpl_desvio
vpl_det

# interpretação
# O VPL determinístico é obtido somente pelas modas de cada fator do VPL. Já o VPL simulado ele incorpora
# a incerteza com valores mínimos e máximos, com valores simulados a partir de números pseudo-aleatórios e 
# uma distribuição triangular, representando melhor a "realidade". Perecebe-se que também há um alto desvio padrão 
# dos VPLs, mostrando que o valor pode variar para bem mais ou bem menos que o valor da média e também do determinístico.
# Para uma melhor avaliação, o VPL simulado (trazendo também probabilidade de ocorrer VPL maior ou menor que 0)
# é mais indicado e real que somente analisarmos o determinístico

# d - Construindo o histograma da distribuição simulada

hist(vpl_sim,
     breaks = 50,
     col = "lightblue",
     main = "Distribuição simulada do VPL",
     xlab = "VPL (R$ mil)")

# linha vertical em VPL = 0
abline(v = 0, col = "red", lwd =2, lty =2)

# linha vertical no vpl médio simulado
abline(v = mean(vpl_sim), col = "blue", lwd = 2)

# legenda das duas linhas
legend("topright",
       legend = c("VPL = 0", "VPL médio simulado"),
       col = c("red", "blue"),
       lwd =2, lty = c(2, 1), bty = 'n')




# Resolução da Questão 2 ----------------------------------------

# a - imporatndo os preçoes ajustados e calculando retornos log diarios

precos_vale3 <- "VALE3.SA" |> 
  tq_get(get = "stock.prices",
         from = "2024-01-01",
         to = "2026-06-08") |> 
  select(date, adjusted)

retornos_vale3 <- precos_vale3 |> 
  # formula do retorno log diario (use o preço ajustado)
  mutate(ret = log(adjusted / dplyr ::lag(adjusted))) |> 
  drop_na()

# extrai a coluna ret como um vetor
ret_vale3 <-  retornos_vale3 |> pull(ret)

# b - Parametros do problema
valor_carteira <- 25000
p <- 0.01

# c - Calculando VaR historico

ret_ordenado <- sort(ret_vale3)

# posição no quantil de p
k <- ceiling(length(ret_ordenado) * p)

# retorno no ponto de corte
retorno_var <-  ret_ordenado[k]

# converte o retorno em perda positiva (%)
var_percentual <- -retorno_var * 100

# VaR em reais 

var_monetario <- -retorno_var * valor_carteira

# exibe os valores do VaR

var_percentual
var_monetario

# d - Expected shortfall

# média dos retornos na posição 1 até K
retorno_medio_cauda <- mean(ret_ordenado[1:k])

# ES em %
es_percentual <- -retorno_medio_cauda * 100

# ES em reais 
es_monetario <- -retorno_medio_cauda * valor_carteira

# exibe os valores do ES

es_percentual
es_monetario

# Interpretação
# O VaR histórico para o horizonte de 1 dia, com p = 1%, é de aprox. 3,86% ou R$ 962,80.
# O limiar de perda diária poderá ser ultrapassado em 1% dos dias, mas 99% dos dias a perda diária não será maior.
# O ES foi de aprox. 4,74% ou R$ 1185, sendo perdas médias localizadas na cauda além do VaR, demonstrando a severidade média das perdas mais extremas
# O VaR ele vai indicar o limiar de perda a 1%, enquanto o ES são as médias dessas perdas extremas (além do VaR).


# Resolução da Questão 3 ----------------------------------------

# a - Importando os preços ajustados

serie_precos <- c("ITUB4.SA", "VALE3.SA", "WEGE3.SA") |> 
  tq_get(get = "stock.prices",
         from = "2024-01-01",
         to = "2026-06-08") |> 
  select(symbol, date, adjusted) |> 
  pivot_wider(names_from = symbol,
              values_from = adjusted) |> 
  rename(dia = date,
         itub4 = ITUB4.SA,
         vale3 = VALE3.SA,
         wege3 = WEGE3.SA)

# retorno simples 
retornos <- serie_precos |> 
  mutate(
    ret_itub4 = itub4 / dplyr::lag(itub4) - 1,
    ret_vale3 = vale3 / dplyr::lag(vale3) - 1,
    ret_wege3 = wege3 / dplyr::lag(wege3) - 1
  ) |> 
  # remove dados faltantes
  drop_na()

# b - Calculando o retorno diário da carteira

# pesos: itub4, vale3, wege3
pesos <- c(0.40, 0.35, 0.25)

retornos <- retornos |> 
  mutate(ret_carteira = pesos[1] * ret_itub4 +
           pesos[2] * ret_vale3 +
           pesos[3] * ret_wege3
         )

# c - d - Parametros e medidas de risco (VaR e ES)

# Parametros
valor_carteira <- 100000
p <- 0.01

# ordena os retornos do pior para o melhor

ret_ordenado <- sort(retornos$ret_carteira)

# posição do quantil de p

k <- ceiling(length(ret_ordenado) * p)

# VaR em percentual
var_percentual <- -ret_ordenado[k] * 100

# VaR monetario
var_monetario <- -ret_ordenado[k] * valor_carteira

# ES em %
es_percentual <- -mean(ret_ordenado[1:k]) * 100

# ES em reais
es_monetario <- -mean(ret_ordenado[1:k]) * valor_carteira
  
# exibe os valores do VaR e do ES
var_percentual
var_monetario
es_percentual
es_monetario

# interpretação
# O VaR da carteira (o limiar de perda a p = 1%) foi de 2,48%, com um perda monetária de R$ 2482,90
# O ES, referente a média das perdas na cauda além do VaR, foi de 3,15% ou R$ 3157,10.
# Ele tende a ser maior justamente por se localizar além do VaR, sendo uma média além do valor do VaR,
# assim captando perdas até mais extremas que o limiar de 1% do VaR.









