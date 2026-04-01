# Arquivo: 03-lista-resolucao.R
# Autor(a): Paulo Aragão Daldegan
# Data: 26/03/2026
# Objetivo: Resolução da lista de exercícios 3

# Configuracoes globais  ------------------------------------

# define opções globais para exibição de números
options(digits = 5, scipen = 999)

# carrega os pacotes necessários
library(here) # para usar caminhos relativos
library(tidyverse) # meta-pacote que inclui readr, dplyr, tidyr...


# Exercício 1 ---------------------------------------------------------------


# importa os arquivos

# Importando os dados da tabela produtos
caminho_produtos <- here("data/raw/produtos.csv")
dados_produtos <- read.csv(caminho_produtos)


# Importando os dados da tabela vendas
caminho_vendas <- here("data/raw/vendas.csv")
dados_vendas <- read.csv(caminho_vendas)


# Importando os dados da tabela clientes
caminho_clientes <- here("data/raw/clientes.csv")
dados_clientes <- read.csv(caminho_clientes)


# analisa os objetos importados

glimpse(dados_produtos)
glimpse(dados_vendas)
glimpse(dados_clientes)

# combina vendas com produtos e clientes
# Combinação de vendas com produtos

vendas_com_produtos <- dados_vendas |> 
  left_join(dados_produtos, by = "codigo_produto")

vendas_com_produtos

# combinação do resultado com clientes e salvando em um objeto

relatorio_vendas <- vendas_com_produtos |> 
  left_join(dados_clientes, by = "id_cliente")

relatorio_vendas

# Selecionando variaveis

relatorio_vendas_final <- relatorio_vendas |> 
  select(id_venda, data_venda, nome_produto, 
         categoria, quantidade, nome_cliente, cidade)

# exibe a estrutura do resultado
glimpse(relatorio_vendas_final)


# Sobre os NA: Na linha 5 das variaveis nome_produto e categoria,
# bem como na linha 6 das variaveis nome_cliente e cidade, verifica-se a sigla NA.
# Isso ocorre devido a valores ausentes, que nao tem correspondencia de uma tabela em outra.
# Ex: Na tabela dados_vendas,existe o codigo P006, o qual nao existe na tabela dados_produtos.


# Utilizando o full_join

dados_vendas_produtos <- dados_vendas |> 
  full_join(dados_produtos, by = "codigo_produto")

dados_vendas_produtos

# Perecebe-se que nenhum dado foi descartado, mesclando todos os valores
# na tabela. O numero de observacoes (linhas) aumentou comparando com o left_join

# Exercício 2 ---------------------------------------------------------------


# importa os arquivos

# Importando os dados da tabela governanca
caminho_governanca <- here("data/raw/governanca.csv")
dados_governanca <- read.csv(caminho_governanca)


# Importando os dados da tabela risco
caminho_risco <- here("data/raw/risco.csv")
dados_risco <- read.csv(caminho_risco)


# Importando os dados da tabela contabeis
caminho_contabeis <- here("data/raw/contabeis.csv")
dados_contabeis <- read.csv(caminho_contabeis)


# analisa os objetos importados

glimpse(dados_governanca)
glimpse(dados_risco)
glimpse(dados_contabeis)


# combina governança, risco e dados contábeis

dados_governanca_risco <- dados_governanca |> 
  left_join(dados_risco, by = "codigo_negociacao")

analise_integrada <- dados_governanca_risco |> 
  left_join(dados_contabeis, by = c("codigo_negociacao", "ano"))

analise_integrada

# selecionando variaveis

analise_integrada_final <- analise_integrada |> 
  select(empresa, codigo_negociacao, ano, indice_governanca,
         tipo_controlador, comite_auditoria, retorno_anual, volatilidade,
         beta, roa, alavancagem, tamanho_ativo)

# exibe a estrutura do resultado
glimpse(analise_integrada_final)


# Comentarios
# Algumas empresas apresentam NA, pois não tem alguns dados destas empresas em outra tabela.
# Quando se aplica left_join, a variavel da tabela da direita deve ter correspondencia (por meio da chave),
# com a variavel da tabela esquerda. Se não tiver, os valores são assumidos como NA.

# O left_join é adequado para manter os valores de acordo com a tabela da esquerda (principal),
# preservando a variavel empresas, e unindo as tabelas mantendo o padrão.



# Exercício 3 ---------------------------------------------------------------


# importa os arquivos

# Importando dados de acoes
caminho_acoes <- here("data/raw/acoes.csv")
dados_acoes <- read.csv(caminho_acoes)

# Importando dados de eventos corporativos
caminho_eventos_corporativos <- here("data/raw/eventos_corporativos.csv")
dados_eventos_corporativos <- read.csv(caminho_eventos_corporativos)


# analisa os objetos importados

glimpse(dados_acoes)
glimpse(dados_eventos_corporativos)


# constrói a base do estudo de eventos

dados_estudo_eventos <- dados_acoes |> 
  inner_join(dados_eventos_corporativos, 
             by = c("ticker", "data" = "data_anuncio")) 

# selecionando variaveis

dados_estudo_eventos_final <- dados_estudo_eventos |> 
  select(ticker, data, tipo_evento, valor, retorno_diario, volume)

# exibe o objeto final

glimpse(dados_estudo_eventos_final)

# Comentarios

# O objeto final possui menos linhas por causa do inner_join. Ele mantém somente 
# as linhas que têm correspondencia nas duas tabelas. No caso, somente as linhas
# que tenham correspondencia com Ticker e data em ambas as tabelas, as linhas sem
# correspondencia são descartadas.

# Porque o inner_join funciona como um filtro de interseção. A lógica por trás dele é que uma linha 
# só tem valor se houver uma correspondência exata em ambos as tabelas.

# ------------------------- FIM ---------------------------------------------#
