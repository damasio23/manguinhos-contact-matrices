# R/00_create_synthetic_data.R
library(dplyr)
library(tidyr)

create_synthetic_data <- function(n = 500) {
  set.seed(2025) 
  
  # Estrutura básica
  df <- data.frame(
    e1_pid = 1:n,
    idade_calc = sample(0:95, n, replace = TRUE),
    e1_q19 = sample(1:10, n, replace = TRUE, prob = c(0.1, 0.3, 0.3, 0.15, 0.1, 0.03, 0.01, 0.01, 0.005, 0.005)), 
    e1_q58_n_contato = sample(0:25, n, replace = TRUE) 
  )
  
  # Criação das colunas de idade dos contatos (1 a 10)
  for(i in 1:10) {
    # 1. Contatos Domésticos (e1_q20_idade)
    df[[paste0("e1_q20_idade_", i)]] <- sample(0:90, n, replace = TRUE)
    # 2. Contatos Reportados (e1_q58_idade_p)
    df[[paste0("e1_q58_idade_p", i)]] <- sample(1:20, n, replace = TRUE) # Códigos de Faixa Etária (1 a 20)
  }
  
  if(!dir.exists("data/raw")) dir.create("data/raw", recursive = TRUE)
  
  # write.table é usado com sep=";" para replicar o read.csv do seu código original
  write.table(df, "data/raw/bancocomvidafull_SINTETICO.csv", 
              sep = ";", row.names = FALSE, quote = FALSE, na = "")
  
  message("Dados sintéticos criados em: data/raw/bancocomvidafull_SINTETICO.csv")
}