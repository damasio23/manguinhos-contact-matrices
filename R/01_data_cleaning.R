# R/01_data_cleaning.R
library(dplyr)

load_and_clean_data <- function(filepath_real, breaks_age, labels_age) {
  
  filepath <- filepath_real
  if (!file.exists(filepath)) {
    warning("Arquivo REAL não encontrado. Tentando carregar dados SINTÉTICOS...")
    filepath <- "data/raw/bancocomvidafull_SINTETICO.csv"
    if (!file.exists(filepath)) stop("Nenhum dado encontrado. Rode 'R/00_create_synthetic_data.R'.")
  }
  
  # Leitura com tratamento de NA strings 
  df <- read.csv(filepath, sep=";", stringsAsFactors = FALSE, na.strings = c("", "z.missing"))
  df$idade_calc <- as.numeric(gsub(",", ".", df$idade_calc))
  df$e1_q58_n_contato <- as.numeric(df$e1_q58_n_contato)
  df$e1_q19 <- as.numeric(df$e1_q19)
  
  # --- CRIAÇÃO DE FAIXAS ETÁRIAS E AJUSTE DOS CONTATOS ---
  # Aplica a correção metodológica: contact_count = max(reportado, moradores - 1) 
  df <- df %>%
    mutate(
      respondent_age_group = cut(idade_calc, breaks = breaks_age, labels = labels_age, right = FALSE),
      contact_count = if_else(e1_q58_n_contato < e1_q19, e1_q19 - 1L, e1_q58_n_contato)
    ) %>%
    filter(!is.na(contact_count), contact_count != 98L)
  
  return(df)
}