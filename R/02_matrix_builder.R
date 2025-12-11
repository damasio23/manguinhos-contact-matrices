# R/02_matrix_builder.R
library(dplyr)
library(tidyr)
library(readxl)

# Lógica da população Pi (IDÊNTICA ao script original)
load_population_vector <- function(filepath, labels_age) {
  if(!file.exists(filepath)) {
    warning("Arquivo de população (2974.xls) não encontrado. Usando população fictícia uniforme.")
    return(setNames(rep(1000, length(labels_age)), labels_age))
  }
  
  pop2010 <- read_excel(path = filepath, sheet = "2010")
  row_manguinhos <- pop2010[178, ]
  # Limpa e converte os valores
  raw_vals <- as.numeric(gsub("\\s+", "", as.character(row_manguinhos[ , -c(1,2)])))
  
  Pi <- numeric(length(labels_age)); names(Pi) <- labels_age
  # Mapeamento e Agregação (replicando a lógica de soma de faixas)
  Pi["<1"]   <- raw_vals[2]
  Pi["1-4"]  <- sum(raw_vals[3:6], na.rm=TRUE)
  Pi["5-9"]  <- raw_vals[7]
  Pi["10-14"]<- raw_vals[8]
  Pi["15-19"]<- sum(raw_vals[9:11], na.rm=TRUE)
  Pi[6:17]   <- raw_vals[12:23]
  pop80p     <- raw_vals[24]
  Pi["80-84"] <- pop80p * 0.50
  Pi["85-89"] <- pop80p * 0.35
  Pi["90+"]   <- pop80p * 0.15
  return(Pi)
}

build_matrices <- function(df, Pi, labels_age, breaks_age) {
  # --- EXTRAÇÃO DE IDADES DOS CONTATOS (Domésticos vs. Reportados) ---
  
  # 1. Contatos Domésticos (sub-relato corrigido usa e1_q20_idade)
  df_house <- df %>%
    filter(e1_q58_n_contato < e1_q19) %>%
    pivot_longer(cols = starts_with("e1_q20_idade_"), names_to = "ord", values_to = "age") %>%
    mutate(ord = as.integer(sub(".*_", "", ord)), 
           age_code = findInterval(age, breaks_age, rightmost.closed = TRUE)) %>%
    filter(ord <= contact_count, age_code >= 1 & age_code <= 20) %>%
    select(respondent_id = e1_pid, respondent_age_group, age_code)
  
  # 2. Contatos Reportados (usando e1_q58_idade_p)
  df_reported <- df %>%
    filter(e1_q58_n_contato >= e1_q19) %>%
    pivot_longer(cols = starts_with("e1_q58_idade_p"), names_to = "ord", values_to = "age_code") %>%
    mutate(ord = as.integer(sub(".*p", "", ord)), age_code = as.integer(age_code)) %>%
    filter(ord <= contact_count, age_code >= 1 & age_code <= 20) %>%
    select(respondent_id = e1_pid, respondent_age_group, age_code)
  
  df_contacts <- bind_rows(df_house, df_reported) %>%
    mutate(contact_age_group = factor(age_code, levels = 1:20, labels = labels_age))
  
  # 3. Matriz Y (Contagens)
  Y_matrix <- df_contacts %>%
    group_by(respondent_age_group, contact_age_group) %>%
    summarise(count = n(), .groups="drop") %>%
    complete(respondent_age_group = labels_age, contact_age_group = labels_age, fill = list(count = 0)) %>%
    spread(key = contact_age_group, value = count, fill = 0) %>%
    select(-respondent_age_group) %>%
    as.matrix()
  rownames(Y_matrix) <- labels_age; colnames(Y_matrix) <- labels_age
  Y_matrix["<1", ] <- 0L # Zera a primeira linha (sem respondentes <1)
  
  # 4. Matriz E (Exposição) e Pesos r, w
  respondent_counts <- df %>% count(respondent_age_group)
  m <- length(labels_age)
  
  r <- setNames(rep(0L, m), labels_age)
  r[as.character(respondent_counts$respondent_age_group)] <- respondent_counts$n
  
  E_matrix <- r %o% rep(1, m)
  E_matrix[1, ] <- 0L # Zera a primeira linha
  
  # Pesos w_i = Pi / r_i
  w <- ifelse(r > 0, Pi / r, 0)
  
  # 5. Taxas (Gamma)
  Gamma_matrix <- ifelse(E_matrix > 0, Y_matrix / E_matrix, 0)
  
  return(list(Y = Y_matrix, E = E_matrix, Gamma = Gamma_matrix, r = r, w = w, Pi = Pi))
}