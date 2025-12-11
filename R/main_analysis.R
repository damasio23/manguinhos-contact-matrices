# ==============================================================================
# TCC: Análise de Matrizes de Contato Suavizadas no Complexo de Manguinhos
# Reprodutor Fiel do Script Original
# ==============================================================================

# 1. Carregar Pacotes e Funções
if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, tidyr, ggplot2, mgcv, readxl, caret, magrittr, tibble, gridExtra)

# Carrega nossas funções modularizadas (00, 01, 02, 03, 04)
sapply(list.files("R", full.names = TRUE), source)

# Definições do Estudo
breaks_age <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, Inf)
labels_age <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
                "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")

# ------------------------------------------------------------------------------
# 2. LEITURA, LIMPEZA E MATRIZES (Seções 2.2 e 2.3)
# ------------------------------------------------------------------------------
df_clean <- load_and_clean_data("data/raw/bancocomvidafull.csv", breaks_age, labels_age)
Pi <- load_population_vector("data/raw/2974.xls", labels_age)
matrices <- build_matrices(df_clean, Pi, labels_age, breaks_age)

# Prepara dataframes longos
data_prep <- prepare_modeling_data(matrices, labels_age, breaks_age)
df_loess <- data_prep$loess
df_gam <- data_prep$gam
m <- data_prep$m

# Plot Matriz Bruta (Figura 3 do TCC)
plot_heatmap(matrices$Gamma, matrices$Gamma, "Matriz Bruta de Taxas (Gamma)", labels_age)

# ------------------------------------------------------------------------------
# 3. LOESS (M1 e M2) - Calibração e Validação (Seção 2.4 e 3.1)
# ------------------------------------------------------------------------------
cat("\n--- 3. LOESS: Calibração de Span (Fig. 23) ---\n")
calib_loess <- calibrate_loess_span(df_loess, m)
best_span <- calib_loess$best_span
cat(sprintf("Melhor Span encontrado para EDF=40: %.3f\n", best_span))

# Plot Calibração Span (Figura 23)
plot(calib_loess$span_grid, calib_loess$trace_hat_vec, type="b", 
     xlab="span", ylab="graus de liberdade efetivos", main="Calibração do parâmetro span")
abline(h = 2 * m, col="red", lty=2)

# Rodar modelos M1 e M2
fit_m1 <- run_loess_model(df_loess %>% select(-diff), "age-age", best_span, m)
fit_m2 <- run_loess_model(df_loess %>% select(-cont), "cohort", best_span, m)

# Cross-Validation 5-fold (para LOESS)
set.seed(2025)
folds_loess <- createFolds(df_loess$y, k=5)
cv_rmse_m1 <- mean(cv_loess(df_loess %>% select(-diff), best_span, folds_loess, "age-age"))
cv_rmse_m2 <- mean(cv_loess(df_loess %>% select(-cont), best_span, folds_loess, "cohort"))

# Imprimir Tabela 1 (Tabela 1 do TCC)
cat("\n--- Tabela 1: Métricas LOESS ---\n")
print(diagnose_loess(fit_m1, fit_m2, df_loess, cv_rmse_m1, cv_rmse_m2))

# ------------------------------------------------------------------------------
# 4. GAM BINOMIAL NEGATIVA (M5 e M6) - Validação (Seção 3.3.2)
# ------------------------------------------------------------------------------
cat("\n--- 4. GAM NB: Ajuste e Validação ---\n")

models_nb <- list(
  M5_k15 = run_gam_model(df_gam, "age-age", "nb", k=15),
  M5_k20 = run_gam_model(df_gam, "age-age", "nb", k=20),
  M6_k15 = run_gam_model(df_gam, "cohort", "nb", k=15),
  M6_k20 = run_gam_model(df_gam, "cohort", "nb", k=20)
)

# Cross-Validation 5-fold (para GAM NB)
set.seed(2025)
folds_gam <- createFolds(df_gam$count, k=5)

cv_res_nb <- tibble(Model=character(), CV_RMSE=double())

# Função para CV do M5 (age-age)
cv_fun_m5 <- function(d) run_gam_model(d, "age-age", "nb", k=15)
cv_res_nb <- cv_res_nb %>% add_row(Model="M5_k15", CV_RMSE=cv_gam(cv_fun_m5, df_gam, folds_gam))
cv_fun_m5_20 <- function(d) run_gam_model(d, "age-age", "nb", k=20)
cv_res_nb <- cv_res_nb %>% add_row(Model="M5_k20", CV_RMSE=cv_gam(cv_fun_m5_20, df_gam, folds_gam))

# Função para CV do M6 (cohort)
cv_fun_m6 <- function(d) run_gam_model(d, "cohort", "nb", k=15)
cv_res_nb <- cv_res_nb %>% add_row(Model="M6_k15", CV_RMSE=cv_gam(cv_fun_m6, df_gam, folds_gam))
cv_fun_m6_20 <- function(d) run_gam_model(d, "cohort", "nb", k=20)
cv_res_nb <- cv_res_nb %>% add_row(Model="M6_k20", CV_RMSE=cv_gam(cv_fun_m6_20, df_gam, folds_gam))

# Imprimir Tabela 3 (Tabela 3 do TCC)
cat("\n--- Tabela 3: Comparativo Final NB ---\n")
print(diagnose_gam_nb(models_nb, cv_res_nb))

# ------------------------------------------------------------------------------
# 5. MODELO FINAL E DIAGNÓSTICO (M5, k=20)
# ------------------------------------------------------------------------------
fit_final <- models_nb$M5_k20
mat_final <- predict_matrix(fit_final, df_gam, labels_age)

# Heatmap da Matriz Final
plot_heatmap(mat_final, matrices$Gamma, "M5 (k=20) - Modelo Final", labels_age)

# Painel de Diagnóstico Completo (Figura 17 do TCC)
plot_complete_diagnostics(
  model_obj = fit_final, 
  predicted_mat = mat_final, 
  observed_mat = matrices$Gamma, 
  model_name = "M5: Binomial Negativa (k=20)", 
  labels_age = labels_age
)
