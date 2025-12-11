# R/04_plotting.R
# Funções de visualização e diagnóstico idênticas ao TCC
library(ggplot2)
library(gridExtra) # Necessário para juntar os gráficos no painel

# ==============================================================================
# 1. FUNÇÃO: Heatmap de Resíduos (Erro Local)
# ==============================================================================
# Alteração: Adicionei 'observed_mat' e 'labels_age' como argumentos para funcionar dentro da função
get_heatmap_plot <- function(predicted_mat, observed_mat, model_name, labels_age) {
  
  resid_mat <- observed_mat - predicted_mat
  resid_long <- as.data.frame(as.table(resid_mat))
  colnames(resid_long) <- c("respondent_age", "contact_age", "resid")
  
  # Ordenar fatores
  resid_long$respondent_age <- factor(resid_long$respondent_age, levels = labels_age)
  resid_long$contact_age    <- factor(resid_long$contact_age,    levels = labels_age)
  
  ggplot(resid_long, aes(x = contact_age, y = respondent_age, fill = resid)) +
    geom_tile(color = "white", linewidth = 0.1) +
    scale_fill_gradient2(
      low = "steelblue", mid = "white", high = "darkred", midpoint = 0,
      name = "Erro"
    ) +
    labs(
      title = paste("Heatmap:", model_name),
      subtitle = "Vermelho: Obs > Pred | Azul: Obs < Pred",
      x = "Idade Contato", y = "Idade Respondente"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      legend.position = "bottom",
      plot.title = element_text(size = 10, face="bold"),
      plot.subtitle = element_text(size = 8)
    )
}

# ==============================================================================
# 2. FUNÇÃO: Resíduos vs Ajustados (Homocedasticidade)
# ==============================================================================
get_scatter_plot <- function(model_obj, model_name) {
  
  if(inherits(model_obj, "gam")) {
    # GAM: Pearson para ver variância
    vals_fitted <- fitted(model_obj)
    vals_resid  <- residuals(model_obj, type = "pearson")
    lab_y <- "Resíduos Pearson"
  } else {
    # LOESS
    vals_fitted <- model_obj$fitted
    vals_resid  <- model_obj$residuals
    lab_y <- "Resíduos Ordinários"
  }
  
  df_diag <- data.frame(Fitted = vals_fitted, Residuals = vals_resid)
  
  ggplot(df_diag, aes(x = Fitted, y = Residuals)) +
    geom_point(alpha = 0.5, color = "#2C3E50", size = 1.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#E74C3C", linewidth = 0.8) +
    labs(
      title = paste("Dispersão:", model_name),
      subtitle = "Homocedasticidade",
      x = "Valores Ajustados", y = lab_y
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face="bold"),
      plot.subtitle = element_text(size = 8),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# 3. FUNÇÃO: Q-Q Plot MANUAL 
# ==============================================================================
get_qq_plot <- function(model_obj, model_name) {
  
  # Extração e Padronização dos Resíduos
  if(inherits(model_obj, "gam")) {
    raw_resids <- residuals(model_obj, type = "deviance")
    # Proteção contra desvio padrão zero
    sd_val <- sd(raw_resids)
    if(sd_val == 0) sd_val <- 1 
    std_resids <- (raw_resids - mean(raw_resids)) / sd_val
    sub_t <- "Quantis Normais (Resid. Deviance)"
  } else {
    # Para LOESS
    raw_resids <- model_obj$residuals
    sd_val <- sd(raw_resids)
    if(sd_val == 0) sd_val <- 1
    std_resids <- (raw_resids - mean(raw_resids)) / sd_val
    sub_t <- "Quantis Normais (Resid. Ordinários)"
  }
  
  n <- length(std_resids)
  df_qq <- data.frame(
    sample = sort(std_resids),
    theo   = qnorm(ppoints(n))
  )
  
  ggplot(df_qq, aes(x = theo, y = sample)) +
    geom_point(alpha = 0.5, color = "#2C3E50", size = 1.2) +
    geom_abline(slope = 1, intercept = 0, color = "#E74C3C", linetype = "solid", linewidth = 0.8) +
    labs(
      title = paste("Q-Q Plot:", model_name),
      subtitle = sub_t,
      x = "Quantis Teóricos", y = "Quantis Amostrais"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face="bold"),
      plot.subtitle = element_text(size = 8),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# 4. FUNÇÃO EXTRA: Gerar Painel Completo 
# ==============================================================================
plot_complete_diagnostics <- function(model_obj, predicted_mat, observed_mat, model_name, labels_age) {
  p1 <- get_heatmap_plot(predicted_mat, observed_mat, model_name, labels_age)
  p2 <- get_scatter_plot(model_obj, model_name)
  p3 <- get_qq_plot(model_obj, model_name)
  
  # Organiza em 1 linha e 3 colunas
  grid.arrange(p1, p2, p3, ncol = 3, top = paste("Diagnóstico Completo:", model_name))
}