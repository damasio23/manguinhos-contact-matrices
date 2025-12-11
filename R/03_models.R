# R/03_models.R
library(mgcv)
library(caret)
library(tibble)

# --- Funções de Preparação ---

prepare_modeling_data <- function(matrices, labels_age, breaks_age) {
  m <- length(labels_age)
  midpoints <- (breaks_age[1:m] + breaks_age[2:(m+1)])/2
  
  Y_vec <- as.vector(matrices$Y)
  E_vec_gam <- rep(matrices$r, times = m) # Exposição para GAM
  w_vec_gam <- rep(matrices$w, times = m) # Pesos P_i/r_i para GAM
  
  resp_mid <- rep(midpoints, times = m)
  cont_mid <- rep(midpoints, each = m)
  
  # Diferença para Coorte
  cohort_diff <- as.vector(outer(1:m, 1:m, FUN = "-"))
  
  # --- Dataframe para GAM (M3 a M6) ---
  df_gam <- data.frame(
    count = Y_vec, E = E_vec_gam, resp = resp_mid, cont = cont_mid,
    diff = cohort_diff, w_gam = w_vec_gam
  ) %>% filter(E > 0)
  
  # --- Dataframe para LOESS (M1 e M2) ---
  df_loess <- data.frame(
    y = as.vector(matrices$Gamma), resp = resp_mid, cont = cont_mid,
    diff = cohort_diff, w_loess = rep(matrices$w %o% rep(1, m), times = 1) # Ew_matrix
  )
  # Normalização dos pesos para LOESS [cite: 283]
  df_loess$w_loess <- df_loess$w_loess / mean(df_loess$w_loess)
  
  return(list(gam = df_gam, loess = df_loess, midpoints = midpoints, m = m, labels_age = labels_age))
}


# --- Funções LOESS (M1 e M2) ---

# Calibra o span para graus de liberdade efetivos (EDF) específicos [cite: 281]
calibrate_loess_span <- function(data, m) {
  span_grid <- seq(0.05, 0.20, by = 0.005)
  trace_hat_vec <- numeric(length(span_grid))
  
  # Target EDF = 2 * m = 40 (para matriz 20x20) [cite: 282]
  target_edf <- 2 * m 
  
  # Usa M1 (resp + cont) para calibração, como no script original
  for (k in seq_along(span_grid)) {
    fit_k <- loess(
      y ~ resp + cont, data = data, weights = w_loess, span = span_grid[k],
      degree = 1, surface = "direct", family = "gaussian"
    )
    trace_hat_vec[k] <- fit_k$trace.hat
  }
  
  best_span <- span_grid[which.min(abs(trace_hat_vec - target_edf))]
  return(list(best_span = best_span, trace_hat_vec = trace_hat_vec, span_grid = span_grid))
}

run_loess_model <- function(data, type, best_span, m) {
  if (type == "age-age") {
    formula <- y ~ resp + cont
  } else {
    formula <- y ~ resp + diff
  }
  
  fit <- loess(
    formula, data = data, weights = w_loess, span = best_span,
    degree = 1, surface = "direct", family = "gaussian"
  )
  
  # Matriz predita
  pred_mat <- matrix(fit$fitted, nrow = m, ncol = m, byrow = FALSE)
  
  # Truncamento a posteriori (Garante positividade) [cite: 298]
  min_pos <- min(pred_mat[pred_mat > 0]); 
  pred_mat[pred_mat < 0] <- min_pos / 2 
  
  return(list(model = fit, pred_matrix = pred_mat))
}


# --- Funções GAM (M3, M4, M5, M6) ---

run_gam_model <- function(data, type="age-age", family="nb", k=20) {
  if (type == "age-age") {
    form <- count ~ te(resp, cont, bs = c("ts","ts"), k = c(k,k)) + offset(log(E))
  } else {
    form <- count ~ te(resp, diff, bs = c("ts","ts"), k = c(k,k)) + offset(log(E))
  }
  
  if (family == "nb") {
    fam <- nb(link = "log")
  } else if (family == "poisson") {
    fam <- poisson(link = "log")
  } else if (family == "quasipoisson") {
    fam <- quasipoisson(link = "log")
  } else {
    stop("Família não suportada.")
  }
  
  # Usa select=TRUE para GAM Poisson (como no script original)
  sel_val <- if (family == "poisson") TRUE else FALSE 
  
  fit <- gam(form, family = fam, data = data, weights = w_gam, method = "REML", select = sel_val)
  return(fit)
}

predict_matrix <- function(model, data, labels_age) {
  m <- length(labels_age)
  pred_rates <- predict(model, type = "response") / data$E
  
  mat <- matrix(pred_rates, nrow = m, ncol = m, byrow = FALSE)
  dimnames(mat) <- list(labels_age, labels_age)
  return(mat)
}

# --- Funções de Validação (RMSE e CV-RMSE) ---

# Função genérica de CV para LOESS (M1/M2) 
cv_loess <- function(data, best_span, folds, type) {
  loess_fun <- function(d) {
    if (type == "age-age") formula <- y ~ resp + cont else formula <- y ~ resp + diff
    loess(formula, data=d, weights=d$w_loess, span=best_span,
          degree=1, surface="direct", family="gaussian")
  }
  
  vapply(folds, function(idx) {
    tr <- data[-idx, ]
    te <- data[ idx, ]
    fit <- loess_fun(tr)
    pred <- predict(fit, newdata=te)
    # RMSE sobre as taxas [cite: 356]
    sqrt(mean((te$y - pred)^2))
  }, numeric(1))
}

# Função genérica de CV para GAM (M3-M6)
cv_gam <- function(fit_fun, data, folds) {
  mean(vapply(folds, function(idx) {
    tr <- data[-idx,]; te <- data[idx,]
    mod <- fit_fun(tr)
    # Taxas preditas e observadas
    pred_rate <- predict(mod, newdata=te, type="response") / te$E
    obs_rate  <- te$count / te$E
    # CV-RMSE sobre as taxas
    sqrt(mean((pred_rate - obs_rate)^2))
  }, numeric(1)))
}

# Gera Tabela de Diagnóstico LOESS 
diagnose_loess <- function(fit_m1, fit_m2, data_loess, cv_rmse_m1, cv_rmse_m2) {
  loess_diag <- function(fit, data, model_name) {
    pred <- fit$fitted
    rmse <- sqrt(mean((pred - data$y)^2))
    tibble(
      Model = model_name,
      span = fit$pars$span,
      df = fit$trace.hat,
      RMSE = rmse
    )
  }
  
  diag_m1 <- loess_diag(fit_m1$model, data_loess, "M1 (Linha-Coluna)")
  diag_m2 <- loess_diag(fit_m2$model, data_loess, "M2 (Linha-Coorte)")
  
  diag_loess <- bind_rows(diag_m1, diag_m2) %>%
    mutate(CV_RMSE = c(cv_rmse_m1, cv_rmse_m2)) %>%
    select(Model, span, df, RMSE, CV_RMSE)
  
  return(diag_loess)
}

# Gera Tabela de Diagnóstico GAM NB 
diagnose_gam_nb <- function(models_list, cv_res_nb) {
  train_res_nb <- tibble()
  
  for(name in names(models_list)) {
    fit <- models_list[[name]]
    su  <- summary(fit)
    
    # k′ = (k1 × k2) – 1 [cite: 321]
    k1 <- fit$smooth[[1]]$margin[[1]]$bs.dim
    k2 <- fit$smooth[[1]]$margin[[2]]$bs.dim
    kprime <- k1 * k2 - 1
    
    edf    <- sum(su$edf)
    kindex <- edf / kprime # Índice de Checagem de Base 
    score  <- AIC(fit) # NB usa AIC 
    
    train_res_nb <- train_res_nb %>% add_row(
      Model = name,
      kprime = kprime,
      edf = edf,
      kindex = kindex,
      DevExp = su$dev.expl * 100,
      R2adj = su$r.sq,
      Score = score
    )
  }
  
  comparison_nb <- train_res_nb %>%
    left_join(cv_res_nb, by = "Model")
  
  return(comparison_nb)
}