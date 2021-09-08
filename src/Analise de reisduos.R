# Media Condicional ------------------------------------------------------

## Funcao para estimar a media condicional do modelo - INICIO
esp_cond_model <- function(data, est, dummy1, dummy2,
                           alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Estimativas - INICIO
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos estimar media condicional - INICIO
  Xt <- (yt - int1)/int2
  media_cond <- rep(NA, n)
  # Insumos estimar media condicional - FIM
  
  # Calculando media condicional - INICIO
  media_cond[1] <- int1[1] # media_cond[t] <- int1 + int2*media_condAR
  media_cond[2:n] <- int1[2:n] + int2[2:n] * (ar * Xt[-length(Xt)])
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional do modelo - FIM

## Funcao para estimar a media condicional do modelo com AR-ARCH - INICIO
esp_cond_model_arch <- function(data, est, dummy1, dummy2,
                                 alpha_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, 1, kmed, kvar))
  
  # Estimativas - INICIO
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos estimar media condicional - INICIO
  Xt <- (yt - int1)/int2
  media_cond <- rep(NA, n)
  # Insumos estimar media condicional - FIM
  
  # Calculando media condicional - INICIO
  media_cond[1] <- int1[1] # media_cond[t] <- int1 + int2*media_condAR
  media_cond[2:n] <- int1[2:n] + int2[2:n] * (ar * Xt[-length(Xt)])
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional do modelo com AR-ARCH - FIM

## Funcao para estimar a media condicional do modelo c/ int suave - INICIO
esp_cond_sauve <- function(data, est, dummy1, dummy2, t_ast, t_til, delta_ind,
                           alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Estimativas - INICIO
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  
  for (i in seq_along(delta_ind)){
    int2[t_ast[i]:t_til[i]] <- reta(delta2[delta_ind[i]]/2, 
                                    delta2[delta_ind[i] + 1]/2, 
                                    t_ast[i], t_til[i], t_ast[i]:t_til[i])
  }
  # Efeito regressao - FIM
  
  # Insumos estimar media condicional - INICIO
  Xt <- (yt - int1)/int2
  media_cond <- rep(NA, n)
  # Insumos estimar media condicional - FIM
  
  # Calculando media condicional - INICIO
  media_cond[1] <- int1[1] # media_cond[t] <- int1 + int2*media_condAR
  media_cond[2:n] <- int1[2:n] + int2[2:n] * (ar * Xt[-length(Xt)])
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional do modelo c/ int suave - FIM

## Funcao para estimar a media condicional de ARCH c/ int na media- INICIO
esp_cond_arch <- function(data, est, dummy1,
                          alpha_order, kmed, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(1, alpha_order, 1, kmed))
  
  # Estimativas - INICIO
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  # Efeito regressao - FIM
  
  # Insumos estimar media condicional - INICIO
  Xt <- yt - int1
  media_cond <- rep(NA, n)
  # Insumos estimar media condicional - FIM
  
  # Calculando media condicional - INICIO
  media_cond[1] <- int1[1] # media_cond[t] <- int1 + media_condAR
  media_cond[2:n] <- int1[2:n] + ar * Xt[-length(Xt)]
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional de ARCH c/ int na media - FIM

## Funcao para estimar a media condicional de GARCH c/ int na media - INICIO
esp_cond_garch <- function(data, est, dummy1,
                           alpha_order, beta_order, kmed, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(1, alpha_order, beta_order, 1, kmed))
  
  # Estimativas - INICIO
  ar <- est[pos[4]]
  delta1 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  # Efeito regressao - FIM
  
  # Insumos estimar media condicional - INICIO
  Xt <- yt - int1
  media_cond <- rep(NA, n)
  # Insumos estimar media condicional - FIM
  
  # Calculando media condicional - INICIO
  media_cond[1] <- int1[1] # media_cond[t] <- int1 + media_condAR
  media_cond[2:n] <- int1[2:n] + ar * Xt[-length(Xt)]
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional de GARCH c/ int na media - FIM

# Variancia Condicional ---------------------------------------------------

## Funcao para estimar a media condicional do modelo - INICIO
var_cond_model <- function(data, est, dummy1, dummy2, Varyt,
                           alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Estimativas - INICIO
  alpha <- est[1:pos[1]]
  beta <- est[(pos[1] + 1):(pos[2])]
  
  ar <- est[pos[3]]
  
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- (yt - int1)/int2
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar * Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order, beta_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  var_cond <- (int2^2) * sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a media condicional do modelo - FIM

## Funcao para estimar a media condicional do modelo com AR-ARCH - INICIO
var_cond_model_arch  <- function(data, est, dummy1, dummy2, Varyt,
                                 alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, 1, kmed, kvar))
  # Estimativas - INICIO
  alpha <- est[1:pos[1]]
  beta <- est[(pos[1] + 1):(pos[2])]
  
  ar <- est[pos[3]]
  
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Definicao dos parametros - FIM
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- (yt - int1)/int2
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar * Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order, beta_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) 
  }
  var_cond <- (int2^2) * sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a media condicional do modelo com AR-ARCH - FIM

## Funcao para estimar a var condicional do modelo c/ int suave - INICIO
var_cond_sauve <- function(data, est, dummy1, dummy2, 
                           t_ast, t_til, delta_ind, Varyt, 
                           alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  # Estimativas - INICIO
  alpha <- est[1:pos[1]]
  beta <- est[(pos[1] + 1):(pos[2])]
  
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  
  for (i in seq_along(delta_ind)){
    int2[t_ast[i]:t_til[i]] <- reta(delta2[delta_ind[i]]/2, 
                                    delta2[delta_ind[i] + 1]/2, 
                                    t_ast[i], t_til[i], t_ast[i]:t_til[i])
  }
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- (yt - int1)/int2
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar * Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order, beta_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha * (epst[i - (1:alpha_order)]) ^ 2) +
      sum(beta * sigma2[i - (1:beta_order)])
  }
  var_cond <- (int2^2) * sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a var condicional do modelo c/ int suave - FIM

## Funcao para estimar a var condicional do modelo com ARCH c/ int na media - INICIO
var_cond_arch <- function(data, est, dummy1, Varyt,
                          alpha_order, kmed, kvar, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(1, alpha_order, 1, kmed))
  
  # Estimativas - INICIO
  omega <- est[pos[1]]
  alpha <- est[2:pos[2]]
  
  ar <- est[pos[3]]
  
  delta1 <- est[(pos[3] + 1):(pos[4])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- yt - int1
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar * Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- omega +
      sum(alpha * (epst[i - (1:alpha_order)]) ^ 2)
  }
  var_cond <- sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a var condicional do modelo com ARCH c/ int na media - FIM

## Funcao para estimar a media condicional com GARCH c/ int na media - INICIO
var_cond_garch <- function(data, est, dummy1, Varyt, 
                           alpha_order, beta_order, kmed, n){
  
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(1, alpha_order, beta_order, 1, kmed))
  
  # Estimativas - INICIO
  omega <- est[pos[1]]
  alpha <- est[2:pos[2]]
  beta <- est[(pos[2] + 1):(pos[3])]
  
  ar <- est[pos[4]]
  
  delta1 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- yt - int1
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar*Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order, beta_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- omega +
      sum(alpha * (epst[i - (1:alpha_order)]) ^ 2) +
      sum(beta * sigma2[i - (1:beta_order)])
  }
  var_cond <- sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a media condicional com GARCH c/ int na media - FIM

# Variancia Incondicional -------------------------------------------------

## Função para estimar a variancia incondicional do modelo - INICIO
var_indcond <- function(data, est, dummy1, dummy2,
                        alpha_order, beta_order, kmed, kvar){
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  # Estimativas - INICIO
  omega <- 1
  alpha <- est[1:pos[1]]
  beta <- est[(pos[1] + 1):(pos[2])]
  
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  var_ind <- (int2^2) * (1/(1 - sum(alpha) - sum(beta))) * (1/(1-ar^2))
  
  return(var_ind)
}
## Função para estimar a variancia incondicional do modelo - FIM

## Função para estimar a variancia incondicional do modelo c/ int suave - INICIO
var_indcond_sauve <- function(data, est, dummy1, dummy2, 
                              t_ast, t_til, delta_ind,
                              alpha_order, beta_order, kmed, kvar){
  est <- as.matrix(est) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  # Estimativas - INICIO
  omega <- 1
  alpha <- est[1:pos[1]]
  beta <- est[(pos[1] + 1):(pos[2])]
  
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Definicao dos parametros - FIM
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int2 <- exp((dummy2%*%delta2)/2)
  
  for (i in seq_along(delta_ind)){
    int2[t_ast[i]:t_til[i]] <- reta(delta2[delta_ind[i]]/2, 
                                    delta2[delta_ind[i] + 1]/2, 
                                    t_ast[i], t_til[i], t_ast[i]:t_til[i])
  }
  # Efeito regressao - FIM
  
  var_ind <- (int2^2) * (1/(1 - sum(alpha) - sum(beta))) * (1/(1-ar^2))
  
  return(var_ind)
}
## Função para estimar a variancia incondicional do modelo c/ int suave - FIM