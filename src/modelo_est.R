# Verossimilhanca de um AR-ARCH - INICIO
llike_arch <- function(pars){
  
  pos <- cumsum(c(1, alpha_order, 1, kmed))
  
  # Definicao dos parametros - INICIO
  omega <- exp(pars[1])
  alpha <- exp(pars[2:pos[2]])
  
  ar <- pars[pos[3]]
  
  delta1 <- pars[(pos[3] + 1):(pos[4])]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_yt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt <- yt - int1
  
  media_cond_xt[1] <- Xt[1]
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  sigma2[1:alpha_order] <- Varyt
  
  for (i in (alpha_order + 1):n) {
    sigma2[i] <- omega + sum(alpha*(epst[i-(1:alpha_order)])^2)
  }
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = (int1 + media_cond_xt)[51:n],
                sd = sqrt(sigma2)[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca de um AR-ARCH- FIM

# Verossimilhanca de um AR-GARCH - INICIO
llike_garch <- function(pars){

  pos <- cumsum(c(1, alpha_order, beta_order, 1, kmed))
  
  # Definicao dos parametros - INICIO
  omega <- exp(pars[1])
  alpha <- exp(pars[2:pos[2]])
  beta <- exp(pars[(pos[2] + 1):pos[3]])
  
  ar <- pars[pos[4]]
  
  delta1 <- pars[(pos[4] + 1):(pos[5])]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt <- yt - int1
  
  media_cond_xt[1] <- Xt[1]
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  sigma2[1:alpha_order] <- Varyt
  
  m <- max(alpha_order, beta_order)
  sigma2[1:m] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- omega +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = (int1 + media_cond_xt)[51:n],
                sd = sqrt(sigma2)[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca de um AR-GARCH - FIM

# Verossimilhanca do modelo com AR-ARCH - INICIO
llike_model_arch <- function(pars){
  
  pos <- cumsum(c(alpha_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:pos[1]])
  
  ar <- pars[pos[2]]
  
  delta1 <- pars[(pos[2] + 1):(pos[3])]
  delta2 <- pars[(pos[3] + 1):(pos[4])]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  sigma2[1:alpha_order] <- Varyt
  
  for (i in (alpha_order + 1):n) {
    sigma2[i] <- 1 + sum(alpha*(epst[i-(1:alpha_order)])^2)
  }
  
  media_cond_yt <- int1 + int2*media_cond_xt
  dp_cond_yt <- (int2*sqrt(sigma2))
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = media_cond_yt[51:n],
                sd = dp_cond_yt[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca do modelo com AR-ARCH - FIM

# Verossimilhanca do modelo com AR-GARCH - INICIO
llike_model_garch <- function(pars){
  
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:pos[1]])
  beta <- exp(pars[(pos[1] + 1):(pos[2])])
  
  ar <- pars[pos[3]]
  
  delta1 <- pars[(pos[3] + 1):(pos[4])]
  delta2 <- pars[(pos[4] + 1):(pos[5])]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  m <- max(alpha_order, beta_order)
  sigma2[1:m] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  
  media_cond_yt <- int1 + int2*media_cond_xt
  dp_cond_yt <- (int2*sqrt(sigma2))
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = media_cond_yt[51:n],
                sd = dp_cond_yt[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca do modelo com AR-ARCH - FIM

# Verossimilhanca do modelo com transição suave - INICIO
llike_suave <- function(pars){
  
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:pos[1]])
  beta <- exp(pars[(pos[1] + 1):(pos[2])])
  
  ar <- pars[pos[3]]
  
  delta1 <- pars[(pos[3] + 1):(pos[4])]
  delta2 <- pars[(pos[4] + 1):(pos[5])]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  
  for (i in seq_along(delta_ind)){
    int2[t_ast[i]:t_til[i]] <- reta(delta2[delta_ind[i]]/2, 
                                    delta2[delta_ind[i] + 1]/2, 
                                    t_ast[i], t_til[i], t_ast[i]:t_til[i])
  }
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  m <- max(alpha_order, beta_order)
  sigma2[1:m] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  
  media_cond_yt <- int1 + int2*media_cond_xt
  dp_cond_yt <- (int2*sqrt(sigma2))
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = media_cond_yt[51:n],
                sd = dp_cond_yt[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca do modelo com transição suave - FIM

# Verossimilhanca do modelo AR-ARCH com transição suave - INICIO
llike_suave_arch <- function(pars){
  
  pos <- cumsum(c(alpha_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:pos[1]])
  
  ar <- pars[pos[2]]
  
  delta1 <- pars[(pos[2] + 1):(pos[3])]
  delta2 <- c(pars[(pos[3] + 1):(pos[4])])
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  
  for (i in seq_along(delta_ind)){
    int2[t_ast[i]:t_til[i]] <- reta(delta2[delta_ind[i]]/2, 
                                    delta2[delta_ind[i] + 1]/2, 
                                    t_ast[i], t_til[i], t_ast[i]:t_til[i])
  }
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  m <- max(alpha_order, beta_order)
  sigma2[1:m] <- Varyt
  
  for (i in (alpha_order + 1):n) {
    sigma2[i] <- 1 + sum(alpha*(epst[i-(1:alpha_order)])^2)
  }
  
  media_cond_yt <- int1 + int2*media_cond_xt
  dp_cond_yt <- (int2 * sqrt(sigma2))
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = media_cond_yt[51:n],
                sd = dp_cond_yt[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca do modelo AR-ARCH com transição suave - FIM

# Verossimilhanca do modelo com transição suave - INICIO
llike_suave <- function(pars){
  
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:pos[1]])
  beta <- exp(pars[(pos[1] + 1):(pos[2])])
  
  ar <- pars[pos[3]]
  
  delta1 <- pars[(pos[3] + 1):(pos[4])]
  delta2 <- pars[(pos[4] + 1):(pos[5])]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  
  for (i in seq_along(delta_ind)){
    int2[t_ast[i]:t_til[i]] <- reta(delta2[delta_ind[i]]/2, 
                                    delta2[delta_ind[i] + 1]/2, 
                                    t_ast[i], t_til[i], t_ast[i]:t_til[i])
  }
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  m <- max(alpha_order, beta_order)
  sigma2[1:m] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  
  media_cond_yt <- int1 + int2*media_cond_xt
  dp_cond_yt <- (int2*sqrt(sigma2))
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = media_cond_yt[51:n],
                sd = dp_cond_yt[51:n], 
                log = TRUE)
  
  return(sum(soma))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca do modelo com transição suave - FIM