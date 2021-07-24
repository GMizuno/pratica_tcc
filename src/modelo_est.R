# Modelos -------------------------------------------------------------

# Inciando verossimilhanca do modelo completo - INICIO
llike_model_arch <- function(pars){
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:alpha_order])
  
  ar <- pars[alpha_order + 1]
  
  dummy_order <- alpha_order + 2
  delta1 <- pars[dummy_order:(dummy_order + kmed - 1)]
  delta2 <- pars[-(1:(dummy_order + kmed - 1))]
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
  
  sigma2[1:(alpha_order + 1)] <- Varyt
  
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
# Inciando verossimilhanca do modelo completo - FIM

# Inciando verossimilhanca do modelo completo - INICIO
llike_model_garch <- function(pars){
  
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  omega <- 1
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
  sigma2[1:(m + 1)] <- Varyt
  
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
# Inciando verossimilhanca do modelo completo - FIM

# Inciando verossimilhanca do modelo suave - INICIO
llike_suave <- function(pars){
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1])
  beta <- exp(pars[2])
  
  ar <- pars[3]
  
  delta1 <- pars[4]
  delta2 <- pars[5:(5 + kvar - 1)]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  int2[t_ast:(t_til - 1)] <- reta(delta2[1]/2, delta2[2]/2,
                                  t_ast, t_til, t_ast:(t_til - 1))
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  sigma2[1] <- Varyt
  
  for (i in 2:n) {
    sigma2[i] <- 1 + alpha*(epst[i-1])^2 + beta*sigma2[i-1]
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
# Inciando verossimilhanca do modelo suave - FIM

# Inciando verossimilhanca do modelo suave - INICIO
llike_suave2 <- function(pars){
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1])
  beta <- exp(pars[2])
  
  ar <- pars[3]
  
  delta1 <- pars[4]
  delta2 <- pars[5:(5 + kvar - 1)]
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  int2[t_ast[1]:(t_til[1] - 1)] <- reta(delta2[2]/2, delta2[3]/2, 
                                        t_ast[1], t_til[1], t_ast[1]:(t_til[1] - 1))
  int2[t_ast[2]:(t_til[2] - 1)] <- reta(delta2[3]/2, delta2[4]/2, 
                                        t_ast[2], t_til[2], t_ast[2]:(t_til[2] - 1))
  # Efeito regressao - FIM
  
  # Insumos verossimilhanca - INICIO
  Xt = (yt - int1)/int2
  
  media_cond_xt[1] <- Xt[1]
  
  media_cond_xt[-1] <- ar*Xt[-length(Xt)]
  
  epst <- Xt - media_cond_xt
  
  sigma2[1] <- Varyt
  
  for (i in 2:n) {
    sigma2[i] <- 1 + alpha*(epst[i-1])^2 + beta*sigma2[i-1]
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
# Inciando verossimilhanca do modelo suave - FIM