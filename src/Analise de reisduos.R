## Funcao para estimar a media condicional do modelo completo - INICIO
esp_cond_geral <- function(data, est, dummy1, dummy2,
                           alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(opt) %>% unname()
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
  media_cond[2:n] <- int1[2:n] + int2[2:n]*(ar*Xt[-length(Xt)])
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional do modelo completo - FIM

## Funcao para estimar a var condicional do modelo completo - INICIO
var_cond_geral <- function(data, est, dummy1, dummy2,
                           alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(opt) %>% unname()
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
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- (yt - int1)/int2
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar*Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order, beta_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  var_cond <- (int2^2)*sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a var condicional do modelo completo - FIM

## Funcao para estimar a media condicional do modelo completo - INICIO
esp_cond_geral_sauve <- function(data, est, dummy1, dummy2, t_ast, t_til,
                                 alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(opt) %>% unname()
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Estimativas - INICIO
  ar <- est[pos[3]]
  delta1 <- est[(pos[3] + 1):(pos[4])]
  delta2 <- est[(pos[4] + 1):(pos[5])]
  # Estimativas - FIM
  
  # Efeito regressao - INICIO
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  
  int2[t_ast[1]:(t_til[1])] <- reta(delta2[1]/2, delta2[2]/2, 
                                    t_ast[1], t_til[1], t_ast[1]:t_til[1])
  int2[t_ast[2]:(t_til[2])] <- reta(delta2[2]/2, delta2[3]/2, 
                                    t_ast[2], t_til[2], t_ast[2]:t_til[2])
  int2[t_ast[3]:(t_til[3])] <- reta(delta2[3]/2, delta2[4]/2, 
                                    t_ast[3], t_til[3], t_ast[3]:t_til[3])
  # Efeito regressao - FIM
  # Efeito regressao - FIM
  
  # Insumos estimar media condicional - INICIO
  Xt <- (yt - int1)/int2
  media_cond <- rep(NA, n)
  # Insumos estimar media condicional - FIM
  
  # Calculando media condicional - INICIO
  media_cond[1] <- int1[1] # media_cond[t] <- int1 + int2*media_condAR
  media_cond[2:n] <- int1[2:n] + int2[2:n]*(ar*Xt[-length(Xt)])
  # Calculando media condicional - FIM
  
  return(media_cond)
}
## Funcao para estimar a media condicional do modelo completo - FIM

## Funcao para estimar a var condicional do modelo completo - INICIO
var_cond_geral_sauve <- function(data, est, dummy1, dummy2, t_ast, t_til,
                                 alpha_order, beta_order, kmed, kvar, n){
  
  est <- as.matrix(opt) %>% unname()
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
  int1 <- dummy1%*%delta1
  int2 <- exp((dummy2%*%delta2)/2)
  
  int2[t_ast[1]:(t_til[1])] <- reta(delta2[1]/2, delta2[2]/2, 
                                    t_ast[1], t_til[1], t_ast[1]:t_til[1])
  int2[t_ast[2]:(t_til[2])] <- reta(delta2[2]/2, delta2[3]/2, 
                                    t_ast[2], t_til[2], t_ast[2]:t_til[2])
  int2[t_ast[3]:(t_til[3])] <- reta(delta2[3]/2, delta2[4]/2, 
                                    t_ast[3], t_til[3], t_ast[3]:t_til[3])
  # Efeito regressao - FIM
  # Efeito regressao - FIM
  
  # Insumos estimar variancia condicional - INICIO
  Xt <- (yt - int1)/int2
  
  epst <- numeric(n)
  sigma2 <- rep(NA, n)
  
  epst[1] <- Xt[1]
  epst[2:n] <- Xt[2:n] - ar*Xt[-length(Xt)]
  # Insumos estimar variancia condicional - FIM
  
  # Calculando variancia condicional - INICIO
  m <- max(alpha_order, beta_order)
  sigma2[1:(m + 1)] <- Varyt
  
  for (i in (m + 1):n) {
    sigma2[i] <- 1 +
      sum(alpha*(epst[i-(1:alpha_order)])^2) +
      sum(beta*sigma2[i-(1:beta_order)])
  }
  var_cond <- (int2^2)*sigma2
  # Calculando variancia condicional - FIM
  
  return(var_cond)
}
## Funcao para estimar a var condicional do modelo completo - FIM