require(dplyr)
source('src/util.R')

# Dummy1 => Delta1, Dummy2 => Delta2

# Modelo Gaussiano -----------------------------------------------------------

# Simulacao de um GARCH - INICIO
Garch <- function(pars, n){
  
  # Definicao dos parametros - INICIO
  omega <- pars$omega
  alpha <- pars$alpha
  p <- length(alpha)
  
  beta <- pars$beta
  q <- length(beta)
  # Definicao dos parametros - FIM
  
  # Inicizalizando as variaveis - INICIO
  n.burn <- 50
  m <- max(q, p)
  N <- n + n.burn
  rt <- numeric(N)
  sigma2 <- numeric(N)
  # Inicizalizando as variaveis - FIM
  
  # Armazenando os primeiros valores de sigma2 e rt - INICIO
  r20 <- rep(omega/(1-sum(alpha)-sum(beta)), m)
  sigma2[1:m] <- omega + sum(r20*alpha) # Estou colocando sigma_{0}^2 = 0 
  rt[1:m] <- sqrt(sigma2[m])*rnorm(m)
  # Armazenando os primeiros valores de sigma2 e rt - FIM
  
  # Iniciando a recursao - INICIO
  for (i in (m+1):N){
    sigma2[i] <- omega + sum(alpha*(rt[i-(1:p)])^2) + 
      sum(beta*sigma2[i-(1:q)])
    rt[i] <- sqrt(sigma2[i])*rnorm(1)
  }
  # Iniciando a recursao - FIM
  
  # Ignorando os valores iniciais passando pelo n.burn - INICIO
  rt <- rt[(n.burn+1):N]
  sigma2 <- sigma2[(n.burn+1):N]
  # Ignorando os valores iniciais passando pelo n.burn - FIM
  
  # Gerando dataframe com a serie e outras informacoes - INICIO
  return(data.frame(rt = rt, sigma2 = sigma2, rt2 = rt^2, time = 1:n))
  # Gerando dataframe com a serie e outras informacoes - FIM
}
# Simulacao de um GARCH - FIM

# Simulacao de um AR - GARCH - INICIO
AR_Garch <- function(pars, n){
  
  # Definicao dos parametros - INICIO
  phi <- pars$ar
  s <- length(phi)
  # Definicao dos parametros - FIM
  
  # Inicizalizando as variaveis - INICIO
  n.burn <- 50
  N <- n + n.burn
  rt <- numeric(N)
  # Inicizalizando as variaveis - FIM
  
  # Gerando o epsilon_t - INICIO
  pars_garch <- list(omega =pars$omega, alpha = pars$alpha, beta = pars$beta)
  epst <- Garch(pars_garch, N)$rt
  # Gerando o epsilon_t - FIM
  
  # Iniciando a recursao - INICIO
  for (i in (s+1):N){
    rt[i] <- sum(phi*rt[i-(1:s)]) + epst[i]
  }
  # Iniciando a recursao - FIM
  
  # Ignorando os valores iniciais passando pelo n.burn - INICIO
  burn <- (n.burn+1):N
  rt <- rt[burn]
  epst <- epst[burn]
  # Ignorando os valores iniciais passando pelo n.burn - FIM
  
  # Gerando dataframe com a serie e outras informacoes - INICIO
  return(data.frame(rt = rt, rt2 = rt^2, epst = epst, time = 1:n))
  # Gerando dataframe com a serie e outras informacoes - FIM
}
# Simulacao de um AR-GARCH - FIM

# Simulacao de modelo - INICIO
modelo <- function(pars, dummy1, dummy2, n){
  
  # Tamanho da series antes de queimar - INICIO
  n.burn <- 100
  N <- n + n.burn
  # Tamanho da series antes de queimar - FIM
  
  # Gerando AR-GARCH - INICIO
  Xt <- AR_Garch(pars, N)
  # Gerando AR-GARCH - FIM
  
  # Inicizalizando as variaveis - INICIO
  yt <- numeric(N)
  int1 <- numeric(N)
  int2 <- numeric(N)
  delta1 <- pars$deltaMedia
  delta2 <- pars$deltaVar
  # Inicizalizando as variaveis - FIM
  
  # Gerando dummy1 e dummy2 com shift no inicio- INICIO
  dummy1 <- rbind(matrix(0, ncol = length(delta1), nrow = 100), dummy1)
  dummy2 <- rbind(matrix(0, ncol = length(delta2), nrow = 100), dummy2)
  # Gerando dummy1 e dummy2 com shift no inicio - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  # Efeito regressao - FIM
  
  # Gerando a serie final - INICIO
  yt <- int1 + int2*Xt$rt
  # Gerando a serie final - FIM
  
  # Ignorando os valores iniciais passando pelo n.burn - INICIO
  burn <- (n.burn+1):N
  yt <- yt[burn, ]
  Xt <- Xt[burn, ]
  int1 <- int1[burn]
  int2 <- int2[burn]
  dummy1 <- dummy1[burn, ]
  dummy2 <- dummy2[burn, ]
  # Ignorando os valores iniciais passando pelo n.burn - FIM
  
  # Gerando dataframe com a serie e outras informacoes - INICIO
  yt <- data.frame(yt = yt, time = 1:n, xt = Xt$rt, 
                   int1 = int1, int2 = int2) %>% 
    cbind(dummy1, dummy2) %>% 
    select(yt, xt, int1, int2, starts_with("Dummy"), 
           everything()) 
  # Gerando dataframe com a serie e outras informacoes - FIM
  
  return(yt)
}
# Simulacao de modelo - FIM

# Simulacao de modelo com intervenção suave - INICIO
modelo_smooth <- function(pars, dummy1, dummy2, t_ast, t_til, n){
  
  # t_til > t_est
  
  # Tamanho da series antes de queimar - INICIO
  n.burn <- 0
  N <- n + n.burn
  # Tamanho da series antes de queimar - FIM
  
  # Gerando AR-GARCH - INICIO
  Xt <- AR_Garch(pars, N)
  # Gerando AR-GARCH - FIM
  
  # Inicizalizando as variaveis - INICIO
  yt <- numeric(N)
  int1 <- numeric(N)
  int2 <- numeric(N)
  delta1 <- pars$deltaMedia
  delta2 <- pars$deltaVar
  # Inicizalizando as variaveis - FIM
  
  # Gerando dummy1 e dummy2 com shift no inicio- INICIO
  dummy1 <- rbind(matrix(0, ncol = length(delta1), nrow = n.burn), dummy1)
  dummy2 <- rbind(matrix(0, ncol = length(delta2), nrow = n.burn), dummy2)
  # Gerando dummy1 e dummy2 com shift no inicio - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  int2[t_ast:(t_til - 1)] <- reta(delta2[1]/2, delta2[2]/2, t_ast, t_til, t_ast:(t_til - 1))
  # Efeito regressao - FIM
  
  # Gerando a serie final - INICIO
  yt <- int1 + int2*Xt$rt
  # Gerando a serie final - FIM
  
  # Ignorando os valores iniciais passando pelo n.burn - INICIO
  burn <- (n.burn+1):N
  yt <- yt[burn, ]
  Xt <- Xt[burn, ]
  int1 <- int1[burn]
  int2 <- int2[burn]
  dummy1 <- dummy1[burn, ]
  dummy2 <- dummy2[burn, ]
  # Ignorando os valores iniciais passando pelo n.burn - FIM
  
  # Gerando dataframe com a serie e outras informacoes - INICIO
  yt <- data.frame(yt = yt, time = 1:n, xt = Xt$rt, 
                   int1 = int1, int2 = int2) %>% 
    cbind(dummy1, dummy2) %>% 
    select(yt, xt, int1, int2, starts_with("Dummy"), 
           everything()) 
  # Gerando dataframe com a serie e outras informacoes - FIM
  
  return(yt)
}
# Simulacao de modelo com intervenção suave - FIM

# Simulacao de modelo com intervenção suave - INICIO
# TENHO Q MELHORAR MUITO ESSA FUNCAO
modelo_smooth2 <- function(pars, dummy1, dummy2, t_ast, t_til, n){
  
  # t_til > t_est
  
  # Tamanho da series antes de queimar - INICIO
  n.burn <- 0
  N <- n + n.burn
  # Tamanho da series antes de queimar - FIM
  
  # Gerando AR-GARCH - INICIO
  Xt <- AR_Garch(pars, N)
  # Gerando AR-GARCH - FIM
  
  # Inicizalizando as variaveis - INICIO
  yt <- numeric(N)
  int1 <- numeric(N)
  int2 <- numeric(N)
  delta1 <- pars$deltaMedia
  delta2 <- pars$deltaVar
  # Inicizalizando as variaveis - FIM
  
  # Gerando dummy1 e dummy2 com shift no inicio- INICIO
  dummy1 <- rbind(matrix(0, ncol = length(delta1), nrow = n.burn), dummy1)
  dummy2 <- rbind(matrix(0, ncol = length(delta2), nrow = n.burn), dummy2)
  # Gerando dummy1 e dummy2 com shift no inicio - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  
  int2[t_ast[1]:(t_til[1] - 1)] <- reta(delta2[2]/2, delta2[3]/2, 
                                        t_ast[1], t_til[1], t_ast[1]:(t_til[1] - 1))
  int2[t_ast[2]:(t_til[2] - 1)] <- reta(delta2[3]/2, delta2[4]/2, 
                                        t_ast[2], t_til[2], t_ast[2]:(t_til[2] - 1))
  # Efeito regressao - FIM
  
  # Gerando a serie final - INICIO
  yt <- int1 + int2*Xt$rt
  # Gerando a serie final - FIM
  
  # Ignorando os valores iniciais passando pelo n.burn - INICIO
  burn <- (n.burn+1):N
  yt <- yt[burn, ]
  Xt <- Xt[burn, ]
  int1 <- int1[burn]
  int2 <- int2[burn]
  dummy1 <- dummy1[burn, ]
  dummy2 <- dummy2[burn, ]
  # Ignorando os valores iniciais passando pelo n.burn - FIM
  
  # Gerando dataframe com a serie e outras informacoes - INICIO
  yt <- data.frame(yt = yt, time = 1:n, xt = Xt$rt, 
                   int1 = int1, int2 = int2) %>% 
    cbind(dummy1, dummy2) %>% 
    select(yt, xt, int1, int2, starts_with("Dummy"), 
           everything()) 
  # Gerando dataframe com a serie e outras informacoes - FIM
  
  return(yt)
}
# Simulacao de modelo com intervenção suave - FIM

