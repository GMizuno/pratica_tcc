# Modelo diferente 

# Verossimilhanca do modelo com transição suave - INICIO
llike_france <- function(pars){
  
  pos <- cumsum(c(alpha_order, beta_order, 1, kmed, kvar))
  
  # Definicao dos parametros - INICIO
  alpha <- exp(pars[1:pos[1]])
  beta <- exp(pars[(pos[1] + 1):(pos[2])])
  
  ar <- pars[pos[3]]
  
  delta1 <- pars[(pos[3] + 1):(pos[4])]
  delta2 <- c(pars[(pos[4] + 1):(pos[5])], pars[(pos[4] + 2)])
  # Definicao dos parametros - FIM
  
  # Momentos condicionais - INICIO
  sigma2 <- rep(NA, n)
  media_cond_xt <- rep(NA, n)
  # Momentos condicionais - FIM
  
  # Efeito regressao - INICIO
  int1 <-  as.matrix(dummy1)%*%delta1
  
  int2 <- exp((as.matrix(dummy2)%*%delta2)/2)
  
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
      sum(alpha*(epst[i-(1:alpha_order)])^2) + sum(beta*sigma2[i-(1:beta_order)])
  }
  
  media_cond_yt <- int1 + int2*media_cond_xt
  dp_cond_yt <- (int2 * sqrt(sigma2))
  # Insumos verossimilhanca - FIM
  
  # Computando verossimilhanca - INICIO
  soma <- dnorm(x = yt[51:n], 
                mean = media_cond_yt[51:n],
                sd = dp_cond_yt[51:n], 
                log = TRUE)
  
  var_indcond <- (int2^2) * (1/(1 - sum(alpha) - sum(beta))) * (1/(1-ar^2))
  
  return(list(
    llike = sum(soma),
    media_cond = media_cond_yt,
    dp_cond = dp_cond_yt,
    var_indcond = var_indcond
  ))
  # Computando verossimilhanca - FIM
}
# Verossimilhanca do modelo com transição suave - FIM

# Funcao para estimar os modelos e fazer output bonito do modelo 8 fr - INICIO
estimando_france <- function(model, pars_init){
  # Iniciando a estimacao - INICIO
  
  opt <- optim(unname(unlist(pars_init)), 
               \(x) model(x)$llike, 
               method = 'BFGS',
               control = list(fnscale = -1, maxit = 500,
                              reltol = 1e-8, trace = 1, REPORT = 1))
  # Iniciando a estimacao - FIM
  
  # Guardando as estimativas/medidas - INICIO 
  nomes <- names(unlist(pars_init)) 
  nomes <- stringr::str_replace(nomes, 'psi1', 'omega')
  nomes <- stringr::str_replace(nomes, 'psi2', 'alpha')
  nomes <- stringr::str_replace(nomes, 'psi3', 'beta')
  names(opt$par) <- nomes
  
  n <- length(yt)
  p <- length(unname(unlist(pars_init)))
  llike <- unname(opt$value)
  ite <- unname(opt$counts[2])
  
  data <- data.frame(t(unpack_exp(opt$par)), 
                     ite = ite, 
                     llike = llike, 
                     num_par = p,
                     AIC = -2*llike + 2*p,
                     BIC = -2*llike + p*log(n-50))
  
  output <-
    list(
      data = data,
      media_cond = model(opt$par)$media_cond,
      dp_cond = model(opt$par)$dp_cond, 
      var_indcond = model(opt$par)$var_indcond
    )
  # Guardando as estimativas/medidas - FIM
  
  return(output)
}
# Funcao para estimar os modelos e fazer output bonito do modelo 8 fr - FIM