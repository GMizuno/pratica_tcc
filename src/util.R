require(purrr)
require(dplyr)

# Modelo novo -------------------------------------------------------------

# Funcao para criar variaveis dummies do tipo step - INICIO
dummy_step <- function(size, time = 1, suffix = "Var"){
  
  dummy <- matrix(NA, nrow = size, ncol = length(time))
  
  for (i in seq_along(time)) {
    dummy[, i] <- c(rep(0, time[i] - 1),
                    rep(1, size - time[i] + 1))
  }
  
  dummy <- as.data.frame(dummy)
  colnames(dummy) <- map_chr(seq_along(time), ~paste0("Dummy_", suffix, .x))
  return(dummy)
}
# Funcao para criar variaveis dummies do tipo step - FIM

# Funcao para criar variaveis dummies do tipo acende/apaga - INICIO
dummy_on_off <- function(n, inicio, fim, suffix = "Var"){
  
  dummy <- matrix(NA, nrow = n, ncol = length(inicio))
  
  for (i in seq_along(inicio)) {
    dummy[, i] <- c(rep(0, inicio[i] - 1),
                    rep(1, fim[i] - inicio[i] + 1),
                    rep(0, n - fim[i]))
  }
  
  dummy <- as.data.frame(dummy)
  colnames(dummy) <- map_chr(seq_along(inicio), ~paste0("Dummy_", suffix, .x))
  return(dummy)
}
# Funcao para criar variaveis dummies do tipo acende/apaga - FIM

# Funçao para achar a reta de suavização - INICIO
reta <- function(delta1, delta2, t_ast, t_til, t){
  intercept <- ((t_til)*exp(delta1) - (t_ast)*exp(delta2))/(t_til - t_ast)
  ang <- (exp(delta2) - exp(delta1))/(t_til - t_ast)
  
  intercept + ang*t
}
# Funçao para achar a reta de suavização - FIM

# Funcao para voltar achar os parametros interpretaveis - INICIO
unpack_exp <- function(pars){
  delta1 <- pars[grepl("^deltaMedia", names(pars))]
  delta2 <- pars[grepl("^deltaVar", names(pars))]
  ar <- pars[grepl("^ar", names(pars))]

  alpha <- exp(pars[grepl("^alpha", names(pars))])
  beta <- exp(pars[grepl("^beta", names(pars))])
  return(c(alpha, beta, ar, delta1, delta2))
}
# Funcao para voltar achar os parametros interpretaveis  - FIM

# Funcao para estimar os modelos e fazer output bonito - INICIO
estimando <- function(model, pars_init){
  # Iniciando a estimacao - INICIO
  
  opt <- optim(unname(unlist(pars_init)), 
               model, 
               method = 'BFGS',
               control = list(fnscale = -1, maxit = 500,
                              reltol = 1e-8, trace = 1, REPORT = 1))
  # Iniciando a estimacao - FIM
  
  # Guardando as estimativas/medidas - INICIO 
  nomes <- names(unlist(pars_init)) 
  nomes <- stringr::str_replace(nomes, 'psi3', 'beta')
  nomes <- stringr::str_replace(nomes, 'psi2', 'alpha')
  names(opt$par) <- nomes
  
  n <- length(yt)
  p <- length(unname(unlist(pars_init)))
  llike <- unname(opt$value)
  ite <- unname(opt$counts[2])
  
  data <- data.frame(t(unpack_exp(opt$par)), 
                     ite = ite, 
                     llike = llike, 
                     AIC = -2*llike + 2*p,
                     BIC = -2*llike + p*log(n))
  # Guardando as estimativas/medidas - FIM
  
  return(data)
}
# Funcao para estimar os modelos e fazer output bonito - INICIO

# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO
estimando_cpp <- function(pars_init, data, dummyMedia, dummyVar, 
                          kvar, n, Varyt){
  # Iniciando a estimacao - INICIO
  opt <- optim(unname(unlist(pars_init)), 
               llike, 
               method = 'BFGS',
               control = list(fnscale = -1, maxit = 500,
                              reltol = 1e-8),
               data = data, dummyMedia = dummyMedia,
               dummyVar = dummyVar, 
               kvar = kvar, n = n, Varyt = Varyt)
  # Iniciando a estimacao - FIM
  
  # Guardando as estimativas/medidas - INICIO
  nomes <- names(unlist(pars_init)) 
  nomes[1] <- "alpha"
  nomes[2] <- "beta"
  names(opt$par) <- nomes
  
  n <- length(yt)
  p <- length(unname(unlist(pars_init)))
  llike <- unname(opt$value)
  ite <- unname(opt$counts[2])
  
  data <- data.frame(t(unpack_exp(opt$par)), 
                     ite = ite, 
                     llike = llike, 
                     AIC = -2*llike + 2*p,
                     BIC = -2*llike + p*log(n))
  
  # Guardando as estimativas/medidas - FIM
  return(data)
}
# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO

# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO
estimando_cpp_geral <- function(pars_init, data, dummyMedia, dummyVar, 
                                kmed, kvar, n, Varyt){
  # Iniciando a estimacao - INICIO
  opt <- optim(unname(unlist(pars_init)), 
               llike_geral, 
               method = 'BFGS',
               control = list(fnscale = -1, maxit = 500),
               data = data, dummyMedia = dummyMedia,
               dummyVar = dummyVar, 
               kvar = kvar, kmed = kmed, n = n, Varyt = Varyt)
  # Iniciando a estimacao - FIM
  
  # Guardando as estimativas/medidas - INICIO
  nomes <- names(unlist(pars_init)) 
  nomes[1] <- "alpha"
  nomes[2] <- "beta"
  names(opt$par) <- nomes
  
  n <- length(yt)
  p <- length(unname(unlist(pars_init)))
  llike <- unname(opt$value)
  ite <- unname(opt$counts[2])
  
  data <- data.frame(t(unpack_exp(opt$par)), 
                     ite = ite, 
                     llike = llike, 
                     AIC = -2*llike + 2*p,
                     BIC = -2*llike + p*log(n))
  
  # Guardando as estimativas/medidas - FIM
  return(data)
}
# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO

# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO
estimando_cpp_geral_suave <- function(pars_init, data, 
                                       dummyMedia, dummyVar,
                                       t_ast, t_til,
                                       kmed, kvar, n, Varyt){
  # Iniciando a estimacao - INICIO
  opt <- optim(unname(unlist(pars_init)), 
               llike_cpp_suave, 
               method = 'BFGS',
               control = list(fnscale = -1, maxit = 500),
               data = data, dummyMedia = dummyMedia,
               dummyVar = dummyVar, 
               t_ast = t_ast, t_til = t_til, 
               kvar = kvar, kmed = kmed, n = n, Varyt = Varyt)
  # Iniciando a estimacao - FIM
  
  # Guardando as estimativas/medidas - INICIO
  nomes <- names(unlist(pars_init)) 
  nomes[1] <- "alpha"
  nomes[2] <- "beta"
  names(opt$par) <- nomes
  
  n <- length(yt)
  p <- length(unname(unlist(pars_init)))
  llike <- unname(opt$value)
  ite <- unname(opt$counts[2])
  
  data <- data.frame(t(unpack_exp(opt$par)), 
                     ite = ite, 
                     llike = llike, 
                     AIC = -2*llike + 2*p,
                     BIC = -2*llike + p*log(n))
  
  # Guardando as estimativas/medidas - FIM
  return(data)
}
# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO

# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO
estimando_cpp_geral_suave2 <- function(pars_init, data, 
                                      dummyMedia, dummyVar,
                                      t_ast, t_til,
                                      kmed, kvar, n, Varyt){
  # Iniciando a estimacao - INICIO
  opt <- optim(unname(unlist(pars_init)), 
               llike_cpp_suave2, 
               method = 'BFGS',
               control = list(fnscale = -1, maxit = 500),
               data = data, dummyMedia = dummyMedia,
               dummyVar = dummyVar, 
               t_ast = t_ast, t_til = t_til, 
               kvar = kvar, kmed = kmed, n = n, Varyt = Varyt)
  # Iniciando a estimacao - FIM
  
  # Guardando as estimativas/medidas - INICIO
  nomes <- names(unlist(pars_init)) 
  nomes[1] <- "alpha"
  nomes[2] <- "beta"
  names(opt$par) <- nomes
  
  n <- length(yt)
  p <- length(unname(unlist(pars_init)))
  llike <- unname(opt$value)
  ite <- unname(opt$counts[2])
  
  data <- data.frame(t(unpack_exp(opt$par)), 
                     ite = ite, 
                     llike = llike, 
                     AIC = -2*llike + 2*p,
                     BIC = -2*llike + p*log(n))
  
  # Guardando as estimativas/medidas - FIM
  return(data)
}
# Funcao para estimar os modelos e fazer output bonito usando c++ - INICIO
