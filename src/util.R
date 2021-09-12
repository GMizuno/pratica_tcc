require(purrr)
require(dplyr)

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

  omega <- exp(pars[grepl("^omega", names(pars))])
  alpha <- exp(pars[grepl("^alpha", names(pars))])
  beta <- exp(pars[grepl("^beta", names(pars))])
  return(c(omega, alpha, beta, ar, delta1, delta2))
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
  # Guardando as estimativas/medidas - FIM
  
  return(data)
}
# Funcao para estimar os modelos e fazer output bonito - INICIO

# Funcao para dar chute inicial no DeltaVar - INICIO
chute_inicial <- function(delta1, chute){
  accumulate(chute, `*`) %>% log() %>% map_dbl(\(x) delta1 + 2*x)
}
# Funcao para dar chute inicial no DeltaVar - FIM

# Teste LR - INICIO
teste_lr <- function(model_com, model_red, alpha = .05){
  p <- length(model_com) - 5 # Tirando o q nao eh parametro
  q <- length(model_red) - 5 # Tirando o q nao eh parametro
  
  lambda <- -2*(model_red$llike - model_com$llike)
  pvalue <- pchisq(lambda, p - q, lower.tail = FALSE)
  return(data.frame(lambda = lambda, pvalue = pvalue, df = p - q))
}
# Teste LR - FIM

# Funcao para calcular o poder preditivo - INICIO
poder_pred <- function(yt, media_cond, var_cond){
  xi <- ((yt - media_cond)^2)[-c(1:50)]
  xi_hat <- var_cond[-c(1:50)]
  
  mape <- abs((xi - xi_hat)/xi) %>% mean() * 100
  mse <- (xi - xi_hat)^2 %>% sum()
  
  return(list(mape = mape, mse = mse, rmse = sqrt(mse)))
}
# Funcao para calcular o poder preditivo - FIM
