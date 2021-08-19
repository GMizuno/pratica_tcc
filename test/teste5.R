source("src/modelo_sim.R")
source("src/util.R")
source("src/modelo_est.R")
source("src/Analise de reisduos.R")

# Estrutura basicas -  INICIO
M <- 1000

n <- 2000

kc <- 2
kc_red <- 1

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))

dummy2 <- as.matrix(dummy_on_off(n, c(1, 1951),
                                 c(1950, n)))
dummy2_red <- as.matrix(dummy_step(n))
# Estrutura basicas -  FIM


# Definindo parametros - INICIO
deltaVar2_range <- c(seq(-6.5, -3.5, .2), -5)

pars_init_reduzido <- list(psi1 = -5, psi2 = log(.13), psi3 = log(.85),
                           ar = .2,   
                           deltaMedia = 1)

modelo_reduzido <- matrix(data = NA,
                          nrow = M*length(deltaVar2_range),
                          ncol = 10)

modelo_completo <- matrix(data = NA,
                          nrow = M*length(deltaVar2_range),
                          ncol = 10)
# Definindo parametros - FIM

# Iniciando loop para poder do teste - INCIO
inicio <- Sys.time()
set.seed(1)
for (j in seq_along(deltaVar2_range)){
  
  ## Definindo para da serie gerada - INICIO
  pars <- list(ar = .2,
               omega = 1, alpha = .13, beta = .85, 
               deltaMedia = 1, 
               deltaVar = c(-5, deltaVar2_range[j]))
  ## Definindo para da serie gerada - FIM
  
  ## Chute inicial Modelo Completo - INICIO
  pars_init_completo <- list(psi2 = log(.13), psi3 = log(.85),
                             ar = .2,  
                             deltaMedia = 1, 
                             deltaVar = c(-5, deltaVar2_range[j]))
  ## Chute inicial Modelo Completo - FIM
  
  cont <- 1
  
  ## Um dado MC - INICIO
  while (cont <= M){
    
    # Gerando a serie - INICIO
    yt <- modelo_cpp(pars, dummy1, as.matrix(dummy2), n)$yt
    Varyt <- var(yt[1:50])
    # Gerando a serie - FIM
    
    # Estimando Modelo Reduzido e Completo - INICIO
    est_red <- estimando_garch(llike_garch, pars_init_reduzido)
    est_red_vec <- est_red %>% unlist(use.names = F)
    
    est_comp <- estimando_cpp(pars_init_completo, yt, dummy1, dummy2, 
                              kc, n, Varyt)
    est_comp_vec <- est_comp %>% unlist(use.names = F)
    # Estimando Modelo Reduzido e Completo - FIM
    
    if (est_comp$llike >= est_red$llike){
      
      modelo_reduzido[cont + M*(j - 1), ] <- c(est_red_vec, deltaVar2_range[j])
      modelo_completo[cont + M*(j - 1), ] <- est_comp_vec
      
      cat("ite:", cont, "deltaVar2:", deltaVar2_range[j], "\n")
      cont <- cont + 1
    }
  }
  ## Um dado MC - FIM
  
}
fim <- Sys.time()
cat("tempo do loop", fim - inicio)