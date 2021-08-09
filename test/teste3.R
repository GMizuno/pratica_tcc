source("src/modelo_sim.R")
source("src/util.R")
source("src/modelo_est.R")
source("src/Analise de reisduos.R")

library(fGarch)

# Exemplo 1 - AR(1)-GARCH(1,1)  ------------------------------------------------

n <- 2000
spec <- garchSpec(model = list(mu = 0, ar = .5, omega = 1))
dados <- garchSim(spec, n = n)
yt <- dados$garch

pars_init <- list(
  psi1 = log(1),
  psi2 = log(.1),
  psi3 = log(.8),
  ar = .5
)
alpha_order <- pars_init$psi2 %>% length()
beta_order <- pars_init$psi3 %>% length()
Varyt <- var(yt[1:50])


fit <- garchFit(garch ~ arma(1,0) + garch(1, 1), data = dados, 
                include.mean = FALSE, trace = FALSE)

fit@fit[["coef"]]
estimando_garch(llike_garch, pars_init)

# Exemplo 2 - AR(1)-GARCH(1,2)  ------------------------------------------------

n <- 2000
spec = garchSpec(model = list(mu = 0, ar = .5, 
                              omega = 1, beta = c(.4, .4), alpha = .15))
dados <- garchSim(spec, n = n)
yt <- dados$garch

pars_init <- list(
  psi1 = log(1),
  psi2 = log(.15),
  psi3 = log(c(.4, .4)),
  ar = .5
)
alpha_order <- pars_init$psi2 %>% length()
beta_order <- pars_init$psi3 %>% length()
Varyt <- var(yt[1:50])


fit <- garchFit(garch ~ arma(1,0) + garch(1, 2), data = dados, 
                include.mean = FALSE, trace = FALSE)

fit@fit[["coef"]]
estimando_garch(llike_garch, pars_init)

# Exemplo 2 - AR(1)-GARCH(2,1)  ------------------------------------------------
n <- 2000
spec = garchSpec(model = list(mu = 0, ar = .5, 
                              omega = 1, alpha = c(.2, .1), beta = .69))
dados <- garchSim(spec, n = n)
yt <- dados$garch

pars_init <- list(
  psi1 = log(1),
  psi2 = log(c(.25, .1)),
  psi3 = log(.69),
  ar = .5
)
alpha_order <- pars_init$psi2 %>% length()
beta_order <- pars_init$psi3 %>% length()
Varyt <- var(yt[1:50])


fit <- garchFit(garch ~ arma(1,0) + garch(2, 1), data = dados, 
                include.mean = FALSE, trace = FALSE)

fit@fit[["coef"]]
estimando_garch(llike_garch, pars_init)
# Exemplo 2 - AR(1)-GARCH(2,2)  ------------------------------------------------
n <- 2000
spec = garchSpec(model = list(mu = 0, ar = .5, 
                              omega = 1, alpha = c(.1, .1), beta = c(.4, .35)))
dados <- garchSim(spec, n = n)
yt <- dados$garch

pars_init <- list(
  psi1 = log(1),
  psi2 = log(c(.1, .05)),
  psi3 = log(c(.4, .4)),
  ar = .5
)
alpha_order <- pars_init$psi2 %>% length()
beta_order <- pars_init$psi3 %>% length()
Varyt <- var(yt[1:50])

fit <- garchFit(garch ~ arma(1,0) + garch(2, 2), data = dados, 
                include.mean = FALSE, trace = FALSE)

fit@fit[["coef"]]
estimando_garch(llike_garch, pars_init)
# Exemplo 3 - AR(1)-ARCH(1)  ------------------------------------------------

n <- 2000
spec = garchSpec(model = list(mu = 0, ar = .5, omega = 1, beta = 0, alpha = .7))
dados <- garchSim(spec, n = n)
yt <- dados$garch

pars_init <- list(
  psi1 = log(1),
  psi2 = log(.1),
  ar = .5
)
alpha_order <- pars_init$psi2 %>% length()
Varyt <- var(yt[1:50])

fit <- garchFit(garch ~ arma(1,0) + garch(1, 0), data = dados, 
                include.mean = FALSE, trace = FALSE)

fit@fit[["coef"]]
estimando_garch(llike_arch, pars_init)
# Exemplo 4 - AR(1)-ARCH(4)  ------------------------------------------------

n <- 2000
spec = garchSpec(model = list(mu = 0, ar = .5, 
                              omega = 1, alpha = c(.1, .15, .1, .6), beta = 0))
dados <- garchSim(spec, n = n)
yt <- dados$garch

pars_init <- list(
  psi1 = log(1),
  psi2 = log(c(.1, .15, .1, .6)),
  ar = .5
)
alpha_order <- pars_init$psi2 %>% length()
beta_order <- pars_init$psi3 %>% length()
Varyt <- var(yt[1:50])


fit <- garchFit(garch ~ arma(1,0) + garch(4, 0), data = dados, 
                include.mean = FALSE, trace = FALSE)

fit@fit[["coef"]]
estimando_garch(llike_arch, pars_init)

