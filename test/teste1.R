source("src/modelo_sim.R")
source("src/util.R")
source("src/modelo_est.R")
source("src/Analise de reisduos.R")

# Exemplo 1 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1000), c(999, n)))

pars <- list(ar = 0.2, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = 1, 
             deltaVar = c(-5, -3.5))

pars_init <- list(
  psi2 = log(.9),
  ar = .2,
  deltaMedia = 1,
  deltaVar = c(-5, -3.5)
)
alpha_order <- length(pars_init$psi2)
beta_order <- length(pars_init$psi3)
kmed <- 1
kvar <- 2

set.seed(1000)
data <- modelo(pars, dummy1, dummy2, n)
yt <- data$yt
Varyt <- var(yt[1:50])

plot(yt, type = 'l')
acf(yt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(yt, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

pars <- unlist(pars_init, use.names = F)

(opt <- estimando(llike_model_arch, pars_init))

media_cond <- esp_cond_geral(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

var_cond <- var_cond_geral_arch(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order = alpha_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

resid_pad <- (yt - media_cond)/sqrt(var_cond)
resid_pad <- resid_pad[-(1:50)]

plot(resid_pad, type = 'l')

mean(resid_pad)
var(resid_pad)

acf(resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))

resid_pad_data <- data.frame(resid_pad = resid_pad, time = seq_along(resid_pad))

## QQplot e Histograma - INICIO
ggplot(resid_pad_data, aes(sample = resid_pad)) + 
  stat_qq() + 
  geom_abline(slope = 1, intercept = 0) + 
  ylim(-6,6) + 
  scale_x_continuous(limits = c(-6, 6),  breaks = c(-6, -4, -2, 0, 2, 4, 6))

ggplot(resid_pad_data, aes(x = resid_pad)) + 
  geom_histogram(aes(y =..density..), fill = "#0c4c8a") +
  theme_minimal() +
  labs(x = "Residuos padronizados", y = 'Densidade') + 
  scale_x_continuous(limits = c(-6, 6),  breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
  stat_function(fun = dnorm, args = list(0, 1), color = 'red')
## QQplot e Histograma - FIM

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond)^2)/sum((yt - media_cond)^2))

# Exemplo 2 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1000), c(999, n)))

pars <- list(ar = 0.2, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = 1, 
             deltaVar = c(-5, -1))

pars_init <- list(
  psi2 = log(c(.6, .38)),
  ar = .2,
  deltaMedia = 1,
  deltaVar = c(-5, -1)
)
alpha_order <- length(pars_init$psi2)
kmed <- 1
kvar <- 1

data <- modelo(pars, dummy1, dummy2, n)
yt <- data$yt
Varyt <- var(yt[1:50])
pars <- unlist(pars_init, use.names = F)

estimando(llike_model_arch, pars_init)
# Exemplo 3 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1000), c(999, n)))

pars <- list(ar = 0.2, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = 1, 
             deltaVar = c(-5, -1))

pars_init <- list(
  psi2 = log(c(.6, .1, .1, .1)),
  ar = .2,
  deltaMedia = 1,
  deltaVar = c(-5, -1)
)
alpha_order <- length(pars_init$psi2)
kmed <- 1
kvar <- 1

data <- modelo(pars, dummy1, dummy2, n)
yt <- data$yt
Varyt <- var(yt[1:50])
pars <- unlist(pars_init, use.names = F)

estimando(llike_model_arch, pars_init)
# Exemplo 4 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1000), c(999, n)))

pars <- list(ar = .6, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = 1, 
             deltaVar = c(-5, -1))

pars_init <- list(
  psi2 = log(.1),
  psi3 = log(.9),
  ar = .6,
  deltaMedia = 1,
  deltaVar = c(-5, -5)
)
alpha_order <- length(pars_init$psi2)
beta_order <- length(pars_init$psi3)
kmed <- length(pars_init$deltaMedia)
kvar <- length(pars_init$deltaVar)

data <- modelo(pars, dummy1, dummy2, n)
yt <- data$yt
Varyt <- var(yt[1:50])
pars <- unlist(pars_init, use.names = F)

estimando(llike_model_garch, pars_init)

# Exemplo 5 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1000), c(999, n)))

pars <- list(ar = 0.2, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = 1, 
             deltaVar = c(-5, -1))

pars_init <- list(
  psi2 = log(.1),
  psi3 = log(c(.75, .15)),
  ar = .2,
  deltaMedia = 1,
  deltaVar = c(-5, -1)
)
alpha_order <- length(pars_init$psi2)
beta_order <- length(pars_init$psi3)
kmed <- 1
kvar <- 2

data <- modelo(pars, dummy1, dummy2, n)
yt <- data$yt
Varyt <- var(yt[1:50])
pars <- unlist(pars_init, use.names = F)

(opt <- estimando(llike_model_garch, pars_init))

media_cond <- esp_cond_geral(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

var_cond <- var_cond_geral(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

resid_pad <- (yt - media_cond)/sqrt(var_cond)

mean(resid_pad[-c(1, 2)])
var(resid_pad[-c(1, 2)])

plot(resid_pad[-c(1, 2)], type = 'l')

# Exemplo 6 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_on_off(n, c(1, 1500), c(1499, n), "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1000), c(999, n)))

pars <- list(ar = 0.2, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = c(1, 2), 
             deltaVar = c(-5, -1))

pars_init <- list(
  psi2 = log(c(.03, .07)),
  psi3 = log(c(.75, .15)),
  ar = .2,
  deltaMedia = c(1, 2),
  deltaVar = c(-5, -1)
)
alpha_order <- length(pars_init$psi2)
beta_order <- length(pars_init$psi3)
kmed <- 2
kvar <- 2

data <- modelo(pars, dummy1, dummy2, n)
yt <- data$yt
Varyt <- var(yt[1:50])
pars <- unlist(pars_init, use.names = F)

(opt <- estimando(llike_model_garch, pars_init))

media_cond <- esp_cond_geral(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

var_cond <- var_cond_geral(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

resid_pad <- (yt - media_cond)/sqrt(var_cond)

resid_pad_data <- data.frame(resid_pad = resid_pad, time = seq_along(resid_pad))
resid_pad_data <- resid_pad_data[-1, ]

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

plot(resid_pad_data$resid_pad, type = 'l')
