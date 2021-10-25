source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")
source("src/modelo9_nikkei.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)

# Carregando dados ---------------------------------------------
load("dados/NIKKEI.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28"))

NIKKEI <- NIKKEI %>% 
  fortify.zoo() %>% 
  as_tibble()

ggplot(NIKKEI, aes(x = Index, y = 100 * nikkei)) +
  geom_line(size = 1L, colour = "#112446") + 
  labs(x = "Tempo", y = "Retorno", title = "NIKKEI")

NIKKEI <- NIKKEI %>% 
  dplyr::filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), nikkei = 100*nikkei) %>% 
  select(Index, id, everything()) 

yt <- NIKKEI$nikkei %>% as.vector()
Varyt <- var(yt[1:50])

# Graficos ----------------------------------------------------------------

p1 <- ggplot(NIKKEI, aes(x = Index, y = NIKKEI)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "NIKKEI") +
  theme_minimal()

p3 <- ggplot(NIKKEI, aes(x = Index, y = nikkei)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "NIKKEI com as Restrições") +
  theme_minimal() +
  geom_vline(
    xintercept = as.Date(c("2008-01-01")),
    colour = 'red',
    size = 1.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = as.Date(c("2008-09-15", "2010-07-31")),
    colour = 'blue',
    size = 1.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = as.Date(c("2008-10-10")),
    colour = 'cyan',
    size = 1.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = as.Date(c("2008-10-20", "2010-07-31")),
    colour = 'grey',
    size = 1.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = as.Date(c("2009-01-01")),
    colour = 'yellow',
    size = 1.5,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = as.Date(c("2009-05-13")),
    colour = 'pink',
    size = 1.5,
    linetype = "dashed"
  ) + 
  
p3
gridExtra::grid.arrange(p1, p3, ncol = 1)


ggplot(NIKKEI, aes(x = Index, y = 100 * nikkei)) +
  geom_line(size = 1L, colour = "#112446") + 
  labs(x = "Tempo", y = "Retorno", title = "NIKKEI")
ggsave(r"{graficos\Japan\jap_serie.png}", width = 6, height = 3.5)

acf(yt, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("")
ggsave(r"{graficos\Japan\jap_fac_serie.png}", width = 6, height = 3.5)
pacf(yt, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("")
ggsave(r"{graficos\Japan\jap_facp_serie.png}", width = 6, height = 3.5)

acf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") 
ggsave(r"{graficos\Japan\jap_fac_quad.png}", width = 6, height = 3.5)
pacf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("")
ggsave(r"{graficos\Japan\jap_facp_quad.png}", width = 6, height = 3.5)

# Modelo 01 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -3, -3)
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 3
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227, 1313),
                                 c(984, 1174, 1179, 1312, n)))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt1 <- estimando(llike_suave, pars))

media_cond_mod1 <- esp_cond_sauve(
  data = yt,
  est = opt1,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod1 <- var_cond_sauve(
  data = yt,
  est = opt1,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod1 <- var_indcond_sauve(
  data = yt,
  est = opt1,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod1 <- (yt - media_cond_mod1)/sqrt(var_cond_mod1)
resid_pad_mod1 <- resid_pad_mod1[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod1, 
                             time = seq_along(resid_pad_mod1))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod1, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod1, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)
# cor(var_cond[-(1:50)], ((yt - media_cond)^2)[-(1:50)])
# Estimando e residuos - FIM

# FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
# FAC e FACP - FIM

# QQplot e Histograma - INICIO

library(moments)
skewness(resid_pad_data$resid_pad)
kurtosis(resid_pad_data$resid_pad)

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
# QQplot e Histograma - FIM

# TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod1)^2)/sum((yt - media_cond_mod1)^2))
moments::kurtosis(resid_pad_mod1)
moments::skewness(resid_pad_mod1)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond,
  var_incond = var_incond,
  var_cond = var_cond,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 
  
ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - one_step_predict)^2), colour = "green") 

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia Incondicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - opt1$deltaMedia)^2), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

# Modelo 02 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi1 = log(.5),
  psi2 = log(.1),
  psi3 = log(.84),
  ar = .5,
  deltaMedia = 0
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)

n <- length(yt)
dummy1 <- as.matrix(dummy_step(n, 1, "Media"))

# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt2 <- estimando(llike_garch, pars))

media_cond_mod2 <- esp_cond_garch(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

var_cond_mod2 <- var_cond_garch(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  Varyt = Varyt,
  kmed = kmed,
  n = n
)

resid_pad_mod2 <- (yt - media_cond_mod2)/sqrt(var_cond_mod2)
resid_pad_mod2 <- resid_pad_mod2[-(1:50)]

plot(resid_pad_mod2, type = 'l', ylim = c(-6, 6))

mean(resid_pad_mod2)
var(resid_pad_mod2)

resid_pad_data <- data.frame(resid_pad = resid_pad_mod2, 
                             var_cond = var_cond_mod2[-c(1:50)],
                             time = seq_along(resid_pad_mod2))

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box')
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box')

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod2)^2)/sum((yt - media_cond_mod2)^2))
moments::kurtosis(resid_pad_mod2)
moments::skewness(resid_pad_mod2)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(yt = yt, one_step_predict = media_cond, 
                   var_cond = var_cond, time = 1:n)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "green")
## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 03 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 3
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227),
                                 c(984, 1174, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

(opt3 <- estimando(llike_suave, pars))

media_cond_mod3 <- esp_cond_sauve(
  data = yt,
  est = opt3,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod3 <- var_cond_sauve(
  data = yt,
  est = opt3,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod3 <- var_indcond_sauve(
  data = yt,
  est = opt3,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod3 <- (yt - media_cond_mod3)/sqrt(var_cond_mod3)
resid_pad_mod3 <-resid_pad_mod3[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod3, 
                             time = seq_along(resid_pad_mod3))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod3, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod3, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod3)^2)/sum((yt - media_cond_mod3)^2))
moments::kurtosis(resid_pad_mod3)
moments::skewness(resid_pad_mod3)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond,
  var_incond = var_incond,
  var_cond = var_cond,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond)^2), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia incondicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = yt^2), colour = "blue")
## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 04 ----------------------------------------------------------------
# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -3)
)
## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 2
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1175, 1227, 1313),
                                 c(1174, 1179, 1312, n)))
# Estimando e residuos - INICIO

(opt4 <- estimando(llike_suave, pars))

teste_lr(opt1, opt4)

media_cond_mod4 <- esp_cond_sauve(
  data = yt,
  est = opt4,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod4 <- var_cond_sauve(
  data = yt,
  est = opt4,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod4 <- var_indcond_sauve(
  data = yt,
  est = opt4,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod4 <- (yt - media_cond_mod4)/sqrt(var_cond_mod4)
resid_pad_mod4 <- resid_pad_mod4[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod4, 
                             time = seq_along(resid_pad_mod4))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod4, type = 'l')
plot(var_incond_mod4, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Analisando residuos - FIM

# FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
# FAC e FACP - FIM

# QQplot e Histograma - INICIO
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
# QQplot e Histograma - FIM

# TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box')
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box')

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod4)^2)/sum((yt - media_cond_mod4)^2))
moments::kurtosis(resid_pad_mod4)
moments::skewness(resid_pad_mod4)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond,
  var_incond = var_incond,
  var_cond = var_cond,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

# Modelo 05 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 2
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1175, 1227),
                                 c(1174, 1179, n)))
# Estimando e residuos - INICIO

(opt5 <- estimando(llike_suave, pars))

media_cond_mod5 <- esp_cond_sauve(
  data = yt,
  est = opt4,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod5 <- var_cond_sauve(
  data = yt,
  est = opt4,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod5 <- var_indcond_sauve(
  data = yt,
  est = opt4,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod5 <- (yt - media_cond_mod5)/sqrt(var_cond_mod5)
resid_pad_mod5 <- resid_pad_mod5[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod5, 
                             time = seq_along(resid_pad_mod5))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad, type = 'l')
plot(var_incond, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Analisando residuos - FIM

# FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
# FAC e FACP - FIM

# QQplot e Histograma - INICIO
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
# QQplot e Histograma - FIM

# TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod5)^2)/sum((yt - media_cond_mod5)^2))
moments::kurtosis(resid_pad_mod5)
moments::skewness(resid_pad_mod5)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond,
  var_incond = var_incond,
  var_cond = var_cond,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

# Modelo 06 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-4, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 1
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1227),
                                 c(1179, n)))
# Estimando e residuos - INICIO

(opt6 <- estimando(llike_suave, pars))

media_cond_mod6 <- esp_cond_sauve(
  data = yt,
  est = opt6,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod6 <- var_cond_sauve(
  data = yt,
  est = opt6,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod6 <- var_indcond_sauve(
  data = yt,
  est = opt6,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast = t_ast,
  t_til = t_til,
  delta_ind = delta_ind,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod6 <- (yt - media_cond_mod6)/sqrt(var_cond_mod6)
resid_pad_mod6 <- resid_pad[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod6, 
                             time = seq_along(resid_pad_mod6))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad, type = 'l')
plot(var_incond, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Analisando residuos - FIM

# FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
# FAC e FACP - FIM

# QQplot e Histograma - INICIO
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
# QQplot e Histograma - FIM

# TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod6)^2)/sum((yt - media_cond_mod6)^2))
moments::kurtosis(resid_pad_mod6)
moments::skewness(resid_pad_mod6)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond,
  var_incond = var_incond,
  var_cond = var_cond,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = yt^2), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

# Modelo 07 -----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3)
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 2
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1227),
                                 c(984, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt7 <- estimando(llike_suave, pars))

media_cond_mod7 <- esp_cond_sauve(
  data = yt,
  est = opt7,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod7 <- var_cond_sauve(
  data = yt,
  est = opt7,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod7 <- var_indcond_sauve(
  data = yt,
  est = opt7,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod7 <- (yt - media_cond_mod7)/sqrt(var_cond_mod7)
resid_pad_mod7 <-resid_pad_mod7[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod7, 
                             time = seq_along(resid_pad_mod7))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod7, type = 'l')
plot(var_incond_mod7, type = 'l', ylim = c(0, 11))

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Modelo 08 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -3)
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 2
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1227, 1313),
                                 c(984, 1179, 1312, n)))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt8 <- estimando(llike_suave, pars))

media_cond_mod8 <- esp_cond_sauve(
  data = yt,
  est = opt8,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod8 <- var_cond_sauve(
  data = yt,
  est = opt8,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod8 <- var_indcond_sauve(
  data = yt,
  est = opt8,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)


# Modelo 09 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 3
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227),
                                 c(984, 1174, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

opt9 <- estimando_model9(llike_suave_nikkei, pars)
# opt9_v2 <- estimando_model9(llike_suave_nikkei_v2, pars)

media_cond_mod9 <- opt9$media_cond
var_cond_mod9 <- opt9$dp_cond^2
var_incond_mod9 <- opt9$var_indcond

resid_pad_mod9 <- (yt - media_cond_mod9)/sqrt(var_cond_mod9)
resid_pad_mod9 <- resid_pad_mod9[-c(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod9, 
                             time = seq_along(resid_pad_mod9))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod9, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod9[-c(1:50)], type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod9)^2)/sum((yt - media_cond_mod9)^2))
moments::kurtosis(resid_pad_mod9)
moments::skewness(resid_pad_mod9)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod9,
  var_incond = var_incond_mod9,
  var_cond = var_cond_mod9,
  time = seq_along(media_cond_mod9)
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond_mod9)^2), colour = "green")

ggplot(data, aes(x = time, y = sqrt(var_incond))) +
  labs(x = "Tempo", y = "Desvio padrao incondicional") + 
  geom_line(size = 1L, colour = "red") + ylim(0, 5) + 
  xlim(900, 1300)
  geom_line(aes(x = time, y = (yt - opt9$data$deltaMedia)^2), colour = "blue") 
  
## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 10 ----------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.74),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3)
)
  
## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 3
t_ast <- 1179
t_til <- 1227
  
dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1157, 1227),
                                   c(984, 1156, 1179, n)))
# Ordens e Parametros - FIM
  
# Estimando e residuos  - INICIO
  
opt10 <- estimando_model9(llike_suave_nikkei, pars)

media_cond_mod10 <- opt10$media_cond
var_cond_mod10 <- opt10$dp_cond^2
var_incond_mod10 <- opt10$var_indcond

resid_pad_mod10 <- (yt - media_cond_mod10)/sqrt(var_cond_mod10)
resid_pad_mod10 <- resid_pad_mod10[-c(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod10, 
                             time = seq_along(resid_pad_mod10))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod10, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod10[-c(1:50)], type = 'l')

mean(resid_pad_mod10)
var(resid_pad_mod10)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod10)^2)/sum((yt - media_cond_mod10)^2))
moments::kurtosis(resid_pad_mod10)
moments::skewness(resid_pad_mod10)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod10,
  var_incond = var_incond_mod10,
  var_cond = var_cond_mod10,
  time = seq_along(media_cond_mod10)
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond_mod9)^2), colour = "green")

ggplot(data, aes(x = time, y = sqrt(var_incond))) +
  labs(x = "Tempo", y = "Desvio padrao incondicional") + 
  geom_line(size = 1L, colour = "red") + ylim(0, 5) + 
  xlim(900, 1300) +
  geom_line(aes(x = time, y = (yt - opt10$data$deltaMedia)^2), colour = "blue") 

## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 11 ---------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.74),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 4
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1157, 1175, 1227),
                                 c(984, 1156, 1174, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

opt11 <- estimando_model9(llike_suave_nikkei, pars)
opt11$data

media_cond_mod11 <- opt11$media_cond
var_cond_mod11 <- opt11$dp_cond^2
var_incond_mod11 <- opt11$var_indcond

resid_pad_mod11 <- (yt - media_cond_mod11)/sqrt(var_cond_mod11)
resid_pad_mod11 <- resid_pad_mod11[-c(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod11, 
                             time = seq_along(resid_pad_mod11))

plot(resid_pad_mod11, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod11[-c(1:50)], type = 'l')

mean(resid_pad_mod11)
var(resid_pad_mod11)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod11)^2)/sum((yt - media_cond_mod11)^2))
moments::kurtosis(resid_pad_mod11)
moments::skewness(resid_pad_mod11)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod11,
  var_incond = var_incond_mod11,
  var_cond = var_cond_mod11,
  time = seq_along(media_cond_mod11)
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond_mod11)^2), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Varianci padrao incondicional") + 
  geom_line(size = 1L, colour = "red") +
  geom_line(aes(x = time, y = (yt - opt11$data$deltaMedia)^2), colour = "blue") 

## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 12 ---------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- c(2, 3)
t_ast <- c(1157, 1179)
t_til <- c(1174, 1227)

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227),
                                 c(984, 1157, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

opt12 <- estimando_model9(llike_suave_nikkei, pars)

media_cond_mod12 <- opt12$media_cond
var_cond_mod12 <- opt12$dp_cond^2
var_incond_mod12 <- opt12$var_indcond

resid_pad_mod12 <- (yt - media_cond_mod12)/sqrt(var_cond_mod12)
resid_pad_mod12 <- resid_pad_mod12[-c(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod12, 
                             time = seq_along(resid_pad_mod12))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod12, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod12[-c(1:50)], type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod12)^2)/sum((yt - media_cond_mod12)^2))
moments::kurtosis(resid_pad_mod12)
moments::skewness(resid_pad_mod12)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod12,
  var_incond = var_incond_mod12,
  var_cond = var_cond_mod12,
  time = seq_along(media_cond_mod12)
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond_mod12)^2), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia padrao incondicional") + 
  geom_line(size = 1L, colour = "red") +
  geom_line(aes(x = time, y = (yt - opt12$data$deltaMedia)^2), colour = "blue") 

## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 12 - ajuste fino ---------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(c(.05, .05)),
  psi3 = log(.85),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- c(2, 3)
t_ast <- c(1157, 1179)
t_til <- c(1174, 1227)

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227),
                                 c(984, 1157, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

opt12_1 <- estimando_model9(llike_suave_nikkei, pars)
opt12_1$data

media_cond_mod12_1 <- opt12_1$media_cond
var_cond_mod12_1 <- opt12_1$dp_cond^2
var_incond_mod12_1 <- opt12_1$var_indcond

resid_pad_mod12_1 <- (yt - media_cond_mod12_1)/sqrt(var_cond_mod12_1)
resid_pad_mod12_1 <- resid_pad_mod12_1[-c(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod12_1, 
                             time = seq_along(resid_pad_mod12_1))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod12_1, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod12_1[-c(1:50)], type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod12_1)^2)/sum((yt - media_cond_mod12_1)^2))
moments::kurtosis(resid_pad_mod12_1)
moments::skewness(resid_pad_mod12_1)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod12_1,
  var_incond = var_incond_mod12_1,
  var_cond = var_cond_mod12_1,
  time = seq_along(media_cond_mod12_1)
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond_mod12_1)^2), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Desvio padrao incondicional") + 
  geom_line(size = 1L, colour = "red") +
  geom_line(aes(x = time, y = (yt - opt12_1$data$deltaMedia)^2), colour = "blue") 

## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 13 ---------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi1 = log(.5),
  psi2 = log(c(.05, .05)),
  psi3 = log(.84),
  ar = .5,
  deltaMedia = 0
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)

n <- length(yt)
dummy1 <- as.matrix(dummy_step(n, 1, "Media"))

# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt13 <- estimando(llike_garch, pars))

media_cond_mod13 <- esp_cond_garch(
  data = yt,
  est = opt13,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

var_cond_mod13 <- var_cond_garch(
  data = yt,
  est = opt13,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  Varyt = Varyt,
  kmed = kmed,
  n = n
)

resid_pad_mod13 <- (yt - media_cond_mod13)/sqrt(var_cond_mod13)
resid_pad_mod13 <- resid_pad_mod13[-(1:50)]

plot(resid_pad_mod13, type = 'l', ylim = c(-6, 6))

mean(resid_pad_mod13)
var(resid_pad_mod13)

# Modelo 14 ---------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(c(.05, .05)),
  psi3 = log(.85),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -1)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- c(2, 3)
t_ast <- c(1157, 1179)
t_til <- c(1174, 1227)

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227),
                                 c(984, 1157, 1179, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

(opt14 <- estimando(llike_suave, pars))

media_cond_mod14 <- esp_cond_sauve(
  data = yt,
  est = opt14,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod14 <- var_cond_sauve(
  data = yt,
  est = opt14,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod14 <- var_indcond_sauve(
  data = yt,
  est = opt14,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod14 <- (yt - media_cond_mod14)/sqrt(var_cond_mod14)
resid_pad_mod14 <- resid_pad_mod14[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod14, 
                             time = seq_along(resid_pad_mod14))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod14, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod14, type = 'l')


# Modelo 15 - modelo estranho ---------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(c(.1, .05)),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -1, -1)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- 2
t_ast <- 1157
t_til <- 1174

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175),
                                 c(984, 1157, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

(opt15 <- estimando(llike_suave, pars))

media_cond_mod15 <- esp_cond_sauve(
  data = yt,
  est = opt15,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod15 <- var_cond_sauve(
  data = yt,
  est = opt15,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod15 <- var_indcond_sauve(
  data = yt,
  est = opt15,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod15 <- (yt - media_cond_mod15)/sqrt(var_cond_mod15)
resid_pad_mod15 <- resid_pad_mod15[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod15, 
                             time = seq_along(resid_pad_mod15))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod15, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod15, type = 'l')


# Modelo 16 ---------------------------------------------------------------

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(c(.05, .05)),
  psi3 = log(.85),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-3, -3, -3, -3, -3)
)

## Definindo parametros e dummies
alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- c(2, 3)
t_ast <- c(1157, 1179)
t_til <- c(1174, 1227)

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227, 1312),
                                 c(984, 1157, 1179, 1311, n)))
# Ordens e Parametros - FIM

# Estimando e residuos  - INICIO

(opt16 <- estimando(llike_suave, pars))

media_cond_mod16 <- esp_cond_sauve(
  data = yt,
  est = opt16,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod16 <- var_cond_sauve(
  data = yt,
  est = opt16,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod16 <- var_indcond_sauve(
  data = yt,
  est = opt16,
  dummy1 = dummy1,
  dummy2 = dummy2,
  delta_ind = delta_ind,
  t_ast = t_ast,
  t_til = t_til,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod16 <- (yt - media_cond_mod16)/sqrt(var_cond_mod16)
resid_pad_mod16 <- resid_pad_mod16[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod16, 
                             time = seq_along(resid_pad_mod16))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod16, type = 'l', ylim = c(-6, 6))
plot(var_incond_mod16, type = 'l')

mean(resid_pad_mod16)
var(resid_pad_mod16)

## Analisando residuos 

## FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
## FAC e FACP - FIM

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

## TH - INICIO

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod12_1)^2)/sum((yt - media_cond_mod12_1)^2))
moments::kurtosis(resid_pad_mod12_1)
moments::skewness(resid_pad_mod12_1)

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod16,
  var_incond = var_incond_mod16,
  var_cond = var_cond_mod16,
  time = seq_along(media_cond_mod16)
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = (yt - media_cond_mod16)^2), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Desvio padrao incondicional") + 
  geom_line(size = 1L, colour = "red") +
  geom_line(aes(x = time, y = (yt - opt16$deltaMedia)^2), colour = "blue") 

## Graficos de linha para esp_cond e var_cond - FIM

# Resultados --------------------------------------------------------------

medidas <- function(modelo, nome){
  modelo %>% select(llike, AIC, BIC) %>% mutate(Modelo = nome)
  
}

resultado <- rbind(medidas(opt1, "opt1"),
                   medidas(opt2, "opt2"),
                   medidas(opt3, "opt3"),
                   medidas(opt4, "opt4"),
                   medidas(opt5, "opt5"),
                   medidas(opt6, "opt6"),
                   medidas(opt7, "opt7"),
                   medidas(opt8, "opt8"),
                   medidas(opt9$data, "opt9"),
                   medidas(opt10$data, "opt10"),
                   medidas(opt11$data, "opt11"),
                   medidas(opt12$data, "opt12"),
                   medidas(opt12_1$data, "opt12_1"),
                   medidas(opt13, "opt13"),
                   medidas(opt14, "opt14"),
                   medidas(opt15, "opt15"),
                   medidas(opt16, "opt16")
                   )
                   
resultado %>% arrange(AIC)
resultado %>% arrange(BIC)


rbind(medidas(opt1, "opt1"),
      medidas(opt2, "opt2"),
      medidas(opt3, "opt3"),
      medidas(opt9$data, "opt9"),
      medidas(opt10$data, "opt10"),
      medidas(opt11$data, "opt11"),
      medidas(opt12$data, "opt12"),
      medidas(opt12_1$data, "opt12_1")
)

## Teste LR
teste_lr(opt1, opt2)
teste_lr(opt1, opt3)
teste_lr(opt3, opt2)
teste_lr(opt1, opt4)
teste_lr(opt3, opt4)
teste_lr(opt1, opt5)
teste_lr(opt3, opt5)
teste_lr(opt1, opt6)
teste_lr(opt5, opt6)
teste_lr(opt3, opt6)
teste_lr(opt1, opt7)
teste_lr(opt7, opt6)
teste_lr(opt3, opt7)
teste_lr(opt1, opt7)

teste_lr(opt1, opt9$data)
teste_lr(opt3, opt9$data)
teste_lr(opt9$data, opt2)
teste_lr(opt11$data, opt9$data)
teste_lr(opt11$data, opt10$data)
# teste_lr(opt12_1$data, opt12$data) NÃO USAR !!!!!!!!!
teste_lr(opt12_1$data, opt13)
teste_lr(opt14, opt15)
teste_lr(opt14, opt12_1$data)
teste_lr(opt16, opt14)

## Poder preditivo
poder_pred(yt, media_cond_mod1, var_cond_mod1)$rmse
poder_pred(yt, media_cond_mod2, var_cond_mod2)$rmse
poder_pred(yt, media_cond_mod3, var_cond_mod3)$rmse
poder_pred(yt, media_cond_mod4, var_cond_mod4)$rmse
poder_pred(yt, media_cond_mod5, var_cond_mod5)$rmse
poder_pred(yt, media_cond_mod6, var_cond_mod6)$rmse
poder_pred(yt, media_cond_mod7, var_cond_mod7)$rmse
poder_pred(yt, media_cond_mod8, var_cond_mod8)$rmse
poder_pred(yt, media_cond_mod9, var_cond_mod9)$rmse
poder_pred(yt, media_cond_mod10, var_cond_mod10)$rmse
poder_pred(yt, media_cond_mod11, var_cond_mod11)$rmse
poder_pred(yt, media_cond_mod12, var_cond_mod12)$rmse
poder_pred(yt, media_cond_mod12_1, var_cond_mod12_1)$rmse
poder_pred(yt, media_cond_mod13, var_cond_mod13)$rmse
poder_pred(yt, media_cond_mod14, var_cond_mod14)$rmse
poder_pred(yt, media_cond_mod15, var_cond_mod15)$rmse
poder_pred(yt, media_cond_mod16, var_cond_mod16)$rmse

cor(var_cond_mod1[-(1:50)], ((yt - media_cond_mod1)^2)[-(1:50)])^2
cor(var_cond_mod2[-(1:50)], ((yt - media_cond_mod2)^2)[-(1:50)])^2
cor(var_cond_mod3[-(1:50)], ((yt - media_cond_mod3)^2)[-(1:50)])^2
cor(var_cond_mod9[-(1:50)], ((yt - media_cond_mod9)^2)[-(1:50)])^2
cor(var_cond_mod10[-(1:50)], ((yt - media_cond_mod10)^2)[-(1:50)])^2
cor(var_cond_mod11[-(1:50)], ((yt - media_cond_mod11)^2)[-(1:50)])^2
cor(var_cond_mod12[-(1:50)], ((yt - media_cond_mod12)^2)[-(1:50)])^2
cor(var_cond_mod12_1[-(1:50)], ((yt - media_cond_mod12_1)^2)[-(1:50)])^2
cor(var_cond_mod13[-(1:50)], ((yt - media_cond_mod13)^2)[-(1:50)])^2
cor(var_cond_mod14[-(1:50)], ((yt - media_cond_mod14)^2)[-(1:50)])^2
cor(var_cond_mod16[-(1:50)], ((yt - media_cond_mod16)^2)[-(1:50)])^2

((yt - opt9$data$deltaMedia)[51:900])^2 %>% mean()
var_incond_mod9[51]

abs((yt - opt9$data$deltaMedia)[51:900]) %>% mean()
var_incond_mod9[51] %>% sqrt()


((yt - opt3$deltaMedia)[51:900])^2 %>% mean()
var_incond_mod3[51]

abs((yt - opt3$deltaMedia)[51:900]) %>% mean()
var_incond_mod3[51] %>% sqrt()