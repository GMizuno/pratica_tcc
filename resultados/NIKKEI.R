source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

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
  as_tibble() %>% 
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

p2 <- ggplot(NIKKEI, aes(x = Index, y = abs(nikkei))) +
  geom_line(size = 1L, colour = "#112446") + 
  labs(x = "Tempo", y = "Retorno", title = "NIKKEI") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2008-10-10", "2010-07-31")), 
             colour = 'blue', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2008-10-20", "2010-07-31")), 
             colour = 'grey', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2008-01-01")), 
             colour = 'red', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2009-05-13")), 
             colour = 'pink', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2009-01-01")), 
             colour = 'yellow', size = 1.5, linetype = "dashed") 

p3 <- ggplot(NIKKEI, aes(x = Index, y = nikkei)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "NIKKEI com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2008-10-10", "2010-07-31")), 
             colour = 'blue', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2008-10-20", "2010-07-31")), 
             colour = 'grey', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2008-01-01")), 
            colour = 'red', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2009-05-13")), 
             colour = 'pink', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = as.Date(c("2009-01-01")), 
             colour = 'yellow', size = 1.5, linetype = "dashed") 

p3
gridExtra::grid.arrange(p1, p3, ncol = 1)

acf(yt, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(yt, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1))

# Modelo 1 ----------------------------------------------------------------

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

media_cond <- esp_cond_sauve(
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

var_cond <- var_cond_sauve(
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

var_incond <- var_indcond_sauve(
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

resid_pad <- (yt - media_cond)/sqrt(var_cond)
resid_pad <- resid_pad[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad, 
                             time = seq_along(resid_pad))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad, type = 'l')
plot(var_incond, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Estimando e residuos - FIM

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

(dw <- sum(diff(yt - media_cond)^2)/sum((yt - media_cond)^2))

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

# Modelo 2 ----------------------------------------------------------------
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

(opt2 <- estimando(llike_garch, pars))

# Teste LR - INICIO
teste_lr(opt1, opt2)
# Teste LR - FIM

# Residuos - INICIO

media_cond <- esp_cond_garch(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

var_cond <- var_cond_garch(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  Varyt = Varyt,
  kmed = kmed,
  n = n
)


resid_pad <- (yt - media_cond)/sqrt(var_cond)
resid_pad <- resid_pad[-(1:50)]

plot(resid_pad, type = 'l')

mean(resid_pad)
var(resid_pad)

(var_incond <- opt2$omega/(1 - opt2$alpha - opt2$beta)*(1/(1 - opt2$ar^2)) * var(resid_pad))
var(yt)
var(yt[-(1:50)])

resid_pad_data <- data.frame(resid_pad = resid_pad, time = seq_along(resid_pad))

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

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box')
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box')

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond)^2)/sum((yt - media_cond)^2))

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

# Modelo 3 ----------------------------------------------------------------

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

media_cond <- esp_cond_sauve(
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

var_cond <- var_cond_sauve(
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

var_incond <- var_indcond_sauve(
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

resid_pad <- (yt - media_cond)/sqrt(var_cond)
resid_pad <-resid_pad[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad, 
                             time = seq_along(resid_pad))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad, type = 'l')
plot(var_incond, type = 'l')

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

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box')
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box')

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond)^2)/sum((yt - media_cond)^2))

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

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "green")

ggplot(data, aes(x = time, y = var_incond)) +
  labs(x = "Tempo", y = "Variancia incondicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
## Graficos de linha para esp_cond e var_cond - FIM

# Modelo 4 ----------------------------------------------------------------
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
t_ast <- 1179
t_til <- 1227
delta_ind <- 2

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1175, 1227, 1313),
                                 c(1174, 1179, 1312, n)))
# Estimando e residuos - INICIO

(opt4 <- estimando(llike_suave, pars))

teste_lr(opt1, opt4)

media_cond <- esp_cond_sauve(
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

var_cond <- var_cond_sauve(
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

var_incond <- var_indcond_sauve(
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
  kvar = kvar
)

resid_pad <- (yt - media_cond)/sqrt(var_cond)
resid_pad <-resid_pad[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad, 
                             time = seq_along(resid_pad))
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

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box')
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box')

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond)^2)/sum((yt - media_cond)^2))

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



# Resultados --------------------------------------------------------------

teste <- function(modelo, nome){
  modelo %>% select(llike, AIC, BIC) %>% mutate(Modelo = nome)
  
}

resultado <- rbind(teste(opt1, "opt1"),
                   teste(opt2, "opt2"),
                   teste(opt3, "opt3"),
                   teste(opt4, "opt4"))

resultado %>% arrange(AIC)
resultado %>% arrange(BIC)


