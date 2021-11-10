source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)

# Carregando dados --------------------------------------------------------

load("dados/FTSE.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28")) # Nao usar segunda data


ggplot(FTSE, aes(x = Index, y = 100 * ftse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "FTSE")

FTSE <- FTSE %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), ftse = 100 * ftse) %>%
  select(Index, id, everything())

yt <- FTSE$ftse %>% as.vector()
Varyt <- var(yt[1:50])

# Graficos ----------------------------------------------------------------

p1 <- ggplot(FTSE, aes(x = Index, y = FTSE)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "FTSE") +
  theme_minimal()

p2 <- ggplot(FTSE, aes(x = Index, y = ftse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "FTSE") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2007-07-01", "2008-11-28")), 
             colour = 'red', size = 1.5, linetype = "dashed")

p3 <- ggplot(FTSE, aes(x = Index, y = ftse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "FTSE com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(crises, as.Date(c("2008-09-19", "2009-01-19"))), 
             colour = 'red', size = 1.5, linetype = "dashed")

gridExtra::grid.arrange(p1, p3, ncol = 1)

ggplot(FTSE, aes(x = Index, y = 100 * ftse)) +
  geom_line(size = 1L, colour = "#112446") + 
  labs(x = "Tempo", y = "Retorno", title = "FTSE") +
  theme_minimal() 
ggsave(r"{graficos\UK\uk_serie.png}", width = 6, height = 3.5)

acf(yt, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\UK\uk_fac_serie.png}", width = 6, height = 3.5)
pacf(yt, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\UK\uk_facp_serie.png}", width = 6, height = 3.5)

acf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal()  
ggsave(r"{graficos\UK\uk_fac_quad.png}", width = 6, height = 3.5)
pacf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\UK\uk_facp_quad.png}", width = 6, height = 3.5)
# Modelo 00 AR(1)-GARCH(1,1) ----------------------------------------------

pars <- list(
  psi1 = log(1),
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
n <- length(yt) # Tamanho da serie

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO
(opt0 <- estimando(llike_garch, pars))

media_cond_mod0 <- esp_cond_garch(
  data = yt,
  est = opt0,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

var_cond_mod0 <- var_cond_garch(
  data = yt,
  est = opt0,
  dummy1 = dummy1,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

resid_pad_mod0 <- (yt - media_cond_mod0)/sqrt(var_cond_mod0)
resid_pad_mod0 <- resid_pad_mod0[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod0, 
                             time = seq_along(resid_pad_mod0))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod0, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Estimando e residuos - FIM

# FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
# FAC e FACP - FIM

# Poder preditivo - INICIO
poder_pred(yt, media_cond_mod0, var_cond_mod0)$rmse
cor(var_cond_mod0[-(1:50)], ((yt - media_cond_mod0)^2)[-(1:50)])^2
# Poder preditivo - FIM

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

(dw <- sum(diff(yt - media_cond_mod0)^2)/sum((yt - media_cond_mod0)^2))
moments::kurtosis(resid_pad_mod0)
moments::skewness(resid_pad_mod0)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod0,
  var_cond = var_cond_mod0,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(x = "Tempo", y = "Desvio Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

# Modelo 00 AR(1)-GARCH(1,1) ----------------------------------------------

pars <- list(
  psi1 = log(1),
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
n <- length(yt) # Tamanho da serie

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO
(opt0 <- estimando(llike_garch, pars))

media_cond_mod0 <- esp_cond_garch(
  data = yt,
  est = opt0,
  dummy1 = dummy1,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

var_cond_mod0 <- var_cond_garch(
  data = yt,
  est = opt0,
  dummy1 = dummy1,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  n = n
)

resid_pad_mod0 <- (yt - media_cond_mod0)/sqrt(var_cond_mod0)
resid_pad_mod0 <- resid_pad_mod0[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod0, 
                             time = seq_along(resid_pad_mod0))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod0, type = 'l')

mean(resid_pad_data$resid_pad)
var(resid_pad_data$resid_pad)

# Estimando e residuos - FIM

# FAC e FACP - INICIO
acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
# FAC e FACP - FIM

# Poder preditivo - INICIO
poder_pred(yt, media_cond_mod0, var_cond_mod0)$rmse
cor(var_cond_mod0[-(1:50)], ((yt - media_cond_mod0)^2)[-(1:50)])^2
# Poder preditivo - FIM

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

(dw <- sum(diff(yt - media_cond_mod0)^2)/sum((yt - media_cond_mod0)^2))
moments::kurtosis(resid_pad_mod0)
moments::skewness(resid_pad_mod0)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod0,
  var_cond = var_cond_mod0,
  time = 1:n
)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') 

ggplot(data, aes(x = time, y = sqrt(var_cond))) +
  labs(x = "Tempo", y = "Desvio Condicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

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

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 758, 1190, 1240, 1294),
                                 c(757, 1189, 1239, 1293, n)))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt1 <- estimando(llike_model_garch, pars))

media_cond_mod1 <- esp_cond_model(
  data = yt,
  est = opt1,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod1 <- var_cond_model(
  data = yt,
  est = opt1,
  dummy1 = dummy1,
  dummy2 = dummy2,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod1 <- var_indcond(
  data = yt,
  est = opt1,
  dummy1 = dummy1,
  dummy2 = dummy2,
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

plot(resid_pad_mod1, type = 'l')
plot(var_incond_mod1, type = 'l')

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
  one_step_predict = media_cond_mod1,
  var_incond = var_incond_mod1,
  var_cond = var_cond_mod1,
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
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")

ggplot(data, aes(x = time, y = sqrt(var_incond))) +
  labs(x = "Tempo", y = "Variancia Incondicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
# Graficos de linha para esp_cond e var_cond - 


# Modelo 2 ----------------------------------------------------------------

ggplot(FTSE, aes(x = Index, y = ftse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "FTSE com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(crises[1], as.Date(c("2008-09-19", "2009-01-19"))), 
             colour = 'red', size = 1.5, linetype = "dashed")

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

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 758, 1190, 1294),
                                 c(757, 1189, 1293, n)))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt2 <- estimando(llike_model_garch, pars))

media_cond_mod2 <- esp_cond_model(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_cond_mod2 <- var_cond_model(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  dummy2 = dummy2,
  Varyt = Varyt,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar,
  n = n
)

var_incond_mod2 <- var_indcond(
  data = yt,
  est = opt2,
  dummy1 = dummy1,
  dummy2 = dummy2,
  alpha_order = alpha_order,
  beta_order = beta_order,
  kmed = kmed,
  kvar = kvar
)

resid_pad_mod2 <- (yt - media_cond_mod2)/sqrt(var_cond_mod2)
resid_pad_mod2 <- resid_pad_mod2[-(1:50)]

resid_pad_data <- data.frame(resid_pad = resid_pad_mod2, 
                             time = seq_along(resid_pad_mod2))
resid_pad_data <- resid_pad_data[-1, ]

plot(resid_pad_mod2, type = 'l')
plot(var_incond_mod2, type = 'l')

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

Box.test(resid_pad_data$resid_pad, type = 'Ljung-Box', lag = 30)
Box.test(resid_pad_data$resid_pad^2, type = 'Ljung-Box', lag = 30)

shapiro.test(resid_pad_data$resid_pad)
tseries::jarque.bera.test(resid_pad_data$resid_pad)
nortest::ad.test(resid_pad_data$resid_pad)

(dw <- sum(diff(yt - media_cond_mod2)^2)/sum((yt - media_cond_mod2)^2))
moments::kurtosis(resid_pad_mod2)
moments::skewness(resid_pad_mod2)

# TH - FIM

# Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(
  yt = yt,
  one_step_predict = media_cond_mod2,
  var_incond = var_incond_mod2,
  var_cond = var_cond_mod2,
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
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")

ggplot(data, aes(x = time, y = sqrt(var_incond))) +
  labs(x = "Tempo", y = "Variancia Incondicional") + 
  geom_line(size = 1L, colour = "red") + 
  geom_line(aes(x = time, y = abs(yt)), colour = "blue")
# Graficos de linha para esp_cond e var_cond - FIM

# Resultados --------------------------------------------------------------

teste <- function(modelo, nome){
  modelo %>% select(llike, AIC, BIC) %>% mutate(Modelo = nome)
  
}

resultado <- rbind(teste(opt1, "opt1"),
                   teste(opt2, "opt2"))

resultado %>% arrange(AIC)
resultado %>% arrange(BIC)

## Teste LR
teste_lr(opt1, opt2)

