source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)

# Carregando dados e graficos ---------------------------------------------
load("dados/NYSE.RData")
BegSample <- '2004-01-01'
EndSample <- '2010-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28")) # Nao usar segunda data

NYSE <- NYSE %>% fortify.zoo %>% as_tibble 

ggplot(NYSE, aes(x = Index, y = 100 * nyse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "NYSE") +
  theme_minimal()

NYSE <- NYSE %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), nyse = 100 * nyse) %>%
  select(Index, id, everything())

yt <- NYSE$nyse %>% as.vector()
Varyt <- var(yt[1:50])

# Graficos ----------------------------------------------------------------

p1 <- ggplot(NYSE, aes(x = Index, y = NYSE)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "NYSE") +
  theme_minimal()

p2 <- ggplot(NYSE, aes(x = Index, y = nyse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "NYSE") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2007-07-01", "2008-11-28")), 
             colour = 'red', size = 1.5, linetype = "dashed")

p3 <- ggplot(NYSE, aes(x = Index, y = nyse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "NYSE com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2008-10-20", "2010-07-31")), 
             colour = 'blue', size = 1.5, linetype = "dashed")
# p2
gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

ggplot(NYSE, aes(x = Index, y = 100 * nyse)) +
  geom_line(size = 1L, colour = "#112446") + 
  labs(x = "Tempo", y = "Retorno", title = "NYSE") +
  theme_minimal() 
ggsave(r"{graficos\USA\usa_serie.png}", width = 6, height = 3.5)

acf(yt, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\USA\usa_fac_serie.png}", width = 6, height = 3.5)
pacf(yt, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\USA\usa_facp_serie.png}", width = 6, height = 3.5)

acf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\USA\usa_fac_quad.png}", width = 6, height = 3.5)
pacf(yt^2, plot = F) %>% autoplot() + ylim(c(-1,1)) + ggtitle("") +
  theme_minimal() 
ggsave(r"{graficos\USA\usa_facp_quad.png}", width = 6, height = 3.5)

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
delta_ind <- 3
t_ast <- 1179
t_til <- 1227

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 985, 1175, 1227, 1313),
                                 c(984, 1174, 1179, 1312, n)))
# Ordens e Parametros - FIM

# Estimando e residuos - INICIO

(opt1 <- estimando(llike_suave, pars))
# Resultados --------------------------------------------------------------