source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)

load("dados/CAC.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'

CAC <- CAC %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number()) %>% select(Index, id, everything())

yt <- CAC$cac %>% as.vector()
Varyt <- var(yt[1:50])

p1 <- ggplot(CAC, aes(x = Index, y = CAC)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "CAC") +
  theme_minimal()

p2 <- ggplot(CAC, aes(x = Index, y = cac)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "CAC") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2007-07-01", "2008-11-28")), 
             colour = 'red', size = 1.5, linetype = "dashed")
# p2
# gridExtra::grid.arrange(p1, p2, ncol = 1)

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-13, -12, -11)
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- 1 
kvar <- 3
n <- length(yt) # Tamanho da serie

# Ordens e Parametros - INICIO

# Criando Dummies - INICIO
dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 772, 1262),
                       c(771, 1261, n)))
# Criando Dummies - FIM

(opt <- estimando(llike_model_garch, pars))

# Residuos - INICIO

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
# Residuos - FIM

# Analise do residuos - INICIO
ggplot(resid_pad_data, aes(sample = resid_pad)) + 
  stat_qq() + 
  geom_abline(slope = 1, intercept = 0) + 
  tema + ylim(-6,6) + 
  scale_x_continuous(limits = c(-6, 6),  breaks = c(-6, -4, -2, 0, 2, 4, 6))

acf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad, plot = F) %>% autoplot() + ylim(c(-1,1))

acf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))
pacf(resid_pad_data$resid_pad^2, plot = F) %>% autoplot() + ylim(c(-1,1))

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

## TH - FIM

## Graficos de linha para esp_cond e var_cond - INICIO
data <- data.frame(yt = yt, one_step_predict = media_cond, 
                   var_cond = var_cond, time = 1:n)

ggplot(data, aes(x = time, y = yt)) +
  geom_line(size = 1L, colour = "#0c4c8a") +
  theme(axis.title.y = element_text(angle = 0)) +
  labs(x = 'Tempo') +
  geom_line(aes(y = one_step_predict), size = 1L, colour = "red")

ggplot(data, aes(x = time, y = var_cond)) +
  labs(y = "Tempo", x = "Variancia Condicional") + 
  geom_line(size = 1L, colour = "#0c4c8a")
## Graficos de linha para esp_cond e var_cond - FIM
# Analise do residuos - FIM
