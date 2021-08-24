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
crises <- as.Date(c("2007-07-01", "2008-11-28"))

NYSE <- NYSE %>% fortify.zoo %>% as_tibble 

ggplot(NYSE, aes(x = Index, y = 100 * nyse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "NYSE") +
  theme_minimal()

NYSE <- NYSE %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), nyse = 100 * nyse) %>%
  select(Index, id, everything())

yt <- NYSE$cac %>% as.vector()
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