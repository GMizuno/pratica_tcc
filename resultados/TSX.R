source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)


# Carregando dados --------------------------------------------------------

load("dados/TSX.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'

TSX <- TSX %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number()) %>% select(Index, id, everything())

yt <- TSX$tsx %>% as.vector()
Varyt <- var(yt[1:50])

# Graficos ----------------------------------------------------------------

p1 <- ggplot(TSX, aes(x = Index, y = TSX)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "TSX") +
  theme_minimal()

p2 <- ggplot(TSX, aes(x = Index, y = tsx)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "TSX") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2007-07-01", "2008-11-28")), 
             colour = 'red', size = 1.5, linetype = "dashed")

p3 <- ggplot(TSX, aes(x = Index, y = tsx)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "TSXX com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2008-09-19", "2008-10-08")), 
             colour = 'blue', size = 1.5, linetype = "dashed")
# p2
gridExtra::grid.arrange(p1, p2, p3, ncol = 1)

# Ordens e Parametros - INICIO
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-13, -12, -11)
)
# Definindo parametros e dummies ------------------------------------------
# Estimando e residuos ----------------------------------------------------
# Analisando residuos -----------------------------------------------------