source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)


load("dados/DAX.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'

DAX <- DAX %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number()) %>% select(Index, id, everything())

yt <- DAX$dax %>% as.vector()
Varyt <- var(yt[1:50])

p1 <- ggplot(DAX, aes(x = Index, y = DAX)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Preço", title = "DAX") +
  theme_minimal()

p2 <- ggplot(DAX, aes(x = Index, y = dax)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "DAX") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2007-07-01", "2008-11-28")), 
             colour = 'red', size = 1.5, linetype = "dashed")

p3 <- ggplot(DAX, aes(x = Index, y = dax)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "DAX com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = as.Date(c("2008-09-20", "2011-03-31")), 
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