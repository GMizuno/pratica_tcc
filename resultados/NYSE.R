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
# Definindo parametros e dummies ------------------------------------------
# Estimando e residuos ----------------------------------------------------
# Analisando residuos -----------------------------------------------------