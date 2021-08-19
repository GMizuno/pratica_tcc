source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)
library(quantmod)
library(imputeTS)
library(ggplot2)
library(dplyr)

load("dados/NIKKEI.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'

NIKKEI <- NIKKEI %>% 
  fortify.zoo() %>% 
  as_tibble() %>% 
  dplyr::filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), nikkei = 100*nikkei) %>% 
  select(Index, id, everything()) 

yt <- NIKKEI$nikkei %>% as.vector()
Varyt <- var(yt[1:50])
pars <- list(
  psi2 = log(.15),
  psi3 = log(.84),
  ar = .2,
  deltaMedia = 0.0,
  deltaVar = c(-4, -1, -5, -2)
)

alpha_order <- length(pars$psi2)
beta_order <- length(pars$psi3)
kmed <- length(pars$deltaMedia)
kvar <- length(pars$deltaVar)
n <- length(yt) # Tamanho da serie
delta_ind <- c(2, 3)
t_ast <- c(900, 1200)
t_til <- c(1000, 1400) 

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 801, 1000, 1401),
                                 c(800, 900, 1200, n)))

pars <- unlist(pars, use.names = FALSE)

llike_suave(pars)
