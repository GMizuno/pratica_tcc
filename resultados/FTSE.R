source("src/util.R")
source("src/Analise de reisduos.R")
source("src/modelo_est.R")

library(zoo)

load("dados/FTSE.RData")

yt <- FTSE$ftse %>% as.vector()
Varyt <- var(yt[1:50])

# Criando Dummies - INICIO
# Criando Dummies - FIM