
source("src/util.R")
source("src/graficos.R")

library(zoo)
library(quantmod)
library(ggplot2)
library(dplyr)
library(imputeTS)

# CAC ---------------------------------------------------------------------

load("dados/CAC.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28"))

CAC <- CAC %>% fortify.zoo %>% as_tibble

CAC <- CAC %>%
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), cac = 100*cac) %>%
  select(Index, id, everything())

yt <- CAC$cac %>% as.vector()

ggplot(CAC, aes(x = Index, y = cac)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "CAC com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(crises[1], as.Date("2008-09-15"),
                            as.Date("2008-12-18"), 
                            as.Date("2009-06-19")), 
             colour = c('red', "yellow", 'green', 'green'), size = 1.5,
             # Red=Crise, Yellow=LB
             linetype = "dashed") +
  geom_rect(data=CAC, 
            mapping=aes(xmin= as.Date("2008-09-22"), 
                        xmax=as.Date('2009-12-31'), ymin=min(yt), ymax=max(yt)), 
            color="grey", alpha=0.003) + tema

ggsave(r"{graficos\CAC\modelo_cac_int.png}", width = 20, height = 10)


# DAX ---------------------------------------------------------------------

load("dados/DAX.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28")) # Nao usar segunda data

DAX <- DAX %>% fortify.zoo %>% as_tibble 

DAX <- DAX %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), dax = 100*dax) %>% 
  select(Index, id, everything())

yt <- DAX$dax %>% as.vector()

ggplot(DAX, aes(x = Index, y = dax)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "DAX com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(crises[1], 
                            as.Date(c("2008-09-15",
                                      "2008-12-11", 
                                      "2009-05-26"))), 
             colour = c('red', "yellow", 'green', 'green'), size = 1.5,
             linetype = "dashed") +
  geom_rect(data=DAX, 
            mapping=aes(xmin= as.Date("2008-09-20"), 
                        xmax=as.Date('2009-12-31'), ymin=min(yt), ymax=max(yt)), 
            color="grey", alpha=0.003) + tema
ggsave(r"{graficos\DAX\modelo_dax_int.png}", width = 20, height = 10)

# NIKKEI ------------------------------------------------------------------

load("dados/NIKKEI.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28"))

NIKKEI <- NIKKEI %>% 
  fortify.zoo() %>% 
  as_tibble()

NIKKEI <- NIKKEI %>% 
  dplyr::filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), nikkei = 100*nikkei) %>% 
  select(Index, id, everything()) 

yt <- NIKKEI$nikkei %>% as.vector()

ggplot(NIKKEI, aes(x = Index, y = nikkei)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "NIKKEI com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(as.Date(c("2008-01-01",
                                      "2008-09-15", 
                                      "2008-10-10",
                                      "2009-01-01"))), 
  colour = c('green', "yellow", 'green', 'green'), size = 1.5,
  linetype = "dashed") +
  geom_rect(data=NIKKEI, 
            mapping=aes(xmin= as.Date("2008-10-20"), 
                        xmax=as.Date('2009-12-31'), ymin=min(yt), ymax=max(yt)), 
            color="grey", alpha=0.003) + tema
ggsave(r"{graficos\NIKKEI\modelo_nikkei_int.png}", width = 20, height = 10)

# FTSE --------------------------------------------------------------------

load("dados/FTSE.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28")) # Nao usar segunda data

FTSE <- FTSE %>% 
  fortify.zoo %>% 
  as_tibble %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), ftse = 100 * ftse) %>%
  select(Index, id, everything())

yt <- FTSE$ftse %>% as.vector()

ggplot(FTSE, aes(x = Index, y = ftse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "FTSE com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(crises[1], 
                            as.Date(c("2008-09-15", 
                                      "2009-06-18"))), 
             colour = c('red', 'yellow', 'green'), 
             size = 1.5, linetype = "dashed") +
  geom_rect(data=FTSE, 
            mapping=aes(xmin= as.Date("2008-09-19"), 
                        xmax=as.Date('2009-01-19'), ymin=min(yt), ymax=max(yt)), 
            color="grey", alpha=0.003)  + tema
ggsave(r"{graficos\FTSE\modelo_ftse_int.png}", width = 20, height = 10)


# NYSE --------------------------------------------------------------------

load("dados/NYSE.RData")
BegSample <- '2004-01-01'
EndSample <- '2009-12-31'
crises <- as.Date(c("2007-07-01", "2008-11-28")) # Nao usar segunda data

NYSE <- NYSE %>% fortify.zoo %>% as_tibble 

NYSE <- NYSE %>% 
  filter(Index >= as.Date(BegSample), Index <= as.Date(EndSample)) %>% 
  mutate(id = row_number(), nyse = 100 * nyse) %>%
  select(Index, id, everything())

yt <- NYSE$nyse %>% as.vector()
Varyt <- var(yt[1:50])

ggplot(NYSE, aes(x = Index, y = nyse)) +
  geom_line(size = 1L, colour = "#112446") +
  labs(x = "Tempo", y = "Retorno", title = "FTSE com as Restrições") +
  theme_minimal() + 
  geom_vline(xintercept = c(crises[1], 
                            as.Date(c("2008-09-15",
                                      "2009-06-18"))), 
             colour = c('red', 'yellow', 'green'), 
             size = 1.5, linetype = "dashed") +
  geom_rect(data=NYSE, 
            mapping=aes(xmin= as.Date("2008-09-19"), 
                        xmax=as.Date('2008-10-08'), ymin=min(yt), ymax=max(yt)), 
            color="grey", alpha=0.003) + tema
ggsave(r"{graficos\NYSE\modelo_nyse_int.png}", width = 20, height = 10)
  



