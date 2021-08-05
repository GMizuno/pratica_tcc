source("src/modelo_sim.R")
source("src/util.R")
source("src/modelo_est.R")
source("src/Analise de reisduos.R")

library(ggplot2)

# Exemplo 1 ---------------------------------------------------------------

n <- 2000

dummy1 <- as.matrix(dummy_step(n, 1, "Media"))
dummy2 <- as.matrix(dummy_on_off(n, c(1, 1050, 1250, 1600), 
                                 c(999, 1200,1500, n)))

pars <- list(ar = 0.2, 
             omega = 1, alpha = .13, beta = .85, 
             deltaMedia = 1, 
             deltaVar = c(-5, -4, -2, -3))

t_ast <- c(999, 1200,1500)
t_til <- c(1050, 1250, 1600)

set.seed(120051234)
data <- modelo_smooth2(pars, dummy1, dummy2, t_ast, t_til, n)
yt <- data$yt
Varyt <- var(yt[1:50])
plot(data$int2, type = 'l', main = 'aaa')

ggplot(data, aes(x = time, y = yt)) + 
  geom_line(size = 0.52, colour = "#000000") +
  ylab(bquote( ~ y[t])) + xlab("Time") +
  theme(axis.title.y = element_text(angle = 0)) + 
  geom_vline(xintercept = t_ast, colour = 'blue', size = 1.5, linetype = "dashed") + 
  geom_vline(xintercept = t_til, colour = 'red', size = 1.5, linetype = "dotdash") 

pars_init <- list(
  psi2 = log(.25),
  psi3 = log(.75),
  ar = .2,
  deltaMedia = 1,
  deltaVar = c(-5, -4, -2, -3)
)
alpha_order <- pars_init$psi2 %>% length()
beta_order <- pars_init$psi3 %>% length()
kmed <- pars_init$deltaMedia %>% length()
kvar <- pars_init$deltaVar %>% length()

(opt <- estimando(llike_suave2, pars_init))

media_cond <- esp_cond_geral_sauve(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast,
  t_til,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

var_cond <- var_cond_geral_sauve(
  data = yt,
  est = opt,
  dummy1 = dummy1,
  dummy2 = dummy2,
  t_ast,
  t_til,
  alpha_order,
  beta_order,
  kmed,
  kvar,
  n
)

resid_pad <- (yt - media_cond)/sqrt(var_cond)

mean(resid_pad)
var(resid_pad)

plot(resid_pad, type = 'l')
