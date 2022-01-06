library(ggplot2)
library(patchwork)

tema <- theme(axis.title.y = element_text(size = 30),
      axis.title.x = element_text(size = 30),
      axis.text.x = element_text(size = 30),
      axis.text.y = element_text(size = 30))


grafico_var_cond <- function(data){
  ggplot(data, aes(x = time, y = sqrt(var_cond))) +
    labs(x = "Tempo", y = "Desvio Condicional") + 
    geom_line(size = 1L, colour = "red") + 
    geom_line(aes(x = time, y = abs(yt)), colour = "blue", alpha = .5) +
    tema +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")
  
}

grafico_var_incond <- function(data){
  ggplot(data, aes(x = time, y = sqrt(var_incond))) +
    labs(x = "Tempo", y = "Desvio Incondicional") + 
    geom_line(size = 1L, colour = "red") + 
    geom_line(aes(x = time, y = abs(yt-med_incond)), 
              colour = "blue", alpha = .5)  +
    tema +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")
  
}

grafico_qqplot <- function(data){
  ggplot(data, aes(sample = resid_pad)) + 
    stat_qq() + 
    geom_abline(slope = 1, intercept = 0) + 
    ylim(-6,6) + 
    theme_minimal() +
    labs(x = "Quantil Teorico", y = 'Quantil Amostral') + 
    scale_x_continuous(limits = c(-6, 6),  breaks = c(-6, -4, -2, 0, 2, 4, 6))  +
    tema
  
}

grafico_hist <- function(data){
  ggplot(data, aes(x = resid_pad)) + 
    geom_histogram(aes(y =..density..), fill = "#0c4c8a") +
    theme_minimal() +
    labs(x = "Residuos padronizados", y = 'Densidade') + 
    scale_x_continuous(limits = c(-6, 6),  breaks = c(-6, -4, -2, 0, 2, 4, 6)) +
    stat_function(fun = dnorm, args = list(0, 1), color = 'red')  +
    tema
} 

juntando_hist_qq <- function(data){
  p1 <- grafico_qqplot(data)
  p2 <- grafico_hist(data)
  
  p1 + p2
}