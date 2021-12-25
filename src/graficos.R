require(ggplot2)

grafico_var_cond <- function(data){
  ggplot(data, aes(x = time, y = sqrt(var_cond))) +
    labs(x = "Tempo", y = "Desvio Condicional") + 
    geom_line(size = 1L, colour = "red") + 
    geom_line(aes(x = time, y = abs(yt)), colour = "blue", alpha = .5) +
    theme(axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")
  
}


grafico_var_incond <- function(data){
  ggplot(data, aes(x = time, y = sqrt(var_incond))) +
    labs(x = "Tempo", y = "Desvio Incondicional") + 
    geom_line(size = 1L, colour = "red") + 
    geom_line(aes(x = time, y = abs(yt-med_incond)), 
              colour = "blue", alpha = .5)  +
    theme(axis.title.y = element_text(size = 15),
          axis.title.x = element_text(size = 15),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m")
  
}

