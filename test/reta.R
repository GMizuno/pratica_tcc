reta <- function(delta1, delta2, t_ast, t_til, t){
  intercept <- ((t_til)*exp(delta1) - (t_ast)*exp(delta2))/(t_til - t_ast)
  ang <- (exp(delta2) - exp(delta1))/(t_til - t_ast)
  
  intercept + ang*t
}


plot(1:5, reta(1, 5, 1, 5, 1:5), type='l')


plot(1:50, reta(1, 5, 1, 5, 1:50), type='l')
