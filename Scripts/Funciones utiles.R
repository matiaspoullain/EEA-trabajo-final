#Funciones utiles:

#Plotear plot(modelo) pero con ggplot:

cd_cont_pos <- function(leverage, level, model) {sqrt(level*length(coef(model))*(1-leverage)/leverage)}
cd_cont_neg <- function(leverage, level, model) {-cd_cont_pos(leverage, level, model)}

gg.plot.modelo <- function(modelo){
  modelo.aumentado <- augment(modelo)
  
  if(class(modelo) == "gls"){
    modelo.aumentado$.std.resid <- residuals(modelo, "normalized")
  }
  
  g1 <- ggplot(modelo.aumentado, aes(.fitted, .std.resid)) +
    geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_point() +
    geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) +
    labs(title = "Residuos vs valores predichos", x = "Predichos", y = "Residuos estandarizados") + 
    theme_bw()
  
  g2 <- ggplot(modelo.aumentado, 
              aes(sample = .std.resid)) +
    stat_qq() +
    geom_abline() +
    labs(title = "QQ-plot", x = "Cuantiles teóricos", y = "Cuantiles muestrales") + 
    theme_bw()
  
  g3 <- ggplot(modelo.aumentado, 
              aes(.fitted, sqrt(abs(.std.resid)))) +
    geom_point() +
    geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) + 
    theme_bw() +
    labs(title = "Scale-location plot", x = "Predichos", y = expression(sqrt(abs(Residuos~estandarizados))))  
  
  if(class(modelo) != "gls"){
    g4 <- ggplot(modelo.aumentado, 
                aes(.hat, .std.resid)) +
      geom_vline(size = 1, colour = "black", xintercept = 0, linetype = "dashed", alpha = 0.3) +
      geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
      stat_function(fun = cd_cont_pos, args = list(level = 0.5, model = modelo), xlim = c(0, 0.25), lty = 2, colour = "red") +
      stat_function(fun = cd_cont_neg, args = list(level = 0.5, model = modelo), xlim = c(0, 0.25), lty = 2, colour = "red") +
      coord_cartesian(xlim = c(-0.001, max(modelo.aumentado$.hat) * 1.2), ylim = c(min(modelo.aumentado$.std.resid) * 1.2, max(modelo.aumentado$.std.resid) * 1.2)) +
      geom_point() + 
      geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) + 
      theme_bw() +
      labs(title = "Residuos estandarizados vs Leverage", x = "Leverage", y = "Residuos estandarizados")
    
    grid.arrange(g1, g2, g3, g4)
    
  }else{
    
    grid.arrange(g1, g2, g3, nrow = 2)
  }
}


gg.plot.comparacion.modelos <- function(modelo.original, modelo.nuevo){
  modelo.original.aumentado <- augment(modelo.original)
  
  if(class(modelo.original) == "gls"){
    modelo.original.aumentado$.std.resid <- residuals(modelo.original, "normalized")
  }
  
  g1 <- ggplot(modelo.original.aumentado, aes(.fitted, .std.resid)) +
    geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_point() +
    geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) +
    labs(title = "Residuos vs valores predichos", x = "Predichos", y = "Residuos estandarizados", subtitle = deparse(substitute(modelo.original))) + 
    theme_bw()
  
  g2 <- ggplot(modelo.original.aumentado, 
               aes(sample = .std.resid)) +
    stat_qq() +
    geom_abline() +
    labs(title = "QQ-plot", x = "Cuantiles teóricos", y = "Cuantiles muestrales", subtitle = deparse(substitute(modelo.original))) + 
    theme_bw()
  
  g3 <- ggplot(modelo.original.aumentado, 
               aes(.fitted, sqrt(abs(.std.resid)))) +
    geom_point() +
    geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) + 
    theme_bw() +
    labs(title = "Scale-location plot", x = "Predichos", y = expression(sqrt(abs(Residuos~estandarizados))), subtitle = deparse(substitute(modelo.original)))
  
  
  modelo.nuevo.aumentado <- augment(modelo.nuevo)
  
  if(class(modelo.nuevo) == "gls"){
    modelo.nuevo.aumentado$.std.resid <- residuals(modelo.nuevo, "normalized")
  }
  
  g4 <- ggplot(modelo.nuevo.aumentado, aes(.fitted, .std.resid)) +
    geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_point() +
    geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) +
    labs(title = "Residuos vs valores predichos", x = "Predichos", y = "Residuos estandarizados", subtitle = deparse(substitute(modelo.nuevo))) + 
    theme_bw()
  
  g5 <- ggplot(modelo.nuevo.aumentado, 
               aes(sample = .std.resid)) +
    stat_qq() +
    geom_abline() +
    labs(title = "QQ-plot", x = "Cuantiles teóricos", y = "Cuantiles muestrales", subtitle = deparse(substitute(modelo.nuevo))) + 
    theme_bw()
  
  g6 <- ggplot(modelo.nuevo.aumentado, 
               aes(.fitted, sqrt(abs(.std.resid)))) +
    geom_point() +
    geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) + 
    theme_bw() +
    labs(title = "Scale-location plot", x = "Predichos", y = expression(sqrt(abs(Residuos~estandarizados))), subtitle = deparse(substitute(modelo.nuevo)))
  
  grid.arrange(g1, g2, g3, g4, g5, g6, nrow = 2)
}





gg.confint <- function(modelo){
  tidy.modelo <- tidy(modelo, conf.int = TRUE)
  tidy.modelo$term <- fct_rev(factor(tidy.modelo$term, levels = tidy.modelo$term))
  
  ggplot(tidy.modelo, aes(estimate, term, color=p.value < 0.05, xmin = conf.low, xmax = conf.high, height = 0)) +
    geom_point() +
    geom_vline(xintercept = 0, lty = 4, color = "black") +
    geom_errorbarh() +
    scale_color_manual(values=c('firebrick', 'forestgreen')) +
    guides(color="none") +
    theme_bw() +
    labs(y = "Coeficientes β", x = "Estimación", title = "Intervalos de confianza para la estimación de los coeficientes", subtitle = deparse(substitute(modelo)))
}
