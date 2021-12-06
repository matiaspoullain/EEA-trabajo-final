rm(list = ls())
gc()
library(tidyverse)
library(data.table)
library(nlme)
library(tidymodels)
library(gridExtra)
library(broom.mixed)
library(scales)

#Imagenes par parte teorica:

set.seed(3)

n.datos <- 200

var1 <- runif(n.datos, min = 0, max = 100)

target <- 200 + var1 * (-4) + rnorm(n.datos, sd = 20)

datos <- data.table(target, var1)

datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")
  

modelo.bien <- lm(target ~ var1, data = datos)

modelo.bien.aumentado <- augment(modelo.bien)

res.vs.pred <- function(modelo.aumentado){
  ggplot(modelo.aumentado, aes(.fitted, .std.resid)) +
  geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_point() +
  labs(x = "Predichos", y = "Residuos estandarizados") + 
  theme_bw()
  }

plot.bien <- res.vs.pred(modelo.bien.aumentado)

ggsave("Imagenes/Teorica/Plot bien.png", plot.bien, width = 4, height = 4)


#Con felcha hacia la derecha:

set.seed(3)

target <- 200 + var1 * (-4) + rnorm(n.datos, sd = 1.5 * var1)

datos <- data.table(target, var1)

datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")


modelo.derecha <- lm(target ~ var1, data = datos)

modelo.derecha.aumentado <- augment(modelo.derecha)

plot.derecha <- res.vs.pred(modelo.derecha.aumentado)

ggsave("Imagenes/Teorica/Plot derecha.png", plot.derecha, width = 4, height = 4)


#Con felcha hacia la izquierda:

set.seed(3)

target <- 200 + var1 * (-4) + rnorm(n.datos, sd = 1.5 * abs(max(var1)-var1))

datos <- data.table(target, var1)

datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")


modelo.izquierda <- lm(target ~ var1, data = datos)

modelo.izquierda.aumentado <- augment(modelo.izquierda)

plot.izquierda <- res.vs.pred(modelo.izquierda.aumentado)

ggsave("Imagenes/Teorica/Plot izquierda.png", plot.izquierda, width = 4, height = 4)


# Con panza:

set.seed(3)

target <- 200 + var1 * (-4) + rnorm(n.datos, sd = -0.08*(var1-50)**2 + 200)

datos <- data.table(target, var1)

datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")


modelo.panza <- lm(target ~ var1, data = datos)

modelo.panza.aumentado <- augment(modelo.panza)

(plot.panza <- res.vs.pred(modelo.panza.aumentado))

ggsave("Imagenes/Teorica/Plot panza.png", plot.panza, width = 4, height = 4)

# Con concava:

set.seed(3)

target <- 200 + var1 * (-4) + rnorm(n.datos, sd = 1*(var1-50)**2 + 200)

datos <- data.table(target, var1)

datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")


modelo.concava <- lm(target ~ var1, data = datos)

modelo.concava.aumentado <- augment(modelo.concava)

(plot.concava <- res.vs.pred(modelo.concava.aumentado))

ggsave("Imagenes/Teorica/Plot concava.png", plot.concava, width = 4, height = 4)



## Correcciones que no son modelados de varianza:
#Otra variable en juego:

set.seed(3)

n.datos <- 50

var1 <- runif(n.datos, min = 0, max = 100)

var2 <- runif(n.datos, min = 0, max = 100)

var3 <- runif(n.datos, min = 0, max = 100)

target1 <- 200 + 0 * var1 + rnorm(n.datos, sd = 20)

target2 <- 200 + 1 * var2 + rnorm(n.datos, sd = 20)

target3 <- 200 - 1 * var3 + rnorm(n.datos, sd = 20)


datos <- rbind(data.table(target = target1, variable = var1, grupo = "1"),
               data.table(target = target2, variable = var2, grupo = "2"),
               data.table(target = target3, variable = var3, grupo = "3"))


plot.otra.variable <- datos %>%
  ggplot(aes(x = variable, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")

ggsave("Imagenes/Teorica/Otra variable.png", plot.otra.variable, width = 4, height = 4)


plot.otra.variable.color <- datos %>%
  ggplot(aes(x = variable, y = target, col = grupo)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1", col = "Grupo")

ggsave("Imagenes/Teorica/Otra variable color.png", plot.otra.variable.color, width = 4, height = 4)


modelo.otra.variable <- lm(target ~ variable, data = datos)

modelo.otra.variable.aumentado <- augment(modelo.otra.variable)

plot.modelo.otra.variable <- res.vs.pred(modelo.otra.variable.aumentado)

ggsave("Imagenes/Teorica/Modelo otra variable.png", plot.modelo.otra.variable, width = 4, height = 4)



modelo.otra.variable.color <- lm(target ~ variable * grupo, data = datos)

modelo.otra.variable.color.aumentado <- augment(modelo.otra.variable.color)

plot.modelo.otra.variable.color <- res.vs.pred(modelo.otra.variable.color.aumentado)

ggsave("Imagenes/Teorica/Modelo otra variable color.png", plot.modelo.otra.variable.color, width = 4, height = 4)



### Transformacion logaritmica:


set.seed(3)

n.datos <- 50

var1 <- runif(n.datos, min = 5, max = 15)

target <- 1 + var1 * 4 + rnorm(n.datos, sd = 1.5 * var1)

datos <- data.table(target, var1)

plot.transformacion <- datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Target", x = "X1")

ggsave("Imagenes/Teorica/Plot transformacion.png", plot.transformacion, width = 4, height = 4)


plot.transformado <- datos %>%
  ggplot(aes(x = var1, y = log(target))) +
  geom_smooth(method = "lm", se = FALSE, alpha = 0.5) +
  geom_point() +
  theme_bw() +
  labs(y = "Log(Target)", x = "X1")

ggsave("Imagenes/Teorica/Plot transformado.png", plot.transformado, width = 4, height = 4)


#Embudo a lindo:

set.seed(2)

n.datos <- 500

var1 <- runif(n.datos, min = 0, max = 10)

set.seed(2)
datos$target2 <- 3 + var1 * 2 + rnorm(n.datos, sd =  4 * exp(0.4*var1))

modelo.mal <- lm(target2 ~ var1, data = datos)

modelo.aumentado <- augment(modelo.mal)

if(class(modelo.mal) == "gls"){
  modelo.aumentado$.std.resid <- residuals(modelo.mal, "normalized")
}

g1 <- ggplot(modelo.aumentado, aes(.fitted, .std.resid)) +
  geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_point() +
  geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) +
  labs(title = "Residuos vs valores predichos", x = "Predichos", y = "Residuos estandarizados") + 
  theme_bw()

ggsave("Imagenes/Teorica/Res vs pred modelo mal.png", g1, width = 4, height = 4)

modelo.corregido <- gls(target2 ~ var1, weights = varExp(form = ~ var1), data = datos)

modelo.aumentado <- augment(modelo.corregido)

if(class(modelo.corregido) == "gls"){
  modelo.aumentado$.std.resid <- residuals(modelo.corregido, "normalized")
}

g2 <- ggplot(modelo.aumentado, aes(.fitted, .std.resid)) +
  geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_point() +
  geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) +
  labs(title = "Residuos vs valores predichos", x = "Predichos", y = "Residuos estandarizados") + 
  theme_bw()

ggsave("Imagenes/Teorica/Res vs pred modelo corregido.png", g2, width = 4, height = 4)
