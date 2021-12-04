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
