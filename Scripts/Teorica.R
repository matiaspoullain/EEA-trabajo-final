########### Teorica: ##########

##### Generamos datos para ajustar una regresión lineal simple:

#Queremos explicar la variable "target" en funcion de la variable "var1"
#Primero mostremos el caso homocedastico:
rm(list = ls())
gc()
library(tidyverse)
library(data.table)
library(nlme)
library(tidymodels)
library(gridExtra)
library(broom.mixed)
source("Scripts/Funciones utiles.R", encoding = "UTF-8")
#var1: 200 numero al azar entre 0 y 100
set.seed(2)

n.datos <- 500

var1 <- runif(n.datos, min = 0, max = 10)

#target: depende linealmente de var1: ordenada al origen de 3 y pendiente de 2
#pero le agregamos un ruido gaussiano de desvio 4 sin variar los parámetros a lo lardo de var1:
set.seed(2)
target <- 3 + var1 * 2 + rnorm(n.datos, sd = 4)

datos <- data.table(target, var1)

#Observemos los datos:

datos %>%
  ggplot(aes(x = var1, y = target)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)

#Ajustemos un modelo lineal y observemos los supuestos:

modelo.bien <- lm(target ~ var1, data = datos)

gg.plot.modelo(modelo.bien)

#Los supuestos se cumplen entonces podemos mirar las significancias:

summary(modelo.bien)

#Los parámetros que nosotros forzamos estan bastante bien estimados y tenemos un R2 grande

#Esto es a lo que vamos a intentar llegar, ahora probemos con un modelo heterocedastico:

#Construyamos la misma variable target pero haciendo su varianza dependiente de var1

set.seed(2)
datos$target2 <- 3 + var1 * 2 + rnorm(n.datos, sd =  4 * exp(0.4*var1))

#Observemos los datos:

datos %>%
  ggplot(aes(x = var1, y = target2)) +
  geom_point() +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)

#Ajustemos un modelo lineal y observemos los supuestos:

modelo.mal <- lm(target2 ~ var1, data = datos)

gg.plot.modelo(modelo.mal)

#Los supuestos no se cumplen, veamos el summary:

summary(modelo.mal)

#La pendiente esta mal estimada y no es significativamente distinta a 0!
#El R2 es bajo, el sigma estimado es muy alto

#Comparemos con el modelo anterior:
lista.modelos <- list(modelo.bien = modelo.bien, modelo.mal = modelo.mal)

df.modelos <- map_df(lista.modelos, tidy, conf.int = TRUE, .id = "modelo")

df.modelos %>%
ggplot(aes(estimate, modelo, color=p.value < 0.05, xmin = conf.low, xmax = conf.high, height = 0)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 4, color = "black") +
  geom_errorbarh() +
  facet_wrap(term~., scales = "free_x") +
  scale_color_manual(values=c('firebrick', 'forestgreen')) +
  guides(color="none") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  labs(y = "Modelos", x = "Estimación", title = "Intervalos de confianza para la estimación de los coeficientes")


#Apliquemos un modelado de varianza

modelo.corregido <- gls(target2 ~ var1, weights = varExp(form = ~ var1), data = datos)



#Miremos los supuestos y comparemoslos con los de modelo.mal:

gg.plot.comparacion.modelos(modelo.mal, modelo.corregido)

#Cambió muchisimo!

summary(modelo.corregido)

#Comparemos estimaciones:

lista.modelos <- list(modelo.bien = modelo.bien, modelo.mal = modelo.mal, modelo.corregido = modelo.corregido)

df.modelos <- map_df(lista.modelos, tidy, conf.int = TRUE, .id = "modelo")

df.modelos %>%
  ggplot(aes(estimate, fct_relevel(modelo, c("modelo.corregido", "modelo.mal", "modelo.bien")), color=p.value < 0.05, xmin = conf.low, xmax = conf.high, height = 0)) +
  geom_point() +
  geom_vline(xintercept = 0, lty = 4, color = "black") +
  geom_errorbarh() +
  facet_wrap(term~., scales = "free_x") +
  scale_color_manual(values=c('firebrick', 'forestgreen')) +
  guides(color="none") +
  scale_x_continuous(n.breaks = 10) +
  theme_bw() +
  labs(y = "Modelos", x = "Estimación", title = "Intervalos de confianza para la estimación de los coeficientes")

