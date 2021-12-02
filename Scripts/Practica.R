library(tidyverse)
library(data.table)
library(GGally)
library(tidymodels)
library(gridExtra)
library(scales)
library(broom.mixed)

source("Scripts/Funciones utiles.R", encoding = "UTF-8")

#### Dataset de calamares: ####
#https://highstat.com/Books/Book2/ZuurDataMixedModelling.zip

#Ver el libro para entender las variables

#### Leo los datos y análisis exploratorios ####

datos <- fread("Datos/Squid.txt")
#Paso el mes a factor:
datos$MONTH <- factor(datos$MONTH, levels = 1:12)

#Saco datos sucios:
datos <- datos[Testisweight >= 2]

summary(datos)

ggpairs(datos, mapping = aes(col = MONTH))

#### Modelo lineal inicial ####
#Queremos explicar el Testisweigth con DML y mes:
modelo.inicial <- lm(Testisweight ~ MONTH + DML, data = datos)

#Chequiemos supuestos:

gg.plot.modelo(modelo.inicial)

#Hay una heterocedasticidad fuerte

#Que pasa si graficamos la variable vs los residuos?

tidy.modelo.inicial <- augment(modelo.inicial)

tidy.modelo.inicial %>%
  ggplot(aes(DML, .std.resid)) +
  geom_hline(size = 1, colour = "black", yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_point() +
  geom_smooth(se = FALSE, col = muted("red"), alpha = 0.75) +
  labs(title = "Residuos vs valores predichos", x = "Predichos", y = "Residuos estandarizados") + 
  theme_bw()

#Aca se ve claro la depenencia de la vairanza con esta variable

#Siendo que hay heterocedasticidad, el error estandar del modelo esta mal estimado, y las inferencias tambien, los p-valores no son confiables:

summary(modelo.inicial)


##### Modelado de varianza #####
#### Paquete nlme ####
library(nlme)
#Modelo original pero con gls:

modelo.inicial.gls <- gls(Testisweight ~ MONTH + DML, data = datos)

summary(modelo.inicial.gls)

#Comparemos con el modelo original:

gg.plot.modelo(modelo.inicial.gls)

grid.arrange(gg.confint(modelo.inicial), gg.confint(modelo.inicial.gls), ncol = 2)

#Son iguales!
#gls sin otros parametros equivale a un lm
#Ahora probemos con modelados de varianza:




#### varFixed ####

modelo.varfixed<- gls(Testisweight ~ MONTH + DML, weights = varFixed(~DML), data = datos)

summary(modelo.varfixed) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varfixed)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varfixed)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varfixed), ncol = 2)

#Se arreglo el problema?
#No, seguimos probando



#### varIdent ####

modelo.varident<- gls(Testisweight ~ MONTH + DML, weights = varIdent(form= ~ 1 | MONTH), data = datos)

summary(modelo.varident) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varident)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varident)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varident), ncol = 2)


#Se arreglo el problema?
#No, seguimos probando



#### varPower ####

modelo.varpower<- gls(Testisweight ~ MONTH + DML, weights = varPower(form = ~ DML), data = datos)

summary(modelo.varpower) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varpower)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varpower)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varpower), ncol = 2)


#Se arreglo el problema?
#Pareciera que si, seguimos probando igual



#### varExp ####

modelo.varexp<- gls(Testisweight ~ MONTH + DML, weights = varExp(form = ~ DML), data = datos)

summary(modelo.varexp) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varexp)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varexp)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varexp), ncol = 2)


#Se arreglo el problema?
#Pareciera que si, seguimos probando igual



#### varConstPower ####

modelo.varconstpower<- gls(Testisweight ~ MONTH + DML, weights = varConstPower(form = ~ DML), data = datos)

summary(modelo.varconstpower) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varconstpower)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varconstpower)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varconstpower), ncol = 2)


#Se arreglo el problema?
#Pareciera que si, seguimos probando igual



#### varComb ####

modelo.varcomb<- gls(Testisweight ~ MONTH + DML, weights = varComb(varIdent(form = ~ 1 | MONTH) ,
                                                                    varExp(form = ~ DML)), data = datos)

summary(modelo.varcomb) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varcomb)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varcomb)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varcomb), ncol = 2)


#Se arreglo el problema?
#Pareciera que si, seguimos probando igual



#Podemos probar otras tambien:
#Si se usa varExp(), varPower(), etc sin nada adentro, modela con la variable respuesta


#### varExp con respuesta ####

modelo.varexp.respuesta<- gls(Testisweight ~ MONTH + DML, weights = varExp(), data = datos)

summary(modelo.varexp.respuesta) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varexp.respuesta)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varexp.respuesta)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varexp.respuesta), ncol = 2)


#Se arreglo el problema?
#Pareciera que si, seguimos probando igual


#Se puede modelar varianza con con un varExp (u otra) por nivel de alguna variable:

#### varExp con respuesta ####

modelo.varexp.niveles<- gls(Testisweight ~ MONTH + DML, weights = varExp(form = ~DML|MONTH), data = datos)

summary(modelo.varexp.respuesta) #que cambio? donde se ven los nuevos parametros estimados?

#Comparemos con el modelo gls original:

gg.plot.modelo(modelo.varexp.respuesta)

gg.plot.comparacion.modelos(modelo.inicial.gls, modelo.varexp.respuesta)

grid.arrange(gg.confint(modelo.inicial.gls), gg.confint(modelo.varexp.respuesta), ncol = 2)


#Se arreglo el problema?
#Pareciera que si, seguimos probando igual





##### Comparacion de los modelos #####

#anova entre modelos: mide si los modelos son distintos con el anterior de la lista

anova(modelo.inicial.gls,
      modelo.varfixed,
      modelo.varident,
      modelo.varpower,
      modelo.varexp,
      modelo.varconstpower,
      modelo.varcomb,
      modelo.varexp.respuesta)


#Como elegimos al mejor?
#Busco el AIC de cada uno, los ordeno de menor a mayor (de mejor a peor):


aic.modelos <- AIC(modelo.inicial.gls,
                   modelo.varfixed,
                   modelo.varident,
                   modelo.varpower,
                   modelo.varexp,
                   modelo.varconstpower,
                   modelo.varcomb,
                   modelo.varexp.respuesta) %>%
  arrange(AIC)

aic.modelos #notar quien esta ultimo y los df (parametros estimados)

#ahora anova pero ordenado:
lista.modelos <- mget(rownames(aic.modelos))

do.call(anova, 
        lapply(names(lista.modelos), as.name))

#Los primeros son diferentes? Con cual nos quedamos?


