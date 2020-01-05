library(haven)
Caso_SEM_evaluacion_1_ <- read_sav("~/Downloads/Caso_SEM_evaluacion_1_, ")
View(Caso_SEM_evaluacion_1_)

library(lavaan)
library(semPlot)
library(semTools)

#modelosemevaluci??nmodeloCFA

modelo_cfaevalucion<- '

#varianzas de errores

JS1~~JS1
JS2~~JS2
JS3~~JS3
JS4~~JS4
JS5~~JS5

OC1~~OC1
OC2~~OC2
OC3~~OC3
OC4~~OC4

SI1~~SI1
SI2~~SI2
SI3~~SI3
SI4~~SI4

EP1~~EP1
EP2~~EP2
EP3~~EP3
EP4~~EP4

AC1~~AC1
AC2~~AC2
AC3~~AC3
AC4~~AC4

#COMO ANALIZAR LAS CARGAS O REGRESIONES

js=~JS1+JS2+JS3+JS4+JS5
oc=~OC1+OC2+OC3+OC4
si=~SI1+SI2+SI3+SI4
ep=~EP1+EP2+EP3+EP4
ac=~AC1+AC2+AC3+AC4

#VARIANZA DE LOS FACTORES

js~~js
oc~~oc
si~~si
ep~~ep
ac~~ac

#COVARIANZAS

js~~oc
js~~si
js~~ep
js~~ac
oc~~si
oc~~ep
oc~~ac
si~~ep
si~~ac
ep~~ac


'

#estimaci??n del modelo

fit<-lavaan(modelo_cfaevalucion,data=Caso_SEM_evaluacion_1_, std.lv = TRUE, mimic = "eqs", estimator = "ml", verbose = TRUE, warn = TRUE)

#peticion de elementos en la salida

summary(fit, fit.measures=TRUE,standardized=TRUE, rsquare=TRUE)
resid(fit,type="standardized")

#Generacion del grafico
semPaths(fit,what = "paths", style = "lisrel", layout = "tree")


