library(haven)
Caso_SEM_evaluacion <- read_sav("~/Downloads/Caso_SEM_evaluacion.sav")
View(Caso_SEM_evaluacio) 

library(lavaan)
library(semPlot)
library(semTools)

# Modelo CFA

modelo_CFAEVA<-'

# varianzas errores

JS1~~JS1
JS2~~JS2
JS3~~JS3
JS4~~JS4
JS5~~JS5

OC1~~OC1
OC2~~OC2
OC3~~OC3
OC4~~OC4

SI4~~SI4
SI4~~SI4
SI4~~SI4
SI4~~SI4

EP1~~EP1
EP2~~EP2
EP3~~EP3
EP4~~EP4

AC1~~AC1
AC2~~AC2
AC3~~AC3
AC4~~AC4

# CARGAS 

js=~JS1+JS2+JS3+JS4+JS5
oc=~OC1+OC2+OC3+OC4
si=~SI1+SI2+SI3+SI4
ep=~EP1+EP2+EP3+EP4
ac=~AC1+AC2+AC3+AC4

# VARIANZA DE LOS FACTORES

js~~js
oc~~oc
si~~si
ep~~ep
ac~~ac

# COVARIANZA

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

#estimacion el modelo 

fit<-lavaan (modelo_CFAEVA, data = Caso_SEM_evaluacion, std.lv = TRUE, mimic = "eqs", estimator = "ml", verbose = TRUE ,warn = TRUE)

#Peticion de elementos de la salida. 

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
resid(fit,type = "standardized")

# graficar el modelo 

semPaths(fit, what = "paths", style = "lisrel", layout = "tree")

