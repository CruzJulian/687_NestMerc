library(haven)
Caso_SEM_transversal <- read_sav("~/Downloads/Caso_SEM_transversal.sav")
View(Caso_SEM_transversal)

# Cargar la libreria 

library(lavaan)
library(semPlot)
library(semTools)

#Varian de los factores


modelo_tam<-'

#descripcion de modelo 

eou =~ 1*eou1+eou2+eou3+eou4 
use =~ 1*Use1+Use2
pu =~ 1*pu1+pu2+pu3+pu4

# ecuaci??n modelo estructural 

use~pu+eou
pu~eou

#Varianza de Factores

eou~~eou

#covarianzas 

# eou~~eou
# pu~~pu
# use~~use


# Varian de los Errores

eou1~~eou1
eou2~~eou2
eou3~~eou3
eou4~~eou4

pu1~~pu1
pu2~~pu2
pu3~~pu3
pu4~~pu4

Use1~~Use1
Use2~~Use2

# varianzas de los errores de las dependientes 

pu~~pu
use~~use

'
#estimacion el modelo 

fit<-lavaan (modelo_tam, data = Caso_SEM_transversal, std.lv = FALSE, mimic = "eqs", estimator = "ml", verbose = TRUE ,warn = TRUE)

#Peticion de elementos de la salida. 

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
resid(fit,type = "standardized")

# graficar el modelo 

semPaths(fit, what = "paths", style = "lisrel", layout = "tree")