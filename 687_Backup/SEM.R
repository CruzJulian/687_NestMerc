library(lavaan)
library (semTools)
library(readxl)
library(corrplot)
library (semPlot)



'Lectura datos'
'data_1 <- read_excel("data 1.xlsx",sheet="Hoja1")'

'construccion matriz varianza-covarianza'
cov<-matrix(data_1 $a,21,21)
colnames(cov) <- rownames(cov) <- c("x1", "x2","x3", "x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21")

'Ecuaciones estruturales'
modelo <-'
# modelo de medida
V1=~x1+x2+x3+x4+x5+x6
V3=~ x7+x8+x9+x10+x11
V2 =~ x12+x13+x14+x15+x16+x17
WON=~x18+x19+x20+x21

# modelo estructural
WON~V3+V2
V2 ~ V1+V3
'
'ajuste por maxima verosimilitud'
fit <- sem(modelo, sample.cov=cov, sample.nobs=231)
fit

'Resumen del modelo: se obtienen todas las tablas relacionadas con la bondad de ajuste'
summary(fit,rsquare=TRUE, fit.measures=TRUE)

'Demas indicadores del modelo'
fitMeasures(f, c("chisq", "df", "pvalue", "cfi", "rmsea"))


'Informacion relevante para las tablas de la pagina 84 y 85'
standardizedSolution( fit )
fitMeasures( fit )

'matriz de correlaciones'
lavCor(f)

'Grafico del modelo'
semPaths(fit, title = FALSE, curvePivot = TRUE)
semPaths(fit,"std")

'correlaciones entre variables y posibles nuevas relaciones'
M <- cor(BASE_SEM)
corrplot(M, method = "ellipse")


'analisis factorial confirmatorio'
f <- cfa(modelo,sample.cov =cov, sample.nobs=231)
summary(f, rsquare=TRUE, fit.measures=TRUE)
lavResiduals(f)



