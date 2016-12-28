
# Cargamos librerias

library(dplyr)
library(ggplot2)
library(tseries) 
library(zoo) 
library(forecast)

# Cargamos la base de datos

leche =read.csv("monthly-milk-production-pounds-p.csv")


# Analisis de los datos 

head(leche)

# Que horror de nombre de columna, vamos a cambiarlo

colnames(leche)[2] <- "production"

head(leche)


# asi esta mejor, seguimos mirando los datos

str(leche)

nrow (leche)

dim(leche)

nrow(unique(leche[1]))

summary(leche)

# vemos que hay un NA. Normal porque 14 anos no tienen 169 mmeses....

# lo quitamos

leche<-na.omit(leche)

# volvemos a analizar de los datos 

head(leche)

str(leche)

nrow (leche)

dim(leche)

nrow(unique(leche[1]))

summary(leche)


# Vale ya tenemos los datos limpios, bueno, sin NA

# Hacemos una copia del data frame

lecheB <- leche

# y vamos a ver graficamente los datos

plot(lecheB)

# ooook vemos que siguen un incremento constante pero vamos a ver por mes como
# evoluciona nos creamos la columna mes

lecheB$mes <- substring(lecheB$Month,6,7)

head(lecheB)

# OK vamos a pintar estos datos

ggplot(lecheB)

G <- ggplot(lecheB, aes(Month,production))
G + geom_point(aes(color=mes))

# Curiososo vemos que todos lo meses de mayo, junio y julio (puntos verdes) siempre
# estan en la parte de arriba del grafico, y los de enero y diciembre en la parte baja

# vamos a ver mas graficos

as.numeric(lecheB$mes)
as.factor(lecheB$mes)
boxplot(lecheB[,2]~lecheB$mes)

# uuhmmm vamos echar un vistazo al dataframe original

plot(leche)
lines(leche)

# OK, entonces agarramos aplicar metodologia Box-Jenkins de selección de modelo
# y vamos a aplicarlo. Para ello primero vamos a ver el ACF y PACF

# ACF

acf(lecheB[,2],lag.max=48)

# PACF 

pacf(lecheB[,2],lag.max=48)

# Y ahora todo junto XoD

tsdisplay(leche[,2])


# OK, vayamos con las preguntas de seleccion

#   1a Pregunta: Parece estacionaria la serie?

# vaya pues no porque su esperanza no es constantes en el tiempo, tiene una tendencia
# y estacionalidad. Entonces descompondremos los datos el componente estacional, 
# tendencia y resto

leche_serie <- ts(leche[,2],start=c(1962,1),frequency=12)

# vemos como quedo

head(leche_serie)

# pero de verdad

leche_serie

plot(leche_serie, main="Milk per cow", xlab="time", ylab="liter", col="blue")

# decomponemos por por estacionalidad , tendencia y resto

leche_desc <- stl(leche_serie,s.window=12)

# vemos como quedo

head(leche_desc)

# pero de verdad 2

leche_desc

plot(leche_desc, main="Descomposición de la serie", col="blue")

# 1er cuadro: serie original (suma de los siguiente 3)
# 2o componente estacionnal
# la tendencia
# y el ultimo el resto


# 2a pregunta: Decrece ACF a 0 en la parte del resto?

# Primero extraemos el resto

leche_serie.dif.12 <- diff(leche_serie, lag = 12) 
leche_serie.stac <- diff(leche_serie.dif.12, diff = 1) 

plot(leche_serie.stac)

# Ya es estacionaria y miramos acf

acf(leche_serie.stac)

# SI, decrece a 0 y pasamos a la siguiente pregunta


# 3a pregunta: Decrece ACF rapidamente?

acf(leche_serie.stac)

# SI, decrece muy rapidamente y pasamos a la siguiente pregunta


# 4a pregunta (prometo que es la ultima) Decrece PACF rapidamente?

pacf(leche_serie.stac)

# pues va a ser que si


# decision vamos por ARIMA

Arima <- auto.arima(leche_serie,seasonal=TRUE,trace=TRUE)


# El mejor modelo nos ha dicho la mquina, que es muy lista, es ARIMA(0,1,1)(0,1,1)[12]


#  PREDICCION DEL MODELO

# prognostico del modelo para los proximos 2 años (24 meses)

Prediccion <- forecast(Arima,h=24) 

Prediccion

plot(Prediccion)

# Tachan !!!!!!!!!!


