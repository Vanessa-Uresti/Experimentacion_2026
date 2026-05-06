# Metodos estadisticos
# Maestría en ciencias forestales
# Vanessa M Uresti Rodríguez
# Matrícula: 2076842

#**Sesión 3**: Fecha 23/02/26


##################################################################


# Insertar datos ----------------------------------------------------------

resp <- data.frame(
  Tiempo = c(12,15,17,18,20,21,22,26),
  Edad = c(14,25,20,35,45,30,60,95)
)
resp

#Crear nuevas columnas (1 a 8)
resp$Rango_Tiempo <-rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <-rank(resp$Edad, ties.method = "first")


#para ver los resultado
resp

plot(resp$Tiempo, resp$Edad)
plot(resp$Rango_Tiempo, resp$Rango_Edad)


cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")


# Prueba de Kendall -------------------------------------------------------

tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)
cor.test(tau$A, tau$B, method = "kendall")


# Correlación punto biserial  ---------------------------------------------

set.seed(123)
#datos dicticios y para que sea siempre los mismos datos, si no sería aleatorio

#Número de observaciones
n <-20

#Generar horas de estudio (entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = TRUE)

# Definir la probabilidad de aprobar dependiendo del tiempo dedicado al estudio.
# Entre más horas se estudien, mayor será la probabilidad de pasar.

Resultado <-sapply(Horas_estudio, function (horas){
  ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado")
  
})


# Crear data frame
estudio <-data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)

View(estudio)


# Crear variable dicotómica: 1 = Aprobado, 0 = Reprobado

estudio$Resultado_bin <-ifelse(estudio$Resultado =="Aprobado", 1, 0)
head(estudio)


#correlacion

cor.test(estudio$Horas_estudio, estudio$Resultado_bin, method = "pearson")
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)