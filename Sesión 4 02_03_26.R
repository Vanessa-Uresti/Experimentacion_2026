# Metodos estadisticos 2026
# Maestria en ciencias forestales 

# Vanessa M Uresti Rodríguez 
# Matrícula: 2076842

#**Sesión 4**
#Fecha 02/03/26


# ingreso de datos --------------------------------------------------------


x2 <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y2 <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)

mean(x2)
mean(y2)


# ej 1 -------------------------------------------------------------
datos <- data.frame(
  Produccion_trigo = c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40),
  Precio_harina    = c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
)

plot(datos$Produccion_trigo, datos$Precio_harina)

mean_x <- mean(datos$Produccion_trigo)
mean_y <- mean(datos$Precio_harina)


cor.test(datos$Produccion_trigo, datos$Precio_harina)

#descargar complemento lmtest

#install.packages("lmtest")
library(lmtest)

fit.lm <- lm(datos$Precio_harina ~ datos$Produccion_trigo)
summary(fit.lm)

anova(fit.lm)

#residuales en el modelo lineal fit.lm
fit.lm$residuals

#valores ajustados Yprima en el modelo lineal
fit.lm$fitted.values


#gráfica de Yprima vs residuales
plot(fit.lm$fitted.values, fit.lm$residuals)



# ej 2 -------------------------------------------------------------


x <- c(30, 28, 32, 25,25, 25, 22, 24, 35, 40)

y <- c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25) 



#Prueba de breunch-pagan
bptest(fit.lm)
#el resultado de esta prueba de p-value= 0.5641 indica si se cumple o no el 
#criterio

#Agregar columna en la recta utilizadno Bo y B1
datos$recta <- 74.1151 - 1.3537 * datos$Produccion_trigo
datos$residuales <- datos$Precio_harina - datos$recta
datos


#estimar residuales
sum(datos$residuales^2)

#varianza
sum(datos$residuales^2)/8
#25.99


#desviación estandar
sqrt(sum(datos$residuales^2)/8)
##5.05

#hay que correr el lm para la linea de tendencia


plot(datos$Produccion_trigo, datos$Precio_harina,
     col ="indianred",
     xlab ="produccion trigo (Ton/ha",
     ylab ="Precio harina (Euros",
     pch = 19,
     cex = 1.2)
abline(fit.lm, col = "red")

