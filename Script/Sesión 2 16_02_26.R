# Vanessa M Uresti Rdidriguez 
# Matricula: 2076842
# Maestria en Ciencias Forestales 
# 16/02/2025
# sesión 2
# Importar datos --------------------------------------------------------

# Función read.csv

IE <-read.csv("01_datos/vivero.csv", header = T)
IE$Tratamiento <-as.factor(IE$Tratamiento)

View(IE)

tapply(IE$IE, IE$Tratamiento, mean)
tapply(IE$IE, IE$Tratamiento, sd)
tapply(IE$IE, IE$Tratamiento, var)


# Diseño de gráfico -------------------------------------------------------


boxplot(IE$IE)

boxplot(IE$IE ~ IE$Tratamiento,
        col = "lightblue",
        xlab = "Fertilizante",
        ylab = "IE",
        main = "Vivero FCF",
        ylim = c(0.4, 1.2))
# El símbolo ~ se utiliza para indicar que una variable depende de otra,
# especialmente cuando se trabaja con varias variables dentro de un modelo.

# Para prevenir errores al compilar, es importante revisar bien la carpeta
# de trabajo. Después de guardar nuevamente el archivo, ya se puede compilar.


# Pruebas de normalida ----------------------------------------------------
#Safiro y Kormogorof

shapiro.test(IE$IE)

# Homogeneidad de varianza ------------------------------------------------

bartlett.test(IE$IE ~ IE$Tratamiento)


hist(IE$IE,
     col = "blue",
     ylim = c(0,12),
     main = "",
     ylab ="Frecuencia",
)
