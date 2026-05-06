# Metodos estadisticos 2026
# Mastria en ciancias forestales 

# Vanessa M Uresti Rodríguez 
# Matrícula: 2076842

#**Sesión 5**
#Fecha 09/03/26


# TIPOS DE MUESTRA:
# Una muestra: se utiliza cuando existe una media de referencia o media teórica
# obtenida previamente en algún estudio, y se compara con un solo grupo de individuos.

# Muestras independientes:
# corresponden a dos grupos distintos de individuos que comparten una misma variable de análisis.

#MUESTRA DEPENDIENTE: GRUPO DE INDIVIDUOS QUE SE COMPARA ANTES Y DESPUES (ESCALA
#DE TIEMPO) #EJEMPLO PRODUCCION XAÑO 2024 - 2025 AGRICOLAS, FRUTALES


############################################################################


#características de las pruebas
#datos normales, homogeneidad, n=30


# Función read.csv

IE <-read.csv("01_datos/vivero.csv", header = T)
IE$Tratamiento <-as.factor(IE$Tratamiento)

View(IE)

tapply(IE$IE, IE$Tratamiento, mean)
tapply(IE$IE, IE$Tratamiento, sd)
tapply(IE$IE, IE$Tratamiento, var)


# Diseño_____________________________________________


boxplot(IE$IE)

boxplot(IE$IE ~ IE$Tratamiento,
        col = "lightblue",
        xlab = "Fertilizante",
        ylab = "IE",
        main = "Vivero FCF",
        ylim = c(0.4, 1.2))
#~ significa en función de o sea cuando tienes varias variables
#para evitar problemas en compilación tener cuidado las carpetas donde se
#trabaja por lo que hay que guardarla de nuevo y ahora si se compila



# Pruebas de normalida ____________________________________________________
#Safiro y Kormogorof

shapiro.test(IE$IE)

# Homogeneidad de varianza ________________________________________________

bartlett.test(IE$IE ~ IE$Tratamiento)


hist(IE$IE,
     col = "blue",
     ylim = c(0,12),
     main = "",
     ylab ="Frecuencia",
     xlab = "Variable IE")

###############################################################################
# pruebas para independientes

t.test(IE$IE ~ IE$Tratamiento, var.equal =T)

# Intervalo de confianza: rango dentro del cual se espera que se encuentre un valor,
# lo que permite evaluar si el comportamiento de los datos es normal o no.

#var.equal =T (no importa si alguno de tus datos es mayor o no  que el otro)


t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative= "greater")
#con este comando es para 1 sola cola cuando ya estas diciendo en tu hipotesis
#mayor que


t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative= "less")


##############################################################################

#Prueba de T una muestra

mean(IE$IE)


t.test(mu = 0.85, IE$IE)


#prueba dependiente

Ctrl <- subset(IE$IE, IE$Tratamiento == "Ctrl")
Fert <- subset(IE$IE, IE$Tratamiento != "Ctrl")