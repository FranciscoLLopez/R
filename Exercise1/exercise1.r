#========================================================================================================
# Ejercicio 1

# Cargamos el fichero
datosFichero <- scan("newcomb.txt")
# Se calcula directamente el vector 
datos <- 7400/((datosFichero+24800)*10^(-9))

# Diagrama de cajas, se muestran los dos outliers
boxplot(datos)

# Método gráfico para el diagnóstico de diferencias entre la distribución de probabilidad de una población
# los datos vienen de una distribución normal, salvo dos elementos al final de #la derecha de la grafica
qqnorm(datos)
qqline(datos,col=2)

#Funcion de densidad, se muestran en que rango de valores se concentran 
# se muestran dos pequeños saltos en los dos puntos 
plot(density(datos))

#Funcion de l
hist(datos)

# Se dibuja el conjunto de datos
plot(datos)

# Se calcula la media de los datos
media = mean(datos)
# En Km/s
velocidad1 = 1/1000 * media

# Se eliminan puntos atipicos y se calcula de nuevo la media
datos_sin_outliers = subset(datos, datos <= 298200000)

# Para confirmar que los outliers han sido eliminados correctamente
boxplot(datos_sin_outliers)
media = mean(datos_sin_outliers)
velocidad2  = 1/1000 * media

diferencia = velocidad1 - velocidad2

#========================================================================================================
#Ejercicio 2
#Cargamos el fichero
load("mamifero")
mamifero
# Se dibujan para ver la distribución
plot(mamifero)
boxplot(mamifero)
plot(density(mamifero))
summary(mamifero)


#Pregunta B
# Se calcula la covarianza entre las variables
cov(mamifero$cuerpo,mamifero$cerebro)
# Da un valor positivo alto,781398.1 indica que hay una relacion lineal directa
# Probamos con la correlacion de las variables. Que es lo mismo.
cor(mamifero$cuerpo,mamifero$cerebro)
# Muestra un valor 0.9341639, un coeficiente de correlacion lineal alto
# Calculamos recta de regresion
mod_lin <- lm(cuerpo ~ cerebro, data=mamifero)
mod_lin
summary(mod_lin)

#Dibuja recta de regresion
plot(mamifero)
abline(mod_lin,col=2)


#Pregunta C
#Mejor en escala logaritmica, observamos que existe una relacion lineal peso cuerpo - peso cerebro
plot (log(mamifero$cuerpo),log(mamifero$cerebro))
#Diagrama de cajas
boxplot(log(mamifero))

#Pregunta D.
#El peso del humano se toma en aquellos valores entre 60 y 100 kg
v2 = c((mamifero$cuerpo>60) & (mamifero$cuerpo<100))
v2

which_m12 <- (1:length(v2))[v2]
which_m12
# Devuelve dos posiciones
# La 30   85.0000 kgs 	325.00 gramos
# La 32   62.0000 kgs 	1320.00 grmos 
plot(log(mamifero))
#indica la posicion del hombre en el grafico
points(log(mamifero$cuerpo[32]),log(mamifero$cerebro[32]),pch="O",col = "red")




