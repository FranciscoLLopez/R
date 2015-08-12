# Instalacion de paquetes necesarios.
#============================================================
# Libreria para obtener las variables dummies
install.packages("dummies")
library(dummies)

# Establecimiento de semilla
set.seed(1000)

#Paso 0. Carga de fichero
#============================================================
adult_sel <- read.table("adult_sel.data",header=T,sep=",")

# En general
str(adult_sel)

# Media, cuartiles...
summary(adult_sel) 
#Paso 1. Selección de variables
#============================================================
# Se dejan al final las variables que se van a categorizar y cambiar como ingresos
# Se crea un subconjunto
df = subset(adult_sel, select = c(age,fnlwgt,educationnum,capitalgain,capitalloss,hoursperweek,ingresos,sex,race))

#Paso 2.Tratamiento de variables categoricas
#============================================================
# Se procesa la variable ingresos
ingresos = factor(df$ingresos)
# La columna ingresos tiene 2 levels
ingresos = model.matrix(~ ingresos -1)
# Ahora ingresos es una variable dummy con 2 columnas ingresos0 e ingresos1

# Se elimina la columna ingresos del dataframe original
columnas_a_eliminar = c("ingresos")
df = df[, !(names(df) %in% columnas_a_eliminar)]

# Se añade solo como columna "ingreso>50k" que es la que tienen el valor 1 si se cummple
df$ingresos = ingresos[, "ingresos>50K"]

# Se obtiene el dataframe con las variables dummies
dfdummi <- dummy.data.frame(df)
# Se renombran los nombres de la columna para no tener problemas despues
colnames(dfdummi)[which(names(dfdummi) == "raceAmer-Indian-Eskimo")] <- "raceAmerIndianEskimo"
colnames(dfdummi)[which(names(dfdummi) == "raceAsian-Pac-Islander")] <- "raceAsianPacIslander"

# Paso 3. Separacion de conjunto de entrenamiento ytest
#============================================================
# Separación conjunto de entrenamiento y de test

train <- sample(1:nrow(dfdummi),2/3*nrow(dfdummi))
df_train_dummi <- dfdummi[train,]
df_test_dummi <- dfdummi[-train,]

media <- sapply(df_train_dummi[1:6] , mean)
desviacion <- sapply(df_train_dummi[1:6],sd)

# Se realiza el conjunto normalizado, pero dentro de aquellas variables que no sean dummies columnas 1 al 6
df_train_norm = as.data.frame(scale(df_train_dummi[1:6]),center=TRUE,scale=TRUE)
sapply(df_train_norm, sd)  # obviamente la desviación es 1 para todas
sapply(df_train_norm, mean) # y la media es 0 para todas
# Unimos los valores dummies y la variable de decision
# df_train tienen las variables normalizadas y las dummies por separado 
df_train <- cbind(df_train_norm,df_train_dummi[7:14])

# Columnas 1 al 6 
df_test_norm = as.data.frame(scale(df_test_dummi[1:6]),center=TRUE,scale=TRUE)
sapply(df_test_norm, sd)  # obviamente la desviación es 1 para todas
sapply(df_test_norm, mean) # y la media es 0 para todas
# Unimos los valores dummies y la variable de decision
df_test <- cbind(df_test_norm,df_test_dummi[7:14])
#============================================================
# Tecnicas de clustering
#============================================================
#============================================================
install.packages("cluster")
install.packages("clue")
library(cluster)
library(clue)

# Elegimos subconjunto que tiene todos los elementos salvo la columna ingresos
df_train_sub = df_train[,1:13]
# Matriz distancia euclídea
mat_euclidea = dist(df_train_sub, "euclidean")
# Matriz similaridades (correlación)
mat_cor = cor(t(df_train_sub), method = "spearman")
# La convierto en tipo de dato dist
mat_cor = as.dist(1-mat_cor);

#============================================================
# 1: Clustering jerárquico

# Clustering jerárquico con linkage completo y distancia euclídea
mihclust = hclust(mat_euclidea, method = "single", members=NULL)
# Plot del dendograma
# El valor de "hang" negativo hace que las etiquetas queden al nivel 0
plot(mihclust, hang = -1)
# Cortar un dendograma según una altura 
mihclust_cortado = cutree(mihclust, h=(2/3)*max(mihclust$height)); 
# quedan 2 clusters despues del corte
# cuenta los valores que se dan en el mihclust_cortado
unique(mihclust_cortado)
#   [1] 1 2   Existen dos valores distintos
#============================================================
# 2: Clustering de repartición
mikm <- kmeans(df_train_sub, 4)

unclass(mikm)

library(clue)
nuevos_valores <- data.frame(row.names = c("1", "2"), 
                             age = c(23, 46), 
                             fnlwgt = c(62278,78022), 
                             educationnum = c(9, 1),
                             capitalgain = c(0,0), 
                             capitalloss = c(0,0), 
                             hoursperweek = c(40,40), 
                             sexFemale = c(0,1),
                             sexMale = c(1,0), 
                             raceAmerIndianEskimo = c(0,0), 
                             raceAsianPacIslander = c(0,0), 
                             raceBlack = c(0,1), 
                             raceOther = c(0,0), 
                             raceWhite = c(1,0))

# Normalizamos igual que el training set (con su media y desviación)
nuevos_valores.stand <- t((t(nuevos_valores[1:6]) - media) / desviacion)

nuevos_valores_norm <- cbind(nuevos_valores.stand,nuevos_valores[7:13])

# Cluster kmeans al que pertenecen cada uno de los nuevos estados
cl_predict(mikm, nuevos_valores_norm) 
# Class ids:
#[1] 3 2 Clusters 3 y 2 .



#============================================================
# TÉCNICAS DE PREDICCION
# 1) Arboles de decision
install.packages("rpart")
install.packages("rpart.plot")
install.packages("rattle")
install.packages("RColorBrewer")

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

# Variable de decisión: ingresos, Variables de entrada: todas las demás
# Variables continuas > anova
adulttree <- rpart(ingresos ~ ., dat = df_train, method = "anova")

# Analizamos el árbol
unclass(adulttree)

# Para mostrarlo, mejor usamos función de la librería rpart.plot
prp(adulttree)

# Pero es mucho mejor con la librería rattle
fancyRpartPlot(adulttree)

# Pasamos los ejemplos de test por el árbol para conseguir una predicción de cada posible clase
prediccion <- predict(adulttree, df_test)
# En este caso de clasificación, para cada caso simplemente vamos a seleccionar la clase con mayor confianza
prediccion2 <- predict(adulttree, df_test, type = "vector")
prediccion2
# Para obtener valores 
prediccion3 = round(prediccion2)
# Para cada caso podemos ver si el árbol ha acertado o no
aciertos = as.numeric(df_test$ingresos) == prediccion3
fallos = as.numeric(df_test$ingresos) != prediccion3

# Sacamos la proporción de aciertos
tasa_aciertos <- sum(aciertos) / nrow(df_test) 
# Sacamos la proporción de fallos
tasa_fallos <- sum(fallos) / nrow(df_test) 
# Variabla mas importante CAPITALGAIN
# Tasa_aciertos = 0.807
# Tasa_fallos = 0.193 19,3 %

#=====================================================================================

install.packages("neuralnet")
install.packages("ROCR")

library(neuralnet)
library(ROCR)

#============================================================
# Paso 2: Entrenamiento de la red neuronal A

# Variable de decisión: case, Variables de entrada: selección de 4 variables
# Algunos parámetros: 5 capa oculta con dos neuronas, error: suma del error cuadrático
nn = neuralnet(ingresos ~ age+fnlwgt+educationnum+sexFemale+sexMale+raceAmerIndianEskimo+raceAsianPacIslander+raceBlack+raceOther+raceWhite+capitalgain+capitalloss+hoursperweek,
               data = df_train, hidden = 5, err.fct = "sse", linear.output = FALSE)
plot(nn)

# Analizamos la red neuronal
unclass(nn)
# Resumen que incluye:
# error total en conjunto de entrenamiento
# número de pasos
# pesos de las conexiones entre neuronas
# error                             105.413226707040
# reached.threshold                   0.008919882723
# steps                            3071.000000000000

nn$result.matrix
# Pesos entre neuronas
nn$weights
# Salida esperada de la red para cada caso (valor real de la variable de decisión)
nn$response
# Salida de la red para cada caso (predicción)
nn$net.result

#============================================================
# Paso 3: Validación de la red neuronal con los ejemplos de test

# Sólo pasamos las variables de entrada necesarias
df_test_sel <- subset(df_test, 
                      select = c("age","fnlwgt","educationnum","sexFemale","sexMale","raceAmerIndianEskimo",
                                 "raceAsianPacIslander","raceBlack","raceOther","raceWhite","capitalgain",
                                 "capitalloss","hoursperweek"))

test_results <- compute(nn, df_test_sel)

validacion <- data.frame(salida_esperada = df_test$ingresos, 
                         salida_nn = test_results$net.result[,1])

#============================================================
# Paso 4: Gráficas de validación de modelos

library(ROCR)

pred <- prediction(validacion$salida_nn, validacion$salida_esperada)

# Cumulative Gains chart
perf_gain <- performance(pred, "tpr", "rpp")
plot(perf_gain, main="Cumulative Gains chart")
plot(perf_gain, main="Cumulative Gains chart", colorize=T)
abline(h=0.54, v=0.2, lty=2)
# Lift chart:
perf_lift <- performance(pred, "lift", "rpp")
plot(perf_lift, main="Lift curve")
plot(perf_lift, main="Lift curve", colorize=T)

# Curva ROC
perf_roc <- performance(pred,"tpr","fpr")
plot(perf_roc, main="ROC curve")
plot(perf_roc, main="ROC curve", colorize=T)

#===================================================================
#============================================================
# Paso 6: Entrenamiento de la red neuronal B

# Variable de decisión: case, Variables de entrada: selección de 4 variables
# Algunos parámetros: 1 capa oculta con dos neuronas, error: suma del error cuadrático
nn = neuralnet(ingresos ~ age+fnlwgt+educationnum+sexFemale+sexMale+capitalgain+capitalloss+hoursperweek,
               data = df_train, hidden = 5, err.fct = "sse", linear.output = FALSE)
plot(nn)

# Analizamos la red neuronal
unclass(nn)
# Resumen que incluye:
# error total en conjunto de entrenamiento
# número de pasos
# pesos de las conexiones entre neuronas
nn$result.matrix
# Pesos entre neuronas
nn$weights
# Salida esperada de la red para cada caso (valor real de la variable de decisión)
nn$response
# Salida de la red para cada caso (predicción)
nn$net.result

#============================================================
# Paso 7: Validación de la red neuronal con los ejemplos de test

# Sólo pasamos las variables de entrada necesarias
df_test_sel <- subset(df_test, 
                      select = c("age","fnlwgt","educationnum","sexFemale","sexMale"
                                 ,"capitalgain",
                                 "capitalloss","hoursperweek"))

test_results <- compute(nn, df_test_sel)

validacion <- data.frame(salida_esperada = df_test$ingresos, 
                         salida_nn = test_results$net.result[,1])

#============================================================
# Paso 8: Gráficas de validación de modelos

library(ROCR)

pred <- prediction(validacion$salida_nn, validacion$salida_esperada)

# Cumulative Gains chart
perf_gain <- performance(pred, "tpr", "rpp")
plot(perf_gain, main="Cumulative Gains chart")
plot(perf_gain, main="Cumulative Gains chart", colorize=T)

# Lift chart:
perf_lift <- performance(pred, "lift", "rpp")
plot(perf_lift, main="Lift curve")
plot(perf_lift, main="Lift curve", colorize=T)

# Curva ROC
perf_roc <- performance(pred,"tpr","fpr")
plot(perf_roc, main="ROC curve")
plot(perf_roc, main="ROC curve", colorize=T)


