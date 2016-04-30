#Practica en curso con lenguaje R

##verificamos que nos encontramos en el directorio de trabajo uasb_R/ obtenido del repositorio
#en github y listamos los archivos
getwd()
dir()

#cargamos los datos a una variable de tipo data.frame
DataTitanic<-read.table("titanic.csv", sep = ",", header = TRUE)

#obtenemos algunas estadísticas de los datos obtenidos
str(DataTitanic)
summary(DataTitanic)

#verificamos si tenemos datos faltantes (missing values)
table(is.na(DataTitanic))
summary(DataTitanic)

#analizamos cada columna para revisar cual tiene valores vacios
table(is.na(DataTitanic$PassengerId)) #no tiene valores vacios
table(is.na(DataTitanic$Survived)) #no tiene valores vacios
table(is.na(DataTitanic$PassengerId))
table(is.na(DataTitanic$PassengerId))

#cargamos todos los valores faltantes de la columna Age
faltantes<-is.na(DataTitanic$Age)

DataTitanicValido<-DataTitanic[!faltantes,]
DataTitanicNoValido<-DataTitanic[faltantes,]

#nuevamente obtenemos informacion de valores faltantes con el nuevo dataset
table(is.na(DataTitanicValido))
summary(DataTitanicValido)

#transformamos el atributo Pclass de numero a Factor
DataTitanicValido<-transform(DataTitanicValido, Pclass = factor(Pclass))
str(DataTitanicValido)

#comenzamos a mostrar algunos graficos de interes

#histogramas
colores<-colors() #para generar aleatoriamente colores con la funcion sample

##Cantidad de pasajeros x clase
d_clase<-matrix(table(DataTitanicValido$Pclass))
bp<-barplot(d_clase[,1], names.arg = c("1ra Clase", "2da Clase", "3ra Clase"), 
        col = colores[sample(1:657,3)])
text(bp, y = d_clase+20,labels = d_clase, xpd = TRUE)
title(main = "Cantidad de personas por clase", xlab = "Clases", ylab = "Frecuencia")

#una simple torta
torta<-pie(d_clase, c("1ra Clase","2da Clase","3ra Clase"), col = colores[sample(1:655,3)] )
title(main = "Distribucion de personas por clase")


## Promedios edad por clase
subtotal_edad<-aggregate(Age~Pclass, DataTitanicValido,mean)
bp<-barplot(subtotal_edad$Age, names.arg = subtotal_edad$Pclass, col = colores[sample(1:655,3)])
text(bp, y = round(subtotal_edad$Age,2)+4, labels = round(subtotal_edad$Age,2), xpd = TRUE)
title(main = "Promedios edad por clase", xlab = "Clases", ylab = "Promedio edad")

#Plot de edad vs costo ticket
plot(DataTitanicValido$Age,DataTitanicValido$Fare, col = colores[sample(1:655,2)], pch = c(7,8), main = "Edad vs Costo Ticket",
     xlab = "Edad", ylab = "Costo Ticket")
abline(lm(DataTitanicValido$Age~DataTitanicValido$Fare), col = "black")
abline(h = 200, col ="red")
abline(v = 70, col = "blue")

#Plot Hermanos vs Padres e hijos 
bp<-plot(DataTitanicValido$Fare,DataTitanicValido$Parch, col = colores[sample(1:655,2)], pch = 19, main = "Tickets vs Edad",
     xlab = "Costo Tickets", ylab = "Parch", ylim = c(0,6))

pairs(~DataTitanicValido$Age+DataTitanicValido$SibSp+DataTitanicValido$Fare, data = DataTitanicValido)

##libreria lattice
library(lattice)
#plot ticket vs edad agrupados por Pclass
xyplot(Fare~Age|Survived, data = DataTitanicValido, xlab = "Edad", ylab = "Costo Ticket", main = "Graficos Lattice")
xyplot(Fare~Age|Embarked, data = DataTitanicValido, xlab = "Costo Ticket", ylab = "Edad", main = "Graficos Lattice")
barchart(Age~Survived|Embarked, data = DataTitanicValido, groups = Pclass, xlab = "Costo Ticket", ylab = "Edad",
         main = "Graficos Lattice", auto.key = TRUE)
dev.off()


##libreria GGPlot
library(ggplot2)

#graficos basicos de qplot
qplot(Fare,Age, data = DataTitanicValido, color = Embarked)

#Lineas de tendencia
qplot(Fare,Age, data = DataTitanicValido, color = Pclass, geom = c("point","smooth"))

#grafico multidimensional fare,Age coloreado por clase y agrupado por lugar de embarque
qplot(Fare,Age, data = DataTitanicValido, color = Pclass, facets = .~Embarked, main = "Grafico GGPLOT")

#histograma
qplot(Age, data = DataTitanicValido, fill = Pclass, facets = .~Embarked, main = "Grafico GGPLOT", binwidth = 5)

#GUARDANDO IMAGENES DESDE CODIGO
png(file = "plot1.png",width = 800, height = 800)
qplot(Fare,Age, data = DataTitanicValido, color = Pclass, facets = Embarked~., main = "Grafico GGPLOT")
dev.off()

#MODELANDO CON CLUSTERS ALGORITMO KMEANS

#iniciamos creando subconjuntos de datos los cuales queremos analizar (edad vs costo ticket)
d_cluster<-data.frame(edad = DataTitanicValido$Age, costo_ticket = DataTitanicValido$Fare)

library(rpart)
d_kmeans<-kmeans(d_cluster,centers = 4, iter.max = 100)

#agregamos la columna con los datos del cluster
d_cluster$cluster<-d_kmeans$cluster

#mostramos informacion de los datos del cluster
head(d_kmeans)
head(d_cluster)

#graficamos los puntos y los centroides utilizando el atributo "centers:"
png(file = "Cluster_Kmeans.png",width = 800, height = 800)
plot(d_cluster$edad,d_cluster$costo_ticket, col = d_kmeans$cluster, main = "Clustering con K-means",
     xlab = "Edad", ylab = "Costo Ticket")
points(d_kmeans$centers, pch = 8, cex = 2)
dev.off()
write.table(d_cluster,"titanic_K-means.csv", sep = ",", row.names = FALSE)

#kmeans con 3 variables
library(scatterplot3d)
d_cluster<-data.frame(edad = DataTitanicValido$Age, 
                      costo_ticket = DataTitanicValido$Fare,
                      padres_hijos = DataTitanicValido$Parch)

d_kmeans<-kmeans(d_cluster,centers = 4, iter.max = 100)

#agregamos la columna con los datos del cluster
d_cluster$cluster<-d_kmeans$cluster

#mostramos informacion de los datos del cluster
head(d_kmeans)
head(d_cluster)

png(file = "Cluster_Kmeans3D.png",width = 800, height = 800)
#graficamos los datos en plano 3D
scatterplot3d(d_cluster$edad,d_cluster$costo_ticket,d_cluster$padres_hijos, color = d_kmeans$cluster,
              main = "Clustering con K-Means", xlab = "Edad", ylab = "Costo Ticket", zlab = "Padres&Hijos",
              type  = "p")
dev.off()

##MODELOS PREDICTIVOS CON ARBOLES DE DECISION
#cargamos la libreria para construccion de modelos con arboles decision
library(rpart)
m_arbol<-rpart(Survived~Sex + Age + Pclass, data = DataTitanicValido, method = "class")

#mostramos lo generado por el modelo
m_arbol
printcp(m_arbol)

#graficamos el arbol
png(file = "Arbol_baseplot.png",width = 800, height = 800)
par(mfrow = c(1,1), mar = c(2,2,2,2))
plot(m_arbol, uniform = TRUE)
text(m_arbol, use.n=TRUE, all=TRUE, cex=.8)
dev.off()

#graficando con mejores librerias
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("RColorBrewer")

library(rattle)
library(rpart.plot)
library(RColorBrewer)

png(file = "Arbol_rattle.png",width = 800, height = 800)

#graficar arbol
fancyRpartPlot(m_arbol)
dev.off()

#PREDICCIONES 

#abrimos el archivo que contiene los datos que queremos predecir
DataTitanicPredict<-read.table("titanic_predict.csv", sep = ",", header = TRUE)
#transformamos los valores
DataTitanicPredict<-transform(DataTitanicPredict, Pclass = factor(Pclass))

prediccion<-predict(m_arbol,DataTitanicPredict, type = "class")
#mostramos los datos de prediccion
head(prediccion)

#unimos los datos resultado a la tabla de prediccion
DataTitanicPredict$prediccion<-prediccion
#mostramos los valores
head(DataTitanicPredict,10)

#guardamos los datos predecidos
write.table(DataTitanicPredict,"titanic_predict_tree.csv", sep = ",", row.names = FALSE)
