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
        col = colores[sample(1:655,3)])
text(bp, y = d_clase+20,labels = d_clase, xpd = TRUE)
title(main = "Cantidad de personas por clase", xlab = "Clases", ylab = "Frecuencia")

#una simple torta
torta<-pie(d_clase, c("1ra Clase","2 Clase","3ra Clase"), col = colores[sample(1:655,3)] )
title(main = "Distribucion de personas por clase")


## Promedios edad por clase
subtotal_edad<-aggregate(Age~Pclass, DataTitanicValido,mean)
bp<-barplot(subtotal_edad$Age, names.arg = subtotal_edad$Pclass, col = colores[sample(1:655,3)])
text(bp, y = round(subtotal_edad$Age,2)+4, labels = round(subtotal_edad$Age,2), xpd = TRUE)
title(main = "Promedios edad por clase", xlab = "Clases", ylab = "Promedio edad")

#Plot de edad vs costo ticket
plot(DataTitanicValido$Age,DataTitanicValido$Fare, col = colores[sample(1:655,2)], pch = 19, main = "Edad vs Costo Ticket",
     xlab = "Edad", ylab = "Costo Ticket")
abline(lm(DataTitanicValido$Age~DataTitanicValido$Fare), col = "red")

#Plot Hermanos vs Padres e hijos 
bp<-plot(DataTitanicValido$Fare,DataTitanicValido$Edad, col = colores[sample(1:655,2)], pch = 19, main = "Tickets vs Edad",
     xlab = "Costo Tickets", ylab = "Edad", ylim = c(0,100))

pairs(~DataTitanicValido$Age+DataTitanicValido$SibSp+DataTitanicValido$Fare, data = DataTitanicValido)

##libreria lattice
#plot ticket vs edad agrupados por Pclass
xyplot(Fare~Age|Pclass, data = DataTitanicValido, xlab = "Costo Ticket", ylab = "Edad", main = "Graficos Lattice")
xyplot(Fare~Age|Embarked, data = DataTitanicValido, xlab = "Costo Ticket", ylab = "Edad", main = "Graficos Lattice")
dev.off()
