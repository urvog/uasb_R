#TRABAJANDO CON CONEXIONES A BASE DE DATOS


#install.packages("RPostgreSQL)
#install.packages("RSQLite")
#install.packages("sqldf")
library(RPostgreSQL)
library(RSQLite)
library(sqldf)

#Configuramos parametros de la conexion
drv<-"PostgreSQL"
con<-dbConnect(drv, dbname="pedidos",host="127.0.0.1",port=5432,user="postgres",password="lustrabotas")

#Nos conectamos a la tabla clientes
tabla_clientes<-dbReadTable(con, "clientes")

#Mostramos información de la tabla
str(tabla_clientes)
summary(tabla_clientes)

#Crear una consulta
sql<-dbGetQuery(con,"select a.descripcion as producto, b.nombreprov as proveedor, c.nombrecat as categoria, preciounit as precio, existencia
from productos a join proveedores b on a.proveedorid = b.proveedorid
           join categorias c on a.categoriaid = c.categoriaid")

#mostramos los datos de la consulta 
str(sql)
summary(sql)

d_sql<-data.frame(producto = sql$producto, prooveedor = sql$proveedor, categoria = sql$categoria, precio = sql$precio, existencia = sql$existencia)
str(d_sql)



dbDisconnect(con)
dev.off()
