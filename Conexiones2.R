##comprobamos si existe la carpeta data
if (!file.exists("data")){
    dir.create("data")
}
##descargar datos
fileUrl<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile = "./data/cameras.csv")

##importar datos en tabla
cameraData<-read.table("./data/cameras.csv", sep = ",", header = TRUE)
head(cameraData)

##importar datos excel
fileUrl<-"https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.xlsx")
library(xlsx)
cameraData<-read.xlsx("./data/cameras.xlsx", sheetIndex=1, header=TRUE)
head(cameraData)
colindex<-2:3
rowindex<-1:4
cameraDataSubset<-read.xlsx("./data/cameras.xlsx",sheetIndex=1,colIndex=colindex,rowIndex=rowindex)

##importar datos XML
library(XML)
fileUrl<-"http://www.w3schools.com/xml/simple.xml"
doc<-xmlTreeParse(fileUrl,useInternal=TRUE)
rootNode<-xmlRoot(doc)
xmlName(rootNode)

rootNode[[1]]

xmlSApply(rootNode,xmlValue)
xpathSApply(rootNode,"//price",xmlValue)

##importar datos ESPN via XML
fileUrl<-"http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc<-htmlTreeParse(fileUrl,useInternal=TRUE)
scores<-xpathSApply(doc,"//li[@class='score']",xmlValue)
teams<-xpathSApply(doc,"//li[@class='team-name']",xmlValue)
scores

##importar datos JSON
library(jsonlite)
jsonData<-fromJSON("https://api.github.com/users/urvog/repos")
names(jsonData)
names(jsonData$owner)
jsonData$owner$html_url

myjson<-toJSON(iris,pretty=TRUE)
cat(myjson)
iris2<-fromJSON(myjson)
head(iris2)


##datatable package
library(data.table)
DF<-data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)

DT<-data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)

##mostrar todas las tables en memoria
tables()

DT[,list(mean(x),sum(z))]
DT[,table(y)]

##anadir columna
DT[,w:=z*x]

DT1<-data.table(x=c('a','a','b','dt1'),y=1:4)
DT2<-data.table(x=c('a','b','dt2'),z=5:7)
setkey(DT1,x)
setkey(DT2,x)
merge(DT1,DT2)
