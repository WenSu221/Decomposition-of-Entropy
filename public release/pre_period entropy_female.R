#### period entropy ####
########################

#### e^dagger functions ####

LSV <- function(dt,Y){
  Ydt<-subset(dt, Year==Y)
  age_length_equal <- all.equal(length(Ydt$age),length(Ydt$dx),length(Ydt$lx),length(Ydt$ex),length(Ydt$ax))
  stopifnot(age_length_equal)
  n <- c(diff(Ydt$age),1)
  explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$age)])
  ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
  A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
  return(A[1])
}


### e0 function ####

LE <- function(dt, Y1, Y2){
  
  for (i in Y1:Y2){
    e<-subset(A1,Year==i)
    ei<-e[1,10]
    e0<-c(e0,ei)
  }
  return(e0)
}

e0<-c()

### data fitting ####

A1 <- read.table("Data/SWE.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A10 <- read.table("Data/CHE.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A11 <- read.table("US data/USA.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

## e ####

e01<-LE(A1,1878,2017)
e02<-LE(A2,1878,2017)
e03<-LE(A3,1878,2017)
e04<-LE(A4,1878,2017)
e05<-LE(A5,1878,2017)
e06<-LE(A6,1878,2017)
e07<-LE(A7,1878,2017)
e08<-LE(A8,1878,2017)
e09<-LE(A9,1878,2017)
e10<-LE(A10,1878,2017)
e11<-LE(A11,1933,2017)

## e dagger # ----
edagger1 <- c()
for (i in seq(1878,2017,1)){
  edagger1 <- c(edagger1, LSV(A1,i))
}

edagger2 <- c()
for (i in seq(1878,2017,1)){
  edagger2 <- c(edagger2, LSV(A2,i))
}

edagger3 <- c()
for (i in seq(1878,2017,1)){
  edagger3 <- c(edagger3, LSV(A3,i))
}

edagger4 <- c()
for (i in seq(1878,2017,1)){
  edagger4 <- c(edagger4, LSV(A4,i))
}

edagger5 <- c()
for (i in seq(1878,2017,1)){
  edagger5 <- c(edagger5, LSV(A5,i))
}

edagger6 <- c()
for (i in seq(1878,2017,1)){
  edagger6 <- c(edagger6, LSV(A6,i))
}

edagger7 <- c()
for (i in seq(1878,2017,1)){
  edagger7 <- c(edagger7, LSV(A7,i))
}

edagger8 <- c()
for (i in seq(1878,2017,1)){
  edagger8 <- c(edagger8, LSV(A8,i))
}

edagger9 <- c()
for (i in seq(1878,2017,1)){
  edagger9 <- c(edagger9, LSV(A9,i))
}

edagger10 <- c()
for (i in seq(1878,2017,1)){
  edagger10 <- c(edagger10, LSV(A10,i))
}

edagger11 <- c()
for (i in seq(1933,2017,1)){
  edagger11 <- c(edagger11, LSV(A11,i))
}

entropye01 <- edagger1/e01
entropye02 <- edagger2/e02
entropye03 <- edagger3/e03
entropye04 <- edagger4/e04
entropye05 <- edagger5/e05
entropye06 <- edagger6/e06
entropye07 <- edagger7/e07
entropye08 <- edagger8/e08
entropye09 <- edagger9/e09
entropye10 <- edagger10/e10
entropye11 <- edagger11/e11

entropye0table <- cbind(entropye01,entropye02,entropye03,entropye04,
                        entropye05,entropye06,entropye07,entropye08,
                        entropye09,entropye10)
colnames(entropye0table) <- c("SWE","DNK","FRATNP","GBRTENW","NOR",
                              "FIN","ITA","GBRSCO","NLD","CHE")
row.names(entropye0table) <- c(1878:2017)
# write.csv(entropye0table, file = "Output/entropye0table_female.CSV")
