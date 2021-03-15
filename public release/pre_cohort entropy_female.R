#### figure 2 entropy of cohort life expectancy ####

#### CLE ####


### Data # ----
A1 <- read.table("Data/SWE.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A10 <- read.table("Data/CHE.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1877
Y2 <- 1926

A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]
A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A3<-A3[(A3$Year>Y1)&(A3$Year<(Y2+1)),]
A4<-A4[(A4$Year>Y1)&(A4$Year<(Y2+1)),]
A5<-A5[(A5$Year>Y1)&(A5$Year<(Y2+1)),]
A6<-A6[(A6$Year>Y1)&(A6$Year<(Y2+1)),]
A7<-A7[(A7$Year>Y1)&(A7$Year<(Y2+1)),]
A8<-A8[(A8$Year>Y1)&(A8$Year<(Y2+1)),]
A9<-A9[(A9$Year>Y1)&(A9$Year<(Y2+1)),]
A10<-A10[(A10$Year>Y1)&(A10$Year<(Y2+1)),]

### e dagger # ----

LSV <- function(dt,Y){
  Ydt<-subset(dt, Year==Y)
  age_length_equal <- all.equal(length(Ydt$age),length(Ydt$dx),
                                length(Ydt$lx),length(Ydt$ex),
                                length(Ydt$ax))
  stopifnot(age_length_equal)
  n <- c(diff(Ydt$age),1)
  explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$age)])
  ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
  A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
  return(A[1])
}


###e0 function # ----

LE <- function(dt, Y1, Y2){
  e0<-c()
  for (i in Y1:Y2){
    e<-subset(dt,Year==i)
    ei<-e[1,10]
    e0<-c(e0,ei)
  }
  return(e0)
}


### Data time! # ----
ec01<-LE(A1,1878,1926)
ec01 <- as.numeric(ec01)
ec02<-LE(A2,1878,1926)
ec02 <- as.numeric(ec02)
ec03<-LE(A3,1878,1926)
ec03 <- as.numeric(ec03)
ec04<-LE(A4,1878,1926)
ec04 <- as.numeric(ec04)
ec05<-LE(A5,1878,1926)
ec05 <- as.numeric(ec05)
ec06<-LE(A6,1878,1926)
ec06 <- as.numeric(ec06)
ec07<-LE(A7,1878,1926)
ec07 <- as.numeric(ec07)
ec08<-LE(A8,1878,1926)
ec08 <- as.numeric(ec08)
ec09<-LE(A9,1878,1926)
ec09 <- as.numeric(ec09)
ec10<-LE(A10,1878,1926)
ec10 <- as.numeric(ec10)


A1 <- as.data.frame(sapply(A1, as.numeric))
A1[is.na(A1)]<-0
ed1 <- c()
for (i in 1878:1926){
  ed1 <- c(ed1, LSV(A1,i))
}

A2 <- as.data.frame(sapply(A2, as.numeric))
A2[is.na(A2)]<-0
ed2 <- c()
for (i in 1878:1926){
  ed2 <- c(ed2, LSV(A2,i))
}

A3 <- as.data.frame(sapply(A3, as.numeric))
A3[is.na(A3)]<-0
ed3 <- c()
for (i in 1878:1926){
  ed3 <- c(ed3, LSV(A3,i))
}

A4 <- as.data.frame(sapply(A4, as.numeric))
A4[is.na(A4)]<-0
ed4 <- c()
for (i in 1878:1926){
  ed4 <- c(ed4, LSV(A4,i))
}

A5 <- as.data.frame(sapply(A5, as.numeric))
A5[is.na(A5)]<-0
ed5 <- c()
for (i in 1878:1926){
  ed5 <- c(ed5, LSV(A5,i))
}

A6 <- as.data.frame(sapply(A6, as.numeric))
A6[is.na(A6)]<-0
ed6 <- c()
for (i in 1878:1926){
  ed6 <- c(ed6, LSV(A6,i))
}

A7 <- as.data.frame(sapply(A7, as.numeric))
A7[is.na(A7)]<-0
ed7 <- c()
for (i in 1878:1926){
  ed7 <- c(ed7, LSV(A7,i))
}

A8 <- as.data.frame(sapply(A8, as.numeric))
A8[is.na(A8)]<-0
ed8 <- c()
for (i in 1878:1926){
  ed8 <- c(ed8, LSV(A8,i))
}

A9 <- as.data.frame(sapply(A9, as.numeric))
A9[is.na(A9)]<-0
ed9 <- c()
for (i in 1878:1926){
  ed9 <- c(ed9, LSV(A9,i))
}

A10 <- as.data.frame(sapply(A10, as.numeric))
A10[is.na(A10)]<-0
ed10 <- c()
for (i in 1878:1926){
  ed10 <- c(ed10, LSV(A10,i))
}

### H # ----

entropyc1 <- ed1/ec01
entropyc2 <- ed2/ec02
entropyc3 <- ed3/ec03
entropyc4 <- ed4/ec04
entropyc5 <- ed5/ec05
entropyc6 <- ed6/ec06
entropyc7 <- ed7/ec07
entropyc8 <- ed8/ec08
entropyc9 <- ed9/ec09
entropyc10 <- ed10/ec10

entropyctable <- cbind(entropyc1,entropyc2,entropyc3,
                       entropyc4,entropyc5,entropyc6,
                       entropyc7,entropyc8,entropyc9,
                       entropyc10)
colnames(entropyctable) <- c("SWE","DNK","FRATNP","GBRTENW","NOR",
                             "FIN","ITA","GBRSCO","NLD","CHE")
row.names(entropyctable) <- c(1878:1926)
# write.csv(entropyctable, file = "Output/entropyctable_female.CSV")