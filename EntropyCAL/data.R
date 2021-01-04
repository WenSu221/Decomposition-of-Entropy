#### Data Preparation ####

### CAL ####

#### CAL functions ###
CALfunc <-function(Mx1,Y){
  CALlx1<-c()
  YM<-Y-Y1
  for (x in 1:111){
    if (x <(YM+1)){
      px1<-c()
      for (z in 1:x){
        px1<-c(px1,Mx1[z,YM-x+z])
      }
      lx1<-prod(px1)
    }
    CALlx1<-c(CALlx1,lx1)
  }
  CAL<-sum(c(1,CALlx1))+.5
  return(CAL)
}


### CAL dagger function ###

CALdagfunc <-function(Mx1,Y){
  CALdaglx1<-c()
  YM<-Y-Y1
  for (x in 1:111){
    if (x <(YM+1)){
      px1<-c()
      for (z in 1:x){
        px1<-c(px1,Mx1[z,YM-x+z])
      }
      lx1<-prod(px1)
      lx<-lx1*log(lx1)
    }
    CALdaglx1<-c(CALdaglx1,lx)
  }
  CALdagger<-sum(CALdaglx1)
  return(CALdagger)
}


### Data fitting ###

A1 <- read.table("Data/SWE.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1877
Y2 <- 2017

A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]
A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A3<-A3[(A3$Year>Y1)&(A3$Year<(Y2+1)),]
A4<-A4[(A4$Year>Y1)&(A4$Year<(Y2+1)),]
A5<-A5[(A5$Year>Y1)&(A5$Year<(Y2+1)),]
A6<-A6[(A6$Year>Y1)&(A6$Year<(Y2+1)),]
A7<-A7[(A7$Year>Y1)&(A7$Year<(Y2+1)),]
A8<-A8[(A8$Year>Y1)&(A8$Year<(Y2+1)),]
A9<-A9[(A9$Year>Y1)&(A9$Year<(Y2+1)),]

qx1<-matrix(1-A1$qx,111)
qx2<-matrix(1-A2$qx,111)
qx3<-matrix(1-A3$qx,111)
qx4<-matrix(1-A4$qx,111)
qx5<-matrix(1-A5$qx,111)
qx6<-matrix(1-A6$qx,111)
qx7<-matrix(1-A7$qx,111)
qx8<-matrix(1-A8$qx,111)
qx9<-matrix(1-A9$qx,111)

## CAL ###

CAL1 <- c()
for (i in seq(1989,2017,4)){
  CAL1 <- c(CAL1, CALfunc(qx1,i))
}

CAL2 <- c()
for (i in seq(1989,2017,4)){
  CAL2 <- c(CAL2, CALfunc(qx2,i))
}

CAL3 <- c()
for (i in seq(1989,2017,4)){
  CAL3 <- c(CAL3, CALfunc(qx3,i))
}

CAL4 <- c()
for (i in seq(1989,2017,4)){
  CAL4 <- c(CAL4, CALfunc(qx4,i))
}

CAL5 <- c()
for (i in seq(1989,2017,4)){
  CAL5 <- c(CAL5, CALfunc(qx5,i))
}

CAL6 <- c()
for (i in seq(1989,2017,4)){
  CAL6 <- c(CAL6, CALfunc(qx6,i))
}

CAL7 <- c()
for (i in seq(1989,2017,4)){
  CAL7 <- c(CAL7, CALfunc(qx7,i))
}

CAL8 <- c()
for (i in seq(1989,2017,4)){
  CAL8 <- c(CAL8, CALfunc(qx8,i))
}

CAL9 <- c()
for (i in seq(1989,2017,4)){
  CAL9 <- c(CAL9, CALfunc(qx9,i))
}
## CAL dagger ###

qx1 <- ifelse(qx1==0,1,qx1)
qx2 <- ifelse(qx2==0,1,qx2)
qx3 <- ifelse(qx3==0,1,qx3)
qx4 <- ifelse(qx4==0,1,qx4)
qx5 <- ifelse(qx5==0,1,qx5)
qx6 <- ifelse(qx6==0,1,qx6)
qx7 <- ifelse(qx7==0,1,qx7)
qx8 <- ifelse(qx8==0,1,qx8)
qx9 <- ifelse(qx9==0,1,qx9)

CALdagger1 <- c()
for (i in seq(1989,2017,4)){
  CALdagger1 <- c(CALdagger1, CALdagfunc(qx1,i))
}

CALdagger2 <- c()
for (i in seq(1989,2017,4)){
  CALdagger2 <- c(CALdagger2, CALdagfunc(qx2,i))
}

CALdagger3 <- c()
for (i in seq(1989,2017,4)){
  CALdagger3 <- c(CALdagger3, CALdagfunc(qx3,i))
}

CALdagger4 <- c()
for (i in seq(1989,2017,4)){
  CALdagger4 <- c(CALdagger4, CALdagfunc(qx4,i))
}

CALdagger5 <- c()
for (i in seq(1989,2017,4)){
  CALdagger5 <- c(CALdagger5, CALdagfunc(qx5,i))
}

CALdagger6 <- c()
for (i in seq(1989,2017,4)){
  CALdagger6 <- c(CALdagger6, CALdagfunc(qx6,i))
}

CALdagger7 <- c()
for (i in seq(1989,2017,4)){
  CALdagger7 <- c(CALdagger7, CALdagfunc(qx7,i))
}

CALdagger8 <- c()
for (i in seq(1989,2017,4)){
  CALdagger8 <- c(CALdagger8, CALdagfunc(qx8,i))
}

CALdagger9 <- c()
for (i in seq(1989,2017,4)){
  CALdagger9 <- c(CALdagger9, CALdagfunc(qx9,i))
}

CALdagger1 <- CALdagger1*-1
CALdagger2 <- CALdagger2*-1
CALdagger3 <- CALdagger3*-1
CALdagger4 <- CALdagger4*-1
CALdagger5 <- CALdagger5*-1
CALdagger6 <- CALdagger6*-1
CALdagger7 <- CALdagger7*-1
CALdagger8 <- CALdagger8*-1
CALdagger9 <- CALdagger9*-1

## entropy ###

entropyCAL1 <- CALdagger1/CAL1
entropyCAL2 <- CALdagger2/CAL2
entropyCAL3 <- CALdagger3/CAL3
entropyCAL4 <- CALdagger4/CAL4
entropyCAL5 <- CALdagger5/CAL5
entropyCAL6 <- CALdagger6/CAL6
entropyCAL7 <- CALdagger7/CAL7
entropyCAL8 <- CALdagger8/CAL8
entropyCAL9 <- CALdagger9/CAL9

entropy <- c(entropyCAL1,entropyCAL2,entropyCAL3,entropyCAL4,
                     entropyCAL5,entropyCAL6,entropyCAL7,entropyCAL8,
                     entropyCAL9)
country <- c(rep("SWE",8),rep("DNK",8),
             rep("FRATNP",8),rep("GBRTENW",8),
             rep("NOR",8),rep("FIN",8),
             rep("ITA",8),rep("GBRSCO",8),rep("NLD",8))
measure <- c(rep("CAL",72))
year <- c(rep(seq(1989,2017,4),9))
entropytable <- cbind(country,year,measure,entropy)

#### e0 ####

#### e0 functions ###

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


### e dagger function ###

LE <- function(dt, Y1, Y2){
  
  for (i in Y1:Y2){
    e<-subset(A1,Year==i)
    ei<-e[1,10]
    e0<-c(e0,ei)
  }
  return(e0)
}

e0<-c()

### data fitting ###

A1 <- read.table("Data/SWE.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)


## e ###

e01<-LE(A1,1878,2017)
e02<-LE(A2,1878,2017)
e03<-LE(A3,1878,2017)
e04<-LE(A4,1878,2017)
e05<-LE(A5,1878,2017)
e06<-LE(A6,1878,2017)
e07<-LE(A7,1878,2017)
e08<-LE(A8,1878,2017)
e09<-LE(A9,1878,2017)

## e dagger #
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

entropye01 <- edagger1/e01
entropye02 <- edagger2/e02
entropye03 <- edagger3/e03
entropye04 <- edagger4/e04
entropye05 <- edagger5/e05
entropye06 <- edagger6/e06
entropye07 <- edagger7/e07
entropye08 <- edagger8/e08
entropye09 <- edagger9/e09

entropy_of_e0 <- c(entropye01,entropye02,entropye03,entropye04,
                    entropye05,entropye06,entropye07,entropye08,
                    entropye09)
country <- c(rep("SWE",140),rep("DNK",140),
             rep("FRATNP",140),rep("GBRTENW",140),
             rep("NOR",140),rep("FIN",140),
             rep("ITA",140),rep("GBRSCO",140),rep("NLD",140))
measure <- c(rep("e0",1260))
year <- c(rep(c(1878:2017),9))
entropye0table <- cbind(country,year,measure,entropy_of_e0)
entropytable <- rbind(entropytable,entropye0table)


#### ec0 ####

### Data #
A1 <- read.table("Data/SWE.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.mltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)

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

### e dagger #

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


###e0 function #

LE <- function(dt, Y1, Y2){
  e0<-c()
  for (i in Y1:Y2){
    e<-subset(dt,Year==i)
    ei<-e[1,10]
    e0<-c(e0,ei)
  }
  return(e0)
}


### Data time! #
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

### H #

entropyc1 <- ed1/ec01
entropyc2 <- ed2/ec02
entropyc3 <- ed3/ec03
entropyc4 <- ed4/ec04
entropyc5 <- ed5/ec05
entropyc6 <- ed6/ec06
entropyc7 <- ed7/ec07
entropyc8 <- ed8/ec08
entropyc9 <- ed9/ec09

entropy_of_ec0 <- c(entropyc1,entropyc2,entropyc3,entropyc4,
                   entropyc5,entropyc6,entropyc7,entropyc8,
                   entropyc9)
country <- c(rep("SWE",49),rep("DNK",49),
             rep("FRATNP",49),rep("GBRTENW",49),
             rep("NOR",49),rep("FIN",49),
             rep("ITA",49),rep("GBRSCO",49),rep("NLD",49))
measure <- c(rep("ec0",441))
year <- rep(c(1878:1926),9)
entropyec0table <- cbind(country,year,measure,entropy_of_ec0)
entropytable <- rbind(entropytable,entropyec0table)
entropytable <- as.data.frame(entropytable)

write.csv(entropytable, "EntropyCAL/entropy_table.csv", 
          row.names = F)
