#### The comparision of different measures ####

library(RColorBrewer)
library(segmented)
COL <- adjustcolor(c("orangered","forestgreen","purple"),
                   alpha.f = 0.8)

source("US Data/USCAL male.R")

#### CAL functions ####
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


### CAL dagger function ####

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


### Data fitting ####

A1 <- read.table("Data/SWE.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A10 <- read.table("Data/CHE.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1877
Y2 <- 2018

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

qx1<-matrix(1-A1$qx,111)
qx2<-matrix(1-A2$qx,111)
qx3<-matrix(1-A3$qx,111)
qx4<-matrix(1-A4$qx,111)
qx5<-matrix(1-A5$qx,111)
qx6<-matrix(1-A6$qx,111)
qx7<-matrix(1-A7$qx,111)
qx8<-matrix(1-A8$qx,111)
qx9<-matrix(1-A9$qx,111)
qx10<-matrix(1-A9$qx,111)

## CAL ####

CAL1 <- c()
for (i in seq(1989,2018,2)){
  CAL1 <- c(CAL1, CALfunc(qx1,i))
}

CAL2 <- c()
for (i in seq(1989,2018,2)){
  CAL2 <- c(CAL2, CALfunc(qx2,i))
}

CAL3 <- c()
for (i in seq(1989,2018,2)){
  CAL3 <- c(CAL3, CALfunc(qx3,i))
}

CAL4 <- c()
for (i in seq(1989,2018,2)){
  CAL4 <- c(CAL4, CALfunc(qx4,i))
}

CAL5 <- c()
for (i in seq(1989,2018,2)){
  CAL5 <- c(CAL5, CALfunc(qx5,i))
}

CAL6 <- c()
for (i in seq(1989,2018,2)){
  CAL6 <- c(CAL6, CALfunc(qx6,i))
}

CAL7 <- c()
for (i in seq(1989,2018,2)){
  CAL7 <- c(CAL7, CALfunc(qx7,i))
}

CAL8 <- c()
for (i in seq(1989,2018,2)){
  CAL8 <- c(CAL8, CALfunc(qx8,i))
}

CAL9 <- c()
for (i in seq(1989,2018,2)){
  CAL9 <- c(CAL9, CALfunc(qx9,i))
}

CAL10 <- c()
for (i in seq(1989,2018,2)){
  CAL10 <- c(CAL10, CALfunc(qx10,i))
}

## CAL dagger ####

qx1 <- ifelse(qx1==0,1,qx1)
qx2 <- ifelse(qx2==0,1,qx2)
qx3 <- ifelse(qx3==0,1,qx3)
qx4 <- ifelse(qx4==0,1,qx4)
qx5 <- ifelse(qx5==0,1,qx5)
qx6 <- ifelse(qx6==0,1,qx6)
qx7 <- ifelse(qx7==0,1,qx7)
qx8 <- ifelse(qx8==0,1,qx8)
qx9 <- ifelse(qx9==0,1,qx9)
qx10 <- ifelse(qx10==0,1,qx10)

CALdagger1 <- c()
for (i in seq(1989,2018,2)){
  CALdagger1 <- c(CALdagger1, CALdagfunc(qx1,i))
}

CALdagger2 <- c()
for (i in seq(1989,2018,2)){
  CALdagger2 <- c(CALdagger2, CALdagfunc(qx2,i))
}

CALdagger3 <- c()
for (i in seq(1989,2018,2)){
  CALdagger3 <- c(CALdagger3, CALdagfunc(qx3,i))
}

CALdagger4 <- c()
for (i in seq(1989,2018,2)){
  CALdagger4 <- c(CALdagger4, CALdagfunc(qx4,i))
}

CALdagger5 <- c()
for (i in seq(1989,2018,2)){
  CALdagger5 <- c(CALdagger5, CALdagfunc(qx5,i))
}

CALdagger6 <- c()
for (i in seq(1989,2018,2)){
  CALdagger6 <- c(CALdagger6, CALdagfunc(qx6,i))
}

CALdagger7 <- c()
for (i in seq(1989,2018,2)){
  CALdagger7 <- c(CALdagger7, CALdagfunc(qx7,i))
}

CALdagger8 <- c()
for (i in seq(1989,2018,2)){
  CALdagger8 <- c(CALdagger8, CALdagfunc(qx8,i))
}

CALdagger9 <- c()
for (i in seq(1989,2018,2)){
  CALdagger9 <- c(CALdagger9, CALdagfunc(qx9,i))
}

CALdagger10 <- c()
for (i in seq(1989,2018,2)){
  CALdagger10 <- c(CALdagger10, CALdagfunc(qx10,i))
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
CALdagger10 <- CALdagger10*-1

## entropy ####

entropyCAL1 <- CALdagger1/CAL1
entropyCAL2 <- CALdagger2/CAL2
entropyCAL3 <- CALdagger3/CAL3
entropyCAL4 <- CALdagger4/CAL4
entropyCAL5 <- CALdagger5/CAL5
entropyCAL6 <- CALdagger6/CAL6
entropyCAL7 <- CALdagger7/CAL7
entropyCAL8 <- CALdagger8/CAL8
entropyCAL9 <- CALdagger9/CAL9
entropyCAL10 <- CALdagger10/CAL10

#### e0 functions ####

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


### e dagger function ####

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
A11 <- read.table("US data/USA.mltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

## e ####

e01<-LE(A1,1878,2018)
e02<-LE(A2,1878,2018)
e03<-LE(A3,1878,2018)
e04<-LE(A4,1878,2018)
e05<-LE(A5,1878,2018)
e06<-LE(A6,1878,2018)
e07<-LE(A7,1878,2018)
e08<-LE(A8,1878,2018)
e09<-LE(A9,1878,2018)
e10<-LE(A10,1878,2018)
e11<-LE(A11,1933,2018)

## e dagger # ----
edagger1 <- c()
for (i in seq(1878,2018,1)){
  edagger1 <- c(edagger1, LSV(A1,i))
}

edagger2 <- c()
for (i in seq(1878,2018,1)){
  edagger2 <- c(edagger2, LSV(A2,i))
}

edagger3 <- c()
for (i in seq(1878,2018,1)){
  edagger3 <- c(edagger3, LSV(A3,i))
}

edagger4 <- c()
for (i in seq(1878,2018,1)){
  edagger4 <- c(edagger4, LSV(A4,i))
}

edagger5 <- c()
for (i in seq(1878,2018,1)){
  edagger5 <- c(edagger5, LSV(A5,i))
}

edagger6 <- c()
for (i in seq(1878,2018,1)){
  edagger6 <- c(edagger6, LSV(A6,i))
}

edagger7 <- c()
for (i in seq(1878,2018,1)){
  edagger7 <- c(edagger7, LSV(A7,i))
}

edagger8 <- c()
for (i in seq(1878,2018,1)){
  edagger8 <- c(edagger8, LSV(A8,i))
}

edagger9 <- c()
for (i in seq(1878,2018,1)){
  edagger9 <- c(edagger9, LSV(A9,i))
}

edagger10 <- c()
for (i in seq(1878,2018,1)){
  edagger10 <- c(edagger10, LSV(A10,i))
}

edagger11 <- c()
for (i in seq(1933,2018,1)){
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

#### ec0 entropy ####

source("main/figure 2 - entropy of ec0 male.R")


#### average ####

eavg <- rbind(entropye01,entropye02,entropye03,entropye04,
              entropye05,entropye06,entropye07,entropye08,
              entropye09,entropye10)
eavg <- colMeans(eavg)

cavg <- rbind(entropyc1,entropyc2,entropyc3,entropyc4,
              entropyc5,entropyc6,entropyc7,entropyc8,
              entropyc9,entropyc10)
cavg <- colMeans(cavg[,1:49])

CALavg <- rbind(entropyCAL1,entropyCAL2,entropyCAL3,
                entropyCAL4,entropyCAL5,entropyCAL6,
                entropyCAL7,entropyCAL8,entropyCAL9,
                entropyCAL10)
CALavg <- colMeans(CALavg)


### Plot ####
png(file = "Output/Measures Comparison (regressed), male 1878-2018.png",
    width = 11, height = 8,units="in",res=300)

years1 <- c(1878:2018)
years2 <- c(seq(1989,2018,2))
years3 <- c(1878:1926)
years6 <- c(seq(1989,2018,2))
years7 <- c(seq(2011,2018,1))
years8 <- c(1933:2018)
#Linear Models

CALlm <- lm(CALavg~c(1:15))
CALcoefs <- coef(CALlm)
CALslope <-  round(CALcoefs[2],5)*100
CALr2 <- round(summary(CALlm)$r.squared,2)
CALeqn <- bquote("CAL:" ~~ slope == .(CALslope)~~"%")

clm <- lm(cavg~c(1:49))
ccoefs <- coef(clm)
cslope <- round(ccoefs[2],5)*100
cr2 <- round(summary(clm)$r.squared,2)
ceqn <- bquote("cohort:" ~~ slope == .(cslope)~~"%")

elm <- lm(eavg~c(1:141))
elm <- segmented(elm,seg.Z =~years1)
ecoefs <- coef(elm)
eslope1 <-  round(ecoefs[2],5)*100
eslope2 <-  round(ecoefs[3],5)*100
er2 <- round(summary(elm)$r.squared,2)
eeqn1 <- bquote("period 1878~1959" ~~ slope == .(eslope1)~~"%")
eeqn2 <- bquote("period 1960~2018" ~~ slope == .(eslope1+eslope2)~~"%")

plot(range(years1),c(0,1),xlab = "Years",ylab = "Entropy", col=0)
lines(years2,entropyCAL1,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL2,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL3,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL4,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL5,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL6,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL7,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL8,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL9,col=COL[1],lty=3,lwd=2)
lines(years2,entropyCAL10,col=COL[1],lty=3,lwd=2)
lines(years7,entropyCAL11,col=COL[1],lty=3,lwd=2)
lines(years2,CALavg,col="red",lwd = 3)
lines(years6,predict(CALlm),col=1,lwd=1.5)
text(1995,0.25,CALeqn,col="red",cex=1.1)
lines(years1,entropye01,col=COL[2],lty=3,lwd=2)
lines(years1,entropye02,col=COL[2],lty=3,lwd=2)
lines(years1,entropye03,col=COL[2],lty=3,lwd=2)
lines(years1,entropye04,col=COL[2],lty=3,lwd=2)
lines(years1,entropye05,col=COL[2],lty=3,lwd=2)
lines(years1,entropye06,col=COL[2],lty=3,lwd=2)
lines(years1,entropye07,col=COL[2],lty=3,lwd=2)
lines(years1,entropye08,col=COL[2],lty=3,lwd=2)
lines(years1,entropye09,col=COL[2],lty=3,lwd=2)
lines(years1,entropye10,col=COL[2],lty=3,lwd=2)
lines(years8,entropye11,col=COL[2],lty=3,lwd=2)
lines(years1,eavg,col="forestgreen",lwd = 3)
lines(years1,predict(elm),col=1,lwd=1.5)
text(1950,0.11,eeqn1,col="forestgreen",cex=1.1)
text(1950,0.08,eeqn2,col="forestgreen",cex=1.1)
text(1950,0.05,"piecewise value: 1957.134",col="forestgreen",cex=1.1)
lines(years3,entropyc1,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc2,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc3,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc4,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc5,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc6,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc7,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc8,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc9,col=COL[3],lty=3,lwd=2)
lines(years3,entropyc10,col=COL[3],lty=3,lwd=2)
lines(years3,cavg,col="purple",lwd = 3)
lines(years3,predict(clm),col=1,lwd=1.5)
text(1900,0.25,ceqn,col="purple",cex=1.1)
title("Figure 2B. Time trends in period, cohort, and CAL entropy for male in selected populations, 1878-2018")
legend("topright"
       ,c("average CAL entropy",
          "CAL entropy",
          "average period entropy",
          "period entropy",
          "average cohort entropy",
          "cohort entropy",
          "best fitted line"),
       col = c("red",COL[1],"forestgreen",COL[2],"purple",COL[3],"black"),
       lty = c(1,3,1,3,1,3,1),
       lwd = c(3,2,3,2,3,2,1.5),cex = 1.1,box.lty = 0)
dev.off()