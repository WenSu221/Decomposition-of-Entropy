#### CAL average ####

### CAL function ####

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

## one with less data ####

CALfunc2 <-function(Mx1,Y){
  CALlx1<-c()
  YM<-Y-Y3
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

## one with less data ####

CALdagfunc2 <-function(Mx1,Y){
  CALdaglx1<-c()
  YM<-Y-Y3
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

A1 <- read.table("Data/SWE.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRATNP.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBRTENW.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/NOR.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A6 <- read.table("Data/FIN.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A7 <- read.table("Data/ITA.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A8 <- read.table("Data/GBRSCO.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A9 <- read.table("Data/NLD.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1845
Y2 <- 2017
Y3 <- 1880

A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]
A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A3<-A3[(A3$Year>Y1)&(A3$Year<(Y2+1)),]
A4<-A4[(A4$Year>Y1)&(A4$Year<(Y2+1)),]
A5<-A5[(A5$Year>Y1)&(A5$Year<(Y2+1)),]
A6<-A6[(A6$Year>Y3)&(A6$Year<(Y2+1)),]
A7<-A7[(A7$Year>Y3)&(A7$Year<(Y2+1)),]
A8<-A8[(A8$Year>Y3)&(A8$Year<(Y2+1)),]
A9<-A9[(A9$Year>Y3)&(A9$Year<(Y2+1)),]

qx1<-matrix(1-A1$qx,111)
qx2<-matrix(1-A2$qx,111)
qx3<-matrix(1-A3$qx,111)
qx4<-matrix(1-A4$qx,111)
qx5<-matrix(1-A5$qx,111)
qx6<-matrix(1-A6$qx,111)
qx7<-matrix(1-A7$qx,111)
qx8<-matrix(1-A8$qx,111)
qx9<-matrix(1-A9$qx,111)

## CAL ####

CAL1 <- c()
for (i in seq(1957,2017,3)){
  CAL1 <- c(CAL1, CALfunc(qx1,i))
}

CAL2 <- c()
for (i in seq(1957,2017,3)){
  CAL2 <- c(CAL2, CALfunc(qx2,i))
}

CAL3 <- c()
for (i in seq(1957,2017,3)){
  CAL3 <- c(CAL3, CALfunc(qx3,i))
}

CAL4 <- c()
for (i in seq(1957,2017,3)){
  CAL4 <- c(CAL4, CALfunc(qx4,i))
}

CAL5 <- c()
for (i in seq(1957,2017,3)){
  CAL5 <- c(CAL5, CALfunc(qx5,i))
}

CAL6 <- c()
for (i in seq(1992,2017,3)){
  CAL6 <- c(CAL6, CALfunc2(qx6,i))
}

CAL7 <- c()
for (i in seq(1992,2017,3)){
  CAL7 <- c(CAL7, CALfunc2(qx7,i))
}

CAL8 <- c()
for (i in seq(1992,2017,3)){
  CAL8 <- c(CAL8, CALfunc2(qx8,i))
}

CAL9 <- c()
for (i in seq(1992,2017,3)){
  CAL9 <- c(CAL9, CALfunc2(qx9,i))
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

CALdagger1 <- c()
for (i in seq(1957,2017,3)){
  CALdagger1 <- c(CALdagger1, CALdagfunc(qx1,i))
}

CALdagger2 <- c()
for (i in seq(1957,2017,3)){
  CALdagger2 <- c(CALdagger2, CALdagfunc(qx2,i))
}

CALdagger3 <- c()
for (i in seq(1957,2017,3)){
  CALdagger3 <- c(CALdagger3, CALdagfunc(qx3,i))
}

CALdagger4 <- c()
for (i in seq(1957,2017,3)){
  CALdagger4 <- c(CALdagger4, CALdagfunc(qx4,i))
}

CALdagger5 <- c()
for (i in seq(1957,2017,3)){
  CALdagger5 <- c(CALdagger5, CALdagfunc(qx5,i))
}

CALdagger6 <- c()
for (i in seq(1992,2017,3)){
  CALdagger6 <- c(CALdagger6, CALdagfunc2(qx6,i))
}

CALdagger7 <- c()
for (i in seq(1992,2017,3)){
  CALdagger7 <- c(CALdagger7, CALdagfunc2(qx7,i))
}

CALdagger8 <- c()
for (i in seq(1992,2017,3)){
  CALdagger8 <- c(CALdagger8, CALdagfunc2(qx8,i))
}

CALdagger9 <- c()
for (i in seq(1992,2017,3)){
  CALdagger9 <- c(CALdagger9, CALdagfunc2(qx9,i))
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

CALHavg1 <- rbind(entropyCAL1[1:12],entropyCAL2[1:12],entropyCAL3[1:12],
                 entropyCAL4[1:12],entropyCAL5[1:12])
CALHavg1 <- colMeans(CALHavg1[,1:12])

CALHavg2 <- rbind(entropyCAL1[13:21],entropyCAL2[13:21],entropyCAL3[13:21],
                 entropyCAL4[13:21],entropyCAL5[13:21],
                 entropyCAL6,entropyCAL7,entropyCAL8,entropyCAL9)
CALHavg2 <- colMeans(CALHavg2[,1:9])

#### CAL dagger average and CAL average ####
CALdavg2 <- rbind(CALdagger1[13:21],CALdagger2[13:21],CALdagger3[13:21],
                  CALdagger4[13:21],CALdagger5[13:21],
                  CALdagger6,CALdagger7,CALdagger8,CALdagger9)
CALdavg2 <- colMeans(CALdavg2[,1:9])

CALavg2 <- rbind(CAL1[13:21],CAL2[13:21],CAL3[13:21],
                 CAL4[13:21],CAL5[13:21],
                 CAL6,CAL7,CAL8,CAL9)
CALavg2 <- colMeans(CALavg2[,1:9])
