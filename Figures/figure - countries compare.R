#### Figure - entropy across time ####

#### beautiful figues1 ####

library(RColorBrewer)
# display.brewer.all()
cols<-brewer.pal(n=10,name = "Paired")

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

CALavg1 <- rbind(entropyCAL1[1:12],entropyCAL2[1:12],entropyCAL3[1:12],
                 entropyCAL4[1:12],entropyCAL5[1:12])
CALavg1 <- colMeans(CALavg1[,1:12])

CALavg2 <- rbind(entropyCAL1[13:21],entropyCAL2[13:21],entropyCAL3[13:21],
                 entropyCAL4[13:21],entropyCAL5[13:21],
                 entropyCAL6,entropyCAL7,entropyCAL8,entropyCAL9)
CALavg2 <- colMeans(CALavg2[,1:9])

CALavg <- c(CALavg1,CALavg2)

#### Figure ####

Years <- seq(1957,2017,3)
Years2 <- seq(1992,2017,3)
windows(12,8)
plot(c(1957,2017),rev(c(0.12,0.35)),col = 0, xlab = "Years",ylab = "entropy of CAL (log scale)", log = "y")
lines(Years,entropyCAL1,type = "b",col = 2,lty=5,lwd=1.5)
lines(Years,entropyCAL2,type = "b",col = 3,pch=2,lty=5,lwd=1.5)
lines(Years,entropyCAL3,type = "b",col = 5,lty=5,lwd=1.5)
lines(Years,entropyCAL4,type = "b",col = 5,pch=3,lty=5,lwd=1.5)
lines(Years,entropyCAL5,type = "b",col = 6,pch=2,lty=5,lwd=1.5)
lines(Years2,entropyCAL6,type = "b",col = 7,lty=6,lwd=1.5)
lines(Years2,entropyCAL7,type = "b",col = 8,pch=3,lty=6,lwd=1.5)
lines(Years2,entropyCAL8,type = "b",col = 9,lty=6,lwd=1.5)
lines(Years2,entropyCAL9,type = "b",col = 10,lty=6,lwd=1.5)
lines(Years2,CALavg2, type = "o",col = "grey",pch = 16,lty = 1,lwd = 2)
title("comparison of entropy of CAL across countries, total 1957-2017")
legend("topright",c("Sweden","Denmark","France","England and Wales",
                    "Norway","Finland","Italy","Scotland","Netherland","average"),
       col = c(2,3,4,5,6,7,8,9,10,"grey"),lty = c(5,5,5,5,5,6,6,6,6,1),
       box.col = 0)

### CALavg as the benchmark ####

diff1 <- entropyCAL1[13:21] - CALavg2
diff2 <- entropyCAL2[13:21] - CALavg2
diff3 <- entropyCAL3[13:21] - CALavg2
diff4 <- entropyCAL4[13:21] - CALavg2
diff5 <- entropyCAL5[13:21] - CALavg2
diff6 <- entropyCAL6 - CALavg2
diff7 <- entropyCAL7 - CALavg2
diff8 <- entropyCAL8 - CALavg2
diff9 <- entropyCAL9 - CALavg2


### Plots ####

windows(20,12)
Years <- seq(1957,2017,3)
Years2 <- seq(1992,2017,3)

plot(c(1992,2017),c(-0.025,0.04),col = 0,
     xlab = "Years",
     ylab="differences from average level of entropy of CAL, 1992-2017")
lines(Years2,diff1,col = cols[2],lty=1,lwd=1.5)
lines(Years2,diff2,col = cols[3],lty=1,lwd=1.5)
lines(Years2,diff3,col = cols[4],lty=1,lwd=1.5)
lines(Years2,diff4,col = cols[5],lty=1,lwd=1.5)
lines(Years2,diff5,col = cols[6],lty=1,lwd=1.5)
lines(Years2,diff6,col = cols[7],lty=1,lwd=1.5)
lines(Years2,diff7,col = cols[8],lty=1,lwd=1.5)
lines(Years2,diff8,col = cols[9],lty=1,lwd=1.5)
lines(Years2,diff9,col = cols[10],lty=1,lwd=1.5)
lines(Years2,rep(0,times=9),col = 1, lty = 2, lwd = 1)
legend("topright",c("Sweden","Denmark","France",
                    "England and Wales","Norway",
                    "Finland","Italy","Scotland","Netherland"),
       col = cols[2:10],lty = rep(1,times=9),box.col = 0)
title("Entropy of CAL compared to average level, total 1957-2017")
mtext("average level 1992-2017", side = 2,adj = 0.3)
