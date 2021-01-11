#### Figure - entropy across time male 1989-2017 ####

#### beautiful figues1 ####

library(RColorBrewer)
# display.brewer.all()
cols<-brewer.pal(n=9,name = "Set1")

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

## CAL ####

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

CALavg <- rbind(entropyCAL1,entropyCAL2,entropyCAL3,
                entropyCAL4,entropyCAL5,entropyCAL6,
                entropyCAL7,entropyCAL8,entropyCAL9)
CALavg <- colMeans(CALavg)


#### Figure ####

Years <- seq(1989,2017,4)
png(file = "Output/Countries Comparison log scale, male 1989-2017.png",
    units = "in", width = 6, height = 8, res = 200)
plot(c(1989,2017),rev(c(0.11,0.24)),col = 0,
     xlab = "Years",ylab = "entropy of CAL (log scale)", 
     log = "y")
lines(Years,entropyCAL1,col = cols[1],lty=5,lwd = 2)
lines(Years,entropyCAL2,col = cols[2],pch=2,lty=5,lwd = 2)
lines(Years,entropyCAL3,col = cols[3],lty=5,lwd = 2)
lines(Years,entropyCAL4,col = cols[4],pch=3,lty=5,lwd = 2)
lines(Years,entropyCAL5,col = cols[5],pch=2,lty=5,lwd = 2)
lines(Years,entropyCAL6,col = cols[6],lty=6,lwd = 2)
lines(Years,entropyCAL7,col = cols[7],pch=3,lty=6,lwd = 2)
lines(Years,entropyCAL8,col = cols[8],lty=6,lwd = 2)
lines(Years,entropyCAL9,col = cols[9],lty=6,lwd = 2)
lines(Years,CALavg,col = "grey",pch = 16,lty = 1,lwd = 2)
title("comparison of entropy of CAL across countries, male 1989-2017")
legend("topright",c("Sweden","Denmark","France","England and Wales",
                    "Norway","Finland","Italy","Scotland","Netherland","average"),
       col = c(cols[1:9]),lty = c(5,5,5,5,5,6,6,6,6,1),
       box.col = 0)
dev.off()

### CALavg as the benchmark ####

diff1 <- entropyCAL1 - CALavg
diff2 <- entropyCAL2 - CALavg
diff3 <- entropyCAL3 - CALavg
diff4 <- entropyCAL4 - CALavg
diff5 <- entropyCAL5 - CALavg
diff6 <- entropyCAL6 - CALavg
diff7 <- entropyCAL7 - CALavg
diff8 <- entropyCAL8 - CALavg
diff9 <- entropyCAL9 - CALavg


### Plots ####

Years <- seq(1989,2017,4)
png(file = "Output/Countries Comparison average benchmark, male 1989-2017.png",
    units = "in", width = 7, height = 9, res = 300)
plot(c(1989,2017),c(-0.025,0.04),col = 0,
     xlab = "Years",
     ylab="differences from average level of entropy of CAL")
lines(Years,diff1,type = "l",
      col = "black",lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff2,type = "l",
      col = cols[2],lty=1,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff3,type = "l",
      col = cols[3],lty=4,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff4,type = "l",
      col = cols[4],lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff5,type = "l",
      col = cols[5],lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff6,type = "l",
      col = cols[1],lty=1,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff7,type = "l",
      col = cols[7],lty=4,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff8,type = "l",
      col = cols[8],lty=4,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff9,type = "l",
      col = cols[9],lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,rep(0,times=8),
      col = 1, lty = 3, lwd = 2,
      cex = 1,pch = 20)
legend("topright",c("Sweden","Denmark","France",
                    "England and Wales","Norway",
                    "Finland","Italy","Scotland","Netherland"),
       col = c("black",cols[2:5],cols[1],cols[7:9]),
       lty = c(2,1,4,2,2,1,4,4,2,3),box.col = 0)
title("Entropy of CAL compared to average level, male 1989-2017")
mtext("average level", side = 2,adj = 0.4)
dev.off()