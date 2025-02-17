#### Figure - entropy across time male 1989-2018 ####

#### beautiful figues1 ####

library(RColorBrewer)
# display.brewer.all()
cols<-brewer.pal(n=10,name = "Paired")

source("US Data/USCAL male.R")

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
qx10<-matrix(1-A10$qx,111)

## CAL ####

CAL1 <- c()
for (i in seq(1989,2018)){
  CAL1 <- c(CAL1, CALfunc(qx1,i))
}

CAL2 <- c()
for (i in seq(1989,2018)){
  CAL2 <- c(CAL2, CALfunc(qx2,i))
}

CAL3 <- c()
for (i in seq(1989,2018)){
  CAL3 <- c(CAL3, CALfunc(qx3,i))
}

CAL4 <- c()
for (i in seq(1989,2018)){
  CAL4 <- c(CAL4, CALfunc(qx4,i))
}

CAL5 <- c()
for (i in seq(1989,2018)){
  CAL5 <- c(CAL5, CALfunc(qx5,i))
}

CAL6 <- c()
for (i in seq(1989,2018)){
  CAL6 <- c(CAL6, CALfunc(qx6,i))
}

CAL7 <- c()
for (i in seq(1989,2018)){
  CAL7 <- c(CAL7, CALfunc(qx7,i))
}

CAL8 <- c()
for (i in seq(1989,2018)){
  CAL8 <- c(CAL8, CALfunc(qx8,i))
}

CAL9 <- c()
for (i in seq(1989,2018)){
  CAL9 <- c(CAL9, CALfunc(qx9,i))
}

CAL10 <- c()
for (i in seq(1989,2018)){
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
for (i in seq(1989,2018)){
  CALdagger1 <- c(CALdagger1, CALdagfunc(qx1,i))
}

CALdagger2 <- c()
for (i in seq(1989,2018)){
  CALdagger2 <- c(CALdagger2, CALdagfunc(qx2,i))
}

CALdagger3 <- c()
for (i in seq(1989,2018)){
  CALdagger3 <- c(CALdagger3, CALdagfunc(qx3,i))
}

CALdagger4 <- c()
for (i in seq(1989,2018)){
  CALdagger4 <- c(CALdagger4, CALdagfunc(qx4,i))
}

CALdagger5 <- c()
for (i in seq(1989,2018)){
  CALdagger5 <- c(CALdagger5, CALdagfunc(qx5,i))
}

CALdagger6 <- c()
for (i in seq(1989,2018)){
  CALdagger6 <- c(CALdagger6, CALdagfunc(qx6,i))
}

CALdagger7 <- c()
for (i in seq(1989,2018)){
  CALdagger7 <- c(CALdagger7, CALdagfunc(qx7,i))
}

CALdagger8 <- c()
for (i in seq(1989,2018)){
  CALdagger8 <- c(CALdagger8, CALdagfunc(qx8,i))
}

CALdagger9 <- c()
for (i in seq(1989,2018)){
  CALdagger9 <- c(CALdagger9, CALdagfunc(qx9,i))
}

CALdagger10 <- c()
for (i in seq(1989,2018)){
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

CALavg <- rbind(entropyCAL1,entropyCAL2,entropyCAL3,
                entropyCAL4,entropyCAL5,entropyCAL6,
                entropyCAL7,entropyCAL8,entropyCAL9,
                entropyCAL10)
CALavg <- colMeans(CALavg)



### CALavg as the benchmark ####

diff1 <- entropyCAL1 /CALavg
diff2 <- entropyCAL2 / CALavg
diff3 <- entropyCAL3 / CALavg
diff4 <- entropyCAL4 / CALavg
diff5 <- entropyCAL5 / CALavg
diff6 <- entropyCAL6 / CALavg
diff7 <- entropyCAL7 / CALavg
diff8 <- entropyCAL8 / CALavg
diff9 <- entropyCAL9 / CALavg
diff10 <- entropyCAL10 / CALavg
diff11 <- entropyCAL11 / CALavg[23:30]
diff11 <- c(rep(NA,22),diff11)

### Plots ####

Years <- seq(1989,2018)
pdf(file = "Output/Countries Comparison average benchmark_rel, male 1989-2018.pdf",
    width = 8, height = 8)
par(mar=c(5,5,5,12),xpd = T)
plot(c(1989,2018),c(0.8,1.2),col = 0,
     xlab = "Years",
     ylab="Differences between populations and average CAL Entropy",
     ylim = c(0.8,1.2))
lines(Years,diff1,type = "l",
      col = "black",lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff2,type = "l",
      col = cols[2],lty=1,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff3,type = "l",
      col = "darkorange4",lty=4,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff4,type = "l",
      col = "lightgreen",lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff5,type = "l",
      col = cols[5],lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff6,type = "l",
      col = cols[1],lty=4,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff7,type = "l",
      col = "forestgreen",lty=4,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff8,type = "l",
      col = cols[8],lty=5,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff9,type = "l",
      col = cols[9],lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff10,type = "l",
      col = cols[10],lty=2,lwd = 2,
      cex = 1,pch = 20)
lines(Years,diff11,type = "l",
      col = "red",lty=5,lwd=2)
lines(Years,rep(0,times=15),
      col = 1, lty = 3, lwd = 2,
      cex = 1,pch = 20)
legend("right", inset=-0.5,c("England & Wales","Netherlands","Norway",
                             "Sweden","Switzerland","Denmark",
                             "USA","Scotland","Finland","France",
                             "Italy"),
       col=c("lightgreen",cols[9],cols[5],"black",cols[10],cols[2],
             "red",cols[8],cols[1],"darkorange4","forestgreen"),
       lty = c(2,2,2,2,2,1,5,5,4,4,4),
       box.col = 0)
title("Figure A1B. Entropy of CAL compared to average level (ratio), male 1989-2018")
mtext("average level", side = 2,adj = 0.4)
dev.off()