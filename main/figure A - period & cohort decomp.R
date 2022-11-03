
####
#### Period and cohort decomposition of CAL entropy
#### 


library(DemoDecomp)
library(data.table)
library(RColorBrewer)

### CAL entropy function ####
CALentropy <-function(Mx1,Y){
  Mx1<-matrix(Mx1,nrow=111)
  CALlx1<-c()
  CALdaglx1<-c()
  YM<-Y-Y1
  for (x in 1:111){
      px1<-c()
      for (z in 1:x){
        px1<-c(px1,Mx1[z,YM-x+z])
      }
      lx1<-prod(px1)
      lx1 <- fifelse(lx1==0|is.na(lx1),1,lx1)
      lx<-lx1*log(lx1)
    CALlx1<-c(CALlx1,lx1)
    CALdaglx1<-c(CALdaglx1,lx)
  }
  CAL<-sum(c(1,CALlx1))+.5
  CALdagger<-sum(CALdaglx1)*-1
  return(CALdagger/CAL)
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

Y1 <- 1879
Y2 <- 1990

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

qx1<-1-A1$qx
qx2<-1-A2$qx
qx3<-1-A3$qx
qx4<-1-A4$qx
qx5<-1-A5$qx
qx6<-1-A6$qx
qx7<-1-A7$qx
qx8<-1-A8$qx
qx9<-1-A9$qx
qx10<-1-A10$qx

avg <- colMeans(rbind(qx1,qx2,qx3,qx4,qx5,
                      qx6,qx7,qx8,qx9,qx10))

HCAL1 <- CALentropy(avg,Y2)
HCAL2 <- CALentropy(qx7,Y2)
round(HCAL2-HCAL1,5)

# direction is 2-1 so ITA - AVG
results <- stepwise_replacement(CALentropy,avg,qx7,Y=Y2)

results <- fifelse(results==0,NaN,results)
results <- matrix(results,nrow=111)

colnames(results) <- seq(Y1+1,Y2,1)

mypalette<-rev(brewer.pal(8,"YlGnBu"))
mypalette2<-rev(brewer.pal(8,"YlOrRd"))
WildColors<-c(mypalette[1:4],"white","white",
              mypalette2[c(6,4,2,1)])
WildColors<-c(WildColors[1:4],"white","white",
              WildColors[7:10])

# the different levels of the Z values
levels<-c(-0.001,-0.0001,-1e-05,-1e-06,-1e-07,
          0,1e-07,1e-06,1e-05,.0001,.001)

customAxis <- function() { 
  n <- length(levels) 
  y <- seq(min(levels), max(levels), length.out=n) 
  rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors) 
  axis(4, at=y, labels=levels) 
} 

pdf("Output/Age & Cohort Decomposition, ITA-AVG, 1990.pdf",
    width = 8,height = 6)

filled.contour(x=seq(Y1+1,Y2,1),y=c(0:110),z=t(results),
               levels = levels,col = WildColors,
               key.axes = customAxis(),
               xlab="Year",ylab="Age")
title(main="Age- & Cohort- Decomposition, \n ITA male-Average, 1990",
      sub = paste("ITA - AVG difference: ", round(HCAL2-HCAL1,5), sep=""),
      font.main=2)

dev.off()

write.table(results,"Data/ITA age-cohort decomp 1990.txt")
