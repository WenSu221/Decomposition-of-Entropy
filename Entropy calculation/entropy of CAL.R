#### entropy of CAL cross-national decomposition ####

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

##cahnge the data!
A1 <- read.table("Data/DNK.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/GBRTENW.bltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

##change the years!
Y1 <- 1845
Y2 <- 2018

A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]
A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]

qx1<-matrix(1-A1$qx,111)
qx2<-matrix(1-A2$qx,111)


## CAL ####

CAL1 <- c()
for (i in seq(1957,2017,5)){
  CAL1 <- c(CAL1, CALfunc(qx1,i))
}

CAL2 <- c()
for (i in seq(1957,2017,5)){
  CAL2 <- c(CAL2, CALfunc(qx2,i))
}

## CAL dagger ####

qx1 <- ifelse(qx1==0,1,qx1)
qx2 <- ifelse(qx2==0,1,qx2)

CALdagger1 <- c()
for (i in seq(1957,2017,5)){
  CALdagger1 <- c(CALdagger1, CALdagfunc(qx1,i))
}

CALdagger2 <- c()
for (i in seq(1957,2017,5)){
  CALdagger2 <- c(CALdagger2, CALdagfunc(qx2,i))
}

CALdagger1 <- CALdagger1*-1
CALdagger2 <- CALdagger2*-1

## entropy ####

entropyCAL1 <- CALdagger1/CAL1
entropyCAL2 <- CALdagger2/CAL2

## decomp ####

entropyavg <- (entropyCAL1+entropyCAL2)/2
entropydiff <- entropyCAL2-entropyCAL1

dispersion <- log(CALdagger2/CALdagger1)
measure <- log(CAL2/CAL1)

inequality <- dispersion*entropyavg
longevity <- -(measure*entropyavg)

# inequality <- dispersion*-1/(measure+dispersion*-1)
# longevity <- measure/(measure+dispersion*-1)
# all <- inequality+longevity
