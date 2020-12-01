#### figure 2 entropy of CAL ####

### CAL function # ----

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


### CAL dagger function # ----

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


### Data fitting # ----

A1<-read.table("Data/SWE.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1845
Y2 <- 2018

A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]

qx1<-matrix(1-A1$qx,111)


## CAL #

CAL <- c()
for (i in seq(1957,2017,5)){
  CAL <- c(CAL, CALfunc(qx1,i))
}

## CAL dagger #

qx1 <- ifelse(qx1==0,1,qx1)

CALdagger <- c()
for (i in seq(1957,2017,5)){
  CALdagger <- c(CALdagger, CALdagfunc(qx1,i))
}
CALdagger <- CALdagger*-1

entropyCAL <- CALdagger/CAL

write.table(entropyCAL,file = "Output/HSWE.txt")
