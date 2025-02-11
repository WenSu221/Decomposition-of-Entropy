### US CAL female ####
## this script is based on 
## Vladimir Canudas-Romo's
## US mortality estimation for 1900-1932

D<-read.table("US Data/USData.csv",header=TRUE,fill=TRUE,skip=0,sep=",")

A<-matrix(0,41,41)
A[1,]<-D$X0[1:41]
A[31,]<-D$X30[1:41]
A[,1]<-D$X1900.2[1:41]
A[,11]<-D$X1910.2[1:41]
A[,21]<-D$X1920.2[1:41]
A[,31]<-D$X1930.2[1:41]
A[,41]<-D$X1940[1:41]

for (x in 1:41){
  A[x,x]<-D$X1900[x]
}
for (x in 1:31){
  A[x,x+10]<-D$X1910[x]
}
for (x in 1:21){
  A[x,x+20]<-D$X1920[x]
}
for (x in 1:11){
  A[x,x+30]<-D$X1930[x]
}

A[is.na(A[,1])]<-0
B<-A
B[,19]<-0

for (z in 1:31){
  N<-which(B[z,]>0)
  n<-length(N)
  for (x in 1:(n-1)){
    B[z,N[x]:N[x+1]]<-seq(B[z,N[x]],B[z,N[x+1]],length.out=((N[x+1]-N[x])+1))
  }
}

N<-which(A[,19]>0)
R<-A[N,19]/B[N,19]
RR<-rep(0,41)
for (x in 1:3){
  RR[N[x]:N[x+1]]<-seq(R[x],R[x+1],length.out=((N[x+1]-N[x])+1))
}
B[1:31,19]<-B[1:31,19]*RR[1:31]
B[N,19]<-A[N,19]


levels<-c(0,.001,0.002,0.004,0.008,0.015,0.15)
WildColors<-heat.colors(length(levels))

Year<-1900:1940
Age<-0:30
filled.contour(Year,Age,t(B[1:31,]),levels=levels,
               col=WildColors,ylab="Age",xlab="Year",cex.lab=1.2)

## transpose it to fit the HMD life table
USAmale <- rbind (B[1:31,1:32],matrix(0,80,32))

## HMD data
A11 <- read.table("US Data/USA.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1932
Y2 <- 2018

A11<-A11[(A11$Year>Y1)&(A11$Year<(Y2+1)),]

qx11<-matrix(A11$qx,111)

## combine
qx11 <- cbind(USAmale,qx11)
qx11 <- (1-qx11)

Y1 <- 1900

### function
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

## CAL
CAL11 <- c()
for (i in seq(2011,2018)){
  CAL11 <- c(CAL11, CALfunc(qx11,i))
}

## CAl dagger 
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

qx11 <- ifelse(qx11==0,1,qx11)

CALdagger11 <- c()
for (i in seq(2011,2018)){
  CALdagger11 <- c(CALdagger11, CALdagfunc(qx11,i))
}

CALdagger11 <- CALdagger11*-1

entropyCAL11 <- CALdagger11/CAL11
