#### Average of CAL vs. CAL by average ASMR 

library(dplyr)

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

#### Life table construction ####

### We use Swedish unsmoothed mortality rates ###
Mx <- read.table("Data/Mx_1x1.txt",header = T,skip = 2)

Mx <- Mx %>% group_by(Year) %>% 
  filter(Year>= 1878&Year<=2017) %>%
  select(c(Year,Age,Female)) %>%
  ungroup()


### Ax ###
Ax <- read.table("Data/SWE.fltper_1x1.txt",header = T,skip = 2)
Ax <- Ax %>% group_by(Year) %>% 
  filter(Year>= 1878&Year<=2017) %>%
  select(ax) %>% 
  ungroup() 

Mx <- cbind(Mx,Ax[,2])
Mx[,3] <- as.numeric(Mx[,3])

### We fit in the life expectancy function ###
Mx$qx <- Mx$Female  / ( 1 + ((1-Mx$ax) * Mx$Female))
qx <- matrix(1-Mx$qx,111)
qx[is.na(qx)] <- 0

Y1 <- 1877
Y2 <- 2017

CALqx <- c()
for (i in seq(1989,2017,2)){
  CALqx <- c(CALqx, CALfunc(qx,i))
}

qx <- ifelse(qx<=0,1,qx)

CALdagger<- c()
for (i in seq(1989,2017,2)){
  CALdagger <- c(CALdagger, CALdagfunc(qx,i))
}

entropy <- (CALdagger*-1)/CALqx

#### Sweden Level ####
A1 <- read.table("Data/SWE.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1877
Y2 <- 2017

A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]
qx1<-matrix(1-A1$qx,111)

CAL1 <- c()
for (i in seq(1989,2017,2)){
  CAL1 <- c(CAL1, CALfunc(qx1,i))
}

qx1 <- ifelse(qx1==0,1,qx1)

CALdagger1 <- c()
for (i in seq(1989,2017,2)){
  CALdagger1 <- c(CALdagger1, CALdagfunc(qx1,i))
}

CALdagger1 <- CALdagger1*-1

entropyCAL1 <- CALdagger1/CAL1

#### Comparison ####
entropyCAL1 - entropy