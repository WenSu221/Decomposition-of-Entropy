### load packages ####
library(MortalityLaws)

#### Theoretical ####

### Period Life Table

x <- c(0:99)
LT <- LifeTable(x,mx = rep(0.05,100))

px <- 1-(LT$lt$qx)
lx <- c()

for (x in 1:100){
  Sx <- c(px[1:x])
  lx <- c(lx,prod(Sx))
}


Hp <- -sum(lx[1:99]*log(lx[1:99]))/sum(lx[1:99])

### CAL

CALpx <- matrix(px,nrow = 100,ncol=100)

CALlx <- c()
for (x in 1:100){
  Sx <- c()
  for (i in 1:x){
    Sx <- c(Sx,CALpx[i,100-x+i])
  }
  CALlx <- c(CALlx,prod(Sx))
}

Hcal <- -sum(CALlx[1:99]*log(CALlx[1:99]))/sum(CALlx[1:99])

plot(c(1,100),c(0,1))
lines(c(1:100),CALlx, col = "red4")
lines(c(0:99),lx, col = "forestgreen")

#### empirical with real data ####

### Vaupel and Canudas-Romo method

x <- c(0:99)
LT <- LifeTable(x,mx = rep(0.05,100))

px <- 1-(LT$lt$qx)
CALpx <- matrix(px,nrow = 100,ncol=100)
lx <- c()

for (x in 1:100){
  Sx <- c(px[1:x])
  lx <- c(lx,prod(Sx))
}

lx <- c(1,lx[1:99])

edagger <- sum(-(lx*log(lx)))

ezero <- sum(lx)-1
HpVC <- edagger/ezero

CALfunc <-function(Mx1,YM){
  CALlx1<-c()
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
CALdagfunc <-function(Mx1,YM){
  CALdaglx1<-c()
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

CAL <- CALfunc(CALpx,100)
CALpx <- ifelse(CALpx==0,1,CALpx)
CALdagger <- -CALdagfunc(CALpx,100)
HcalVC <- CALdagger/CAL

y <-c()
e_x <- function(x,mu){
  y <- c(y,exp(1)^(-mu*x))
}

value <- e_x(1:100,0.5)

plot(c(1,100),c(0,1))
lines(c(1:100),value)

y <-c()
plot(c(1,100),c(0,1))
for (a in seq(0,1,0.01)){
  value <- e_x(1:100,a)
  lines(c(1:100),value)
}

