### load packages ####
library(MortalityLaws)

### Life table
x <- c(0:99)
LT <- LifeTable(x,mx = rep(0.0535,100))

px <- 1-(LT$lt$qx)

lx <- c()
for (i in 1:100){
  lx <- c(lx,prod(px[1:i]))
}

plot(c(0,100),c(0,1))
lines(c(0:99),lx)

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

plot(c(0,100),c(0,1))
lines(c(0:99),CALlx)

Hcal <- -sum(lx[1:99]*log(lx[1:99]))/sum(lx[1:99])
