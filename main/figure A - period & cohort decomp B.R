
###
### 
###

library(DemoDecomp)
library(data.table)
library(RColorBrewer)

Confunc <-function(Mx){
  Con<-c()
  YM<-111
  for (x in 1:111){
    px1<-c()
    for (z in 1:x){
      px1<-c(px1,Mx[z,YM-x+z])
      lx1<-sum(px1)
    }
    Con<-c(Con,lx1)
  }
  return(Con)
}

### Data fitting ####

data1 <- as.matrix(read.table("Data/ITA age-cohort decomp 1990.txt")
                   ,nrow=111)
sum(data1,na.rm = T)
data1 <- t(apply(data1,1,function(x){fifelse(is.na(x),0,x)}))

data2 <- as.matrix(read.table("Data/ITA age-cohort decomp 2018.txt"),
                   nrow=111)
sum(data2,na.rm = T)
data2 <- t(apply(data2,1,function(x){fifelse(is.na(x),0,x)}))

data3 <- (data2-data1)

# data3 <- ((data1*data2)^0.5)*(log(data2/data1)/29)

results <- ifelse(is.nan(data3),0,data3)

#### Plot the Lexis Surface ####

Y1 <- 1893
Y2 <- 2004

colnames(results) <- seq(Y1+1,Y2,1)

mypalette<-rev(brewer.pal(8,"YlGnBu"))
mypalette2<-rev(brewer.pal(8,"YlOrRd"))
WildColors<-c(mypalette[1:4],"white","white",
              mypalette2[c(6,4,2,1)])
WildColors<-c(WildColors[1:4],"white","white",
              WildColors[7:10])

# the different levels of the Z values
levels<-c(-1e-04,-5e-05,-1e-05,-5e-06,-1e-06,
          0,1e-06,5e-06,1e-05,5e-05,1e-04)

customAxis <- function() { 
  n <- length(levels) 
  y <- seq(min(levels), max(levels), length.out=n) 
  rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors) 
  axis(4, at=y, labels=levels) 
} 

pdf("Output/Age & Cohort Decomposition, ITA-AVG, 1990-2018.pdf",
    width = 8,height = 6)

filled.contour(x=seq(Y1+1,Y2,1),y=c(0:110),z=t(results),
               levels = levels,col = WildColors,
               key.axes = customAxis(),
               xlab="Year",ylab="Age")
title(main="Age- & Cohort- Decomposition across time, \n ITA male - Average, 1990-2018",
      sub = paste("ITA - AVG differences across time: ", "0.0273", sep=""),
      font.main=4)

dev.off()

test1 <- Confunc(data1)
test2 <- Confunc(data2)
plot(test1,col=0,ylab="Contribution",xlab="Age")
lines(test1,lwd=2)
lines(test2,col="blue",lwd=2)
abline(h=0,lty=2)
title("Cohorts' contributions to differences in CAL-entropy gap,
      Italy - Average, 1879-1990")
