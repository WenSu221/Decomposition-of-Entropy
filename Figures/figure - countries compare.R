#### Figure - entropy across time ####

SWE <- read.table("Output/HSWE.txt")
DNK <- read.table("Output/HDNK.txt")
FRA <- read.table("Output/HFRA.txt")
GBR <- read.table("Output/HGBR.txt")
NOR <- read.table("Output/HNOR.txt")
allH <- as.data.frame(cbind(SWE,DNK,FRA,GBR,NOR))
names(allH) <- c("SWE","DNK","FRA","GBR","NOR")
allH$Years <- c(seq(1957,2017,5))

windows(8,5)
plot(range(allH$Years),rev(c(0.12,0.35)),col = 0, xlab = "Years",ylab = "entropy of CAL (log scale)", log = "y")
lines(allH$Years,allH$SWE,col = 1,lty=1,lwd=1.5)
lines(allH$Years,allH$DNK,col = 2,lty=2,lwd=1.5)
lines(allH$Years,allH$FRA,col = 3,lty=3,lwd=1.5)
lines(allH$Years,allH$GBR,col = 4,lty=4,lwd=1.5)
lines(allH$Years,allH$NOR,col = 6,lty=5,lwd=1.5)
title("comparison of entropy of CAL across five countries, female 1957-2017")
legend("topright",c("Sweden","Denmark","France","England and Wales","Norway"),col = c(1,2,3,4,6),lty = c(1,2,3,4,5))


### SWEDEN as the benchmark # ----

diffDNK <- allH$DNK - allH$SWE
diffFRA <- allH$FRA - allH$SWE
diffGBR <- allH$GBR - allH$SWE
diffNOR <- allH$NOR - allH$SWE

### R graphics
diffH <- cbind(diffDNK,diffFRA,diffGBR,diffNOR)
diffH <- cbind(rep(allH$Years),diffH)
Years <- allH$Years

windows(8,5)
plot(Years,seq(0,0.1,length.out = 13),col = 0,ylab="differences from Sweden level of entropy of CAL")
lines(Years,diffDNK,col = 2,lty=2,lwd=1.5)
lines(Years,diffFRA,col = 3,lty=3,lwd=1.5)
lines(Years,diffGBR,col = 4,lty=4,lwd=1.5)
lines(Years,diffNOR,col = 6,lty=6,lwd=1.5)
legend("topright",c("Denmark","France","England and Wales","Norway"),col = c(2,3,4,6),lty = c(2,3,4,6))
title("Entropy of CAL compared to Sweden level, female 1957-2017")