#### Italy and average ####

### Data fitting ####

source("Entropy calculation/entropy of CAL average.R")

## decomp ####

entropyavg <- (entropyCAL7+CALHavg2)/2
entropydiff <- entropyCAL7-CALHavg2

dispersion <- log(CALdagger7/CALdavg2)
measure <- log(CAL7/CALavg2)

right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

inequality <- dispersion*entropyavg
longevity <- measure*entropyavg*-1

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1992,2017,3)

longevity <- longevity
inequality <- inequality
total <- longevity+inequality
decomp <- rbind(inequality,longevity)
rownames(decomp) <- c("lifespan inequality component","longevity component")
colnames(decomp) <- c(seq(1992,2017,3))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- c(0,0.035)

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
             main = "Decomposition of entropy of CAL between Denmark and average level,both 1992-2017",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,
             args.legend = list(x="topright",bg="white",box.col=0))

lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average level", side = 2,adj = -0.1)
