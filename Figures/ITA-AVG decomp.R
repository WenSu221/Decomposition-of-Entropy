#### Italy and average ####

### Data fitting ####

source("Figures/average of CAL 1989-2017.R")

## decomp ####

entropyavg <- (entropyCAL7+CALentropyavg)/2
entropydiff <- entropyCAL7-CALentropyavg

dispersion <- log(CALdagger7/CALdaggeravg)
measure <- log(CAL7/CALavg)

## validation ####
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ####
variation <- dispersion*entropyavg
longevity <- measure*entropyavg*-1

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1989,2017,4)

total <- longevity+variation
decomp <- rbind(variation,longevity)
rownames(decomp) <- c("lifespan inequality component","longevity component")
colnames(decomp) <- c(seq(1989,2017,4))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- c(0,max(total)*1.5)

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
             main = "Decomposition of entropy of CAL between Denmark and average level,both 1992-2017",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T)

lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average level", side = 2,adj = -0.1)
