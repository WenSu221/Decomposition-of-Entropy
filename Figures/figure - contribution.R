#### contribution figure ####

### data ####

source("Figures/average of CAL 1989-2017.R")

#### short data ####

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1989,2017,4)

entropydiff <- entropyCAL2-CALentropyavg

entropyavg <- (entropyCAL2+CALentropyavg)/2

dispersion <- log(CALdagger2/CALdaggeravg)
measure <- log(CAL2/CALavg)

variation <- dispersion*entropyavg
longevity <- -(measure*entropyavg)

total <- longevity+variation
decomp <- rbind(variation,longevity)
rownames(decomp) <- c("lifespan inequality component","longevity component")
colnames(decomp) <- c(seq(1989,2017,4))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- range(total)*1.2

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
mtext("average level", side = 2,adj = 0.5)