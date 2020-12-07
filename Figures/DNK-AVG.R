#### Denmark and Average ####


### decomp ####

source("Figures/average of CAL 1989-2017.R")

## decomp ####

entropyavg <- (entropyCAL2+CALentropyavg)/2
entropydiff <- entropyCAL2-CALentropyavg

dispersion <- log(CALdagger2/CALdaggeravg)
measure <- log(CAL2/CALavg)

## validation ####
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ####
variation <- dispersion*entropyavg
longevity <- measure*entropyavg*-1

## figures ####

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
myrange <- range(range(total),0)*1.2

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,args.legend = list(x="bottomright",bg="white",box.col=0),
             main = "Decomposition of entropy of CAL between Denmark and average level,both 1989-2017",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T)

lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average level", side = 2,adj = 1.0)

### decomp in changes ####

rm(list = ls())
source("Figures/average of CAL 1989-2017.R")

## decomp of contributions ####

entropydiff <- entropyCAL2-CALentropyavg

entropyavg <- (entropyCAL2+CALentropyavg)/2

dispersion <- log(CALdagger2/CALdaggeravg)
measure <- log(CAL2/CALavg)

variation <- dispersion*entropyavg
longevity <- -(measure*entropyavg)

equation1 <- entropydiff - (variation+longevity)

## decomp of changes in contributions ####

centropydiff <- (entropydiff[2:8]-entropydiff[1:7])/4

centropyavg <- (entropyavg[2:8] - entropyavg[1:7])/4
entropydiffavg <- (entropydiff[1:7]+entropydiff[2:8])/2
entropyavgavg <- (entropyavg[1:7]+entropyavg[2:8])/2

cvariation <- (variation[2:8]-variation[1:7])/4
clongevity <- (longevity[2:8]-longevity[1:7])/4

equation2 <- centropydiff - (centropyavg*entropydiffavg+entropyavgavg*(cvariation+clongevity))


## validation ####
round(equation1,5)
round(equation2,5)


## figures ####

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1991,2015,4)

longevitypart <- clongevity*entropyavgavg
variationpart <- cvariation*entropyavgavg
entropychange <- centropyavg*entropydiffavg
total <- variationpart+longevitypart+entropychange
decomp <- rbind(variationpart,longevitypart,entropychange)
rownames(decomp) <- c("lifespan variation component","longevity component","changes in averge entropy differences")
colnames(decomp) <- c(seq(1991,2015,4))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- range(range(total),0)*1.2

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
             main = "Decomposition of entropy of CAL between Denmark and average level,both 1991-2015",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)

lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average entropy level", side = 2,adj = 0.1)
