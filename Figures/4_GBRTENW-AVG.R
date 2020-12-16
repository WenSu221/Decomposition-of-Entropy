#### GBR and AVG ####

### decomp ####

source("Figures/average of CAL 1989-2017.R")

## decomp ####

entropyavg <- (entropyCAL4+CALentropyavg)/2
entropydiff <- entropyCAL4-CALentropyavg

dispersion <- log(CALdagger4/CALdaggeravg)
measure <- log(CAL4/CALavg)

## validation ####
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ####
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ####

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1989,2017,4)

total <- variation+longevity
decomp <- rbind(variation,longevity)
rownames(decomp) <- c("lifespan inequality component","longevity component")
colnames(decomp) <- c(seq(1989,2017,4))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- c(-0.025,0.035)

windows(12,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,args.legend = list(x="bottomright",bg="white",box.col=0),
             main = "England and Wales,female 1989-2017",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T)

lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average level", side = 2,adj = 0.4)

### decomp in changes ####

rm(list = ls())
source("Figures/average of CAL 1989-2017.R")

## decomp of contributions ####

entropydiff <- entropyCAL4-CALentropyavg

entropyavg <- (entropyCAL4+CALentropyavg)/2

dispersion <- log(CALdagger4/CALdaggeravg)
measure <- log(CAL4/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ####

#change in entropy differences
centropydiff_relative <- log(entropydiff[2:8]/entropydiff[1:7])/4
centropydiff_original <- entropydiff[1:7]*exp(1)^(2*log(entropydiff[2:8]/entropydiff[1:7])/4)
centropydiff_derivative <- centropydiff_relative*centropydiff_original

#change in entropy average
centropyavg_relative <- log(entropyavg[2:8]/entropyavg[1:7])/4
centropyavg_original <- entropyavg[1:7]*exp(1)^(2*log(entropyavg[2:8]/entropyavg[1:7])/4)
centropyavg_derivative <- centropyavg_relative*centropyavg_original

#average in entropy difference
entropydiffavg_relative <- log(entropydiff[2:8]/entropydiff[1:7])/4
entropydiffavg_original <- entropydiff[1:7]*exp(1)^(2*entropydiffavg_relative)

#average in average in entropy
entropyavgavg_relative <- log(entropyavg[2:8]/entropyavg[1:7])/4
entropyavgavg_original <- entropyavg[1:7]*exp(1)^(2*entropyavgavg_relative)


cvariation_relative <- log(variation[2:8]/variation[1:7])/4
cvariation_original <- variation[1:7]*exp(1)^(2*cvariation_relative)
cvariation_derivative <- cvariation_relative*cvariation_original

clongevity_relative <- log(longevity[2:8]/longevity[1:7])/4
clongevity_original <- longevity[1:7]*exp(1)^(2*clongevity_relative)
clongevity_derivative <- clongevity_original*clongevity_relative

equation2 <- centropydiff_derivative - (centropyavg_derivative*entropydiffavg_original+entropyavgavg_original*(cvariation_derivative+clongevity_derivative))

## validation ####
round(equation1,5)
round(equation2,5)

## figures ####

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1991,2015,4)

longevitypart <- clongevity_derivative*entropyavgavg_original
variationpart <- cvariation_derivative*entropyavgavg_original
entropychange <- centropyavg_derivative*entropydiffavg_original
total <- variationpart+longevitypart+entropychange
decomp <- rbind(variationpart,longevitypart,entropychange)
rownames(decomp) <- c("lifespan variation component","longevity component","changes in averge entropy differences")
colnames(decomp) <- c(seq(1991,2015,4))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- c(-0.0004,0.0002)

windows(12,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             legend.text = TRUE,args.legend = list(x="bottomright",bg="white",box.col=0),
             main = "England and Wales,female 1991-2015",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)

lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average level", side = 2,adj = 0.7)
