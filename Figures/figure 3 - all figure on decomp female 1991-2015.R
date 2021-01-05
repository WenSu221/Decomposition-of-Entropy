#### ALL FIGURES for female ####

source("Figures/figure 3 - average of CAL female 1989-2017.R")

### Sweden ####

## decomp ###

entropyavg <- (entropyCAL1+CALentropyavg)/2
entropydiff <- entropyCAL1-CALentropyavg

dispersion <- log(CALdagger1/CALdaggeravg)
measure <- log(CAL1/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###

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
# myrange <- range(range(total),0)*1.2
myrange <- c(-0.025,0.035)
png("Output/SWE_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Sweden, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL1-CALentropyavg

entropyavg <- (entropyCAL1+CALentropyavg)/2

dispersion <- log(CALdagger1/CALdaggeravg)
measure <- log(CAL1/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

#longevity and variation components
cvariation_relative <- log(variation[2:8]/variation[1:7])/4
cvariation_original <- variation[1:7]*exp(1)^(2*cvariation_relative)
cvariation_derivative <- cvariation_relative*cvariation_original

clongevity_relative <- log(longevity[2:8]/longevity[1:7])/4
clongevity_original <- longevity[1:7]*exp(1)^(2*clongevity_relative)
clongevity_derivative <- clongevity_original*clongevity_relative

equation2 <- centropydiff_derivative - (centropyavg_derivative*entropydiffavg_original+entropyavgavg_original*(cvariation_derivative+clongevity_derivative))

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###

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
# myrange <- range(range(total),0)*1.2
myrange <- c(-0.0004,0.0002)

png("Output/SWE_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             legend.text = TRUE,args.legend = list(x="bottomright",bg="white",box.col=0),
             main = "Sweden, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Denmark ####

## decomp ###

entropyavg <- (entropyCAL2+CALentropyavg)/2
entropydiff <- entropyCAL2-CALentropyavg

dispersion <- log(CALdagger2/CALdaggeravg)
measure <- log(CAL2/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- measure*entropyavg*-1

## figures ###

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
# myrange <- range(range(total),0)*1.2
myrange <- c(-0.025,0.035)

png("Output/DNK_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Denmark, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL2-CALentropyavg

entropyavg <- (entropyCAL2+CALentropyavg)/2

dispersion <- log(CALdagger2/CALdaggeravg)
measure <- log(CAL2/CALavg)

variation <- dispersion*entropyavg
longevity <- -(measure*entropyavg)

equation1 <- entropydiff - (variation+longevity)

## decomp of changes in contributions ###

centropydiff <- (entropydiff[2:8]-entropydiff[1:7])/4

centropyavg <- (entropyavg[2:8] - entropyavg[1:7])/4
entropydiffavg <- (entropydiff[1:7]+entropydiff[2:8])/2
entropyavgavg <- (entropyavg[1:7]+entropyavg[2:8])/2

cvariation <- (variation[2:8]-variation[1:7])/4
clongevity <- (longevity[2:8]-longevity[1:7])/4

equation2 <- centropydiff - (centropyavg*entropydiffavg+entropyavgavg*(cvariation+clongevity))


## validation ###
round(equation1,5)
round(equation2,5)


## figures ###

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
# myrange <- range(range(total),0)*1.2
myrange <- c(-0.0004,0.0002)

png("Output/DNK_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "Denmark, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average entropy level", side = 2,adj = 0.7)
dev.off()

### France ####

## decomp ###

entropyavg <- (entropyCAL3+CALentropyavg)/2
entropydiff <- entropyCAL3-CALentropyavg

dispersion <- log(CALdagger3/CALdaggeravg)
measure <- log(CAL3/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###

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

png("Output/FRATNP_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "France, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL3-CALentropyavg

entropyavg <- (entropyCAL3+CALentropyavg)/2

dispersion <- log(CALdagger3/CALdaggeravg)
measure <- log(CAL3/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

#longevity and variation components
cvariation_relative <- log(variation[2:8]/variation[1:7])/4
cvariation_original <- variation[1:7]*exp(1)^(2*cvariation_relative)
cvariation_derivative <- cvariation_relative*cvariation_original

clongevity_derivative <- clongevity <- (longevity[2:8]-longevity[1:7])/4

equation2 <- centropydiff_derivative - (centropyavg_derivative*entropydiffavg_original+entropyavgavg_original*(cvariation_derivative+clongevity_derivative))

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###

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

png("Output/FRATNP_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "France, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Britain ####

## decomp ###

entropyavg <- (entropyCAL4+CALentropyavg)/2
entropydiff <- entropyCAL4-CALentropyavg

dispersion <- log(CALdagger4/CALdaggeravg)
measure <- log(CAL4/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###
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

png("Output/GBRTENW_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "England and Wales, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL4-CALentropyavg

entropyavg <- (entropyCAL4+CALentropyavg)/2

dispersion <- log(CALdagger4/CALdaggeravg)
measure <- log(CAL4/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###
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

png("Output/GBRTENW_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "England and Wales, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Norway ####

## decomp ###

entropyavg <- (entropyCAL5+CALentropyavg)/2
entropydiff <- entropyCAL5-CALentropyavg

dispersion <- log(CALdagger5/CALdaggeravg)
measure <- log(CAL5/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###
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

png("Output/NOR_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Norway, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL5-CALentropyavg

entropyavg <- (entropyCAL5+CALentropyavg)/2

dispersion <- log(CALdagger5/CALdaggeravg)
measure <- log(CAL5/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###

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

png("Output/NOR_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "Norway, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Finland ####

## decomp ###

entropyavg <- (entropyCAL6+CALentropyavg)/2
entropydiff <- entropyCAL6-CALentropyavg

dispersion <- log(CALdagger6/CALdaggeravg)
measure <- log(CAL6/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###

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

png("Output/FIN_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Finland, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL6-CALentropyavg

entropyavg <- (entropyCAL6+CALentropyavg)/2

dispersion <- log(CALdagger6/CALdaggeravg)
measure <- log(CAL6/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

centropydiff_derivative <- (entropydiff[2:8]-entropydiff[1:7])/4

centropyavg_derivative <- (entropyavg[2:8] - entropyavg[1:7])/4
entropydiffavg_original <- (entropydiff[1:7]+entropydiff[2:8])/2
entropyavgavg_original <- (entropyavg[1:7]+entropyavg[2:8])/2

cvariation_derivative <- (variation[2:8]-variation[1:7])/4
clongevity_derivative <- (longevity[2:8]-longevity[1:7])/4

equation2 <- centropydiff_derivative - (centropyavg_derivative*entropydiffavg_original+entropyavgavg_original*(cvariation_derivative+clongevity_derivative))

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###

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

png("Output/FIN_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "Finland, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Italy ####

## decomp ###
entropyavg <- (entropyCAL7+CALentropyavg)/2
entropydiff <- entropyCAL7-CALentropyavg

dispersion <- log(CALdagger7/CALdaggeravg)
measure <- log(CAL7/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###
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

png("Output/ITA_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Italy, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL7-CALentropyavg

entropyavg <- (entropyCAL7+CALentropyavg)/2

dispersion <- log(CALdagger7/CALdaggeravg)
measure <- log(CAL7/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

#longevity and variation components
cvariation_relative <- log(variation[2:8]/variation[1:7])/4
cvariation_original <- variation[1:7]*exp(1)^(2*cvariation_relative)
cvariation_derivative <- cvariation_relative*cvariation_original

clongevity_relative <- log(longevity[2:8]/longevity[1:7])/4
clongevity_original <- longevity[1:7]*exp(1)^(2*clongevity_relative)
clongevity_derivative <- clongevity_original*clongevity_relative

equation2 <- centropydiff_derivative - (centropyavg_derivative*entropydiffavg_original+entropyavgavg_original*(cvariation_derivative+clongevity_derivative))

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###

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

png("Output/ITA_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "Italy, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Scotland ####

## decomp ###
entropyavg <- (entropyCAL8+CALentropyavg)/2
entropydiff <- entropyCAL8-CALentropyavg

dispersion <- log(CALdagger8/CALdaggeravg)
measure <- log(CAL8/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###
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

png("Output/GBRSCO_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Scotland, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###
entropydiff <- entropyCAL8-CALentropyavg

entropyavg <- (entropyCAL8+CALentropyavg)/2

dispersion <- log(CALdagger8/CALdaggeravg)
measure <- log(CAL8/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

#longevity and variation components
cvariation_relative <- log(variation[2:8]/variation[1:7])/4
cvariation_original <- variation[1:7]*exp(1)^(2*cvariation_relative)
cvariation_derivative <- cvariation_relative*cvariation_original

clongevity_relative <- log(longevity[2:8]/longevity[1:7])/4
clongevity_original <- longevity[1:7]*exp(1)^(2*clongevity_relative)
clongevity_derivative <- clongevity_original*clongevity_relative

equation2 <- centropydiff_derivative - (centropyavg_derivative*entropydiffavg_original+entropyavgavg_original*(cvariation_derivative+clongevity_derivative))

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###
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

png("Output/GBRSCO_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "Scotland, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()

### Netherlands ####

## decomp ###
entropyavg <- (entropyCAL9+CALentropyavg)/2
entropydiff <- entropyCAL9-CALentropyavg

dispersion <- log(CALdagger9/CALdaggeravg)
measure <- log(CAL9/CALavg)

## validation ###
right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

## decomposition ###
variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

## figures ###
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

png("Output/NLD_change_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             main = "Netherlands, female 1989-2017")
b <- barplot(decomp2,col = coul,border = "white",add = T)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.4)
dev.off()

### decomp in changes ###

## decomp of contributions ###

entropydiff <- entropyCAL9-CALentropyavg

entropyavg <- (entropyCAL9+CALentropyavg)/2

dispersion <- log(CALdagger9/CALdaggeravg)
measure <- log(CAL9/CALavg)

variation <- dispersion*entropyavg
longevity <- (measure*entropyavg)*-1

equation1 <- entropydiff - (variation-longevity)

## decomp of changes in contributions ###

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

## validation ###
round(equation1,5)
round(equation2,5)

## figures ###
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

png("Output/NLD_dod_female.png", units = "in",width = 10,height = 8,res = 150)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "contribution to changes in relative disparities",
             main = "Netherlands, female 1991-2015")
b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange)
lines(b,total,lwd=2)
points(b,total,cex = 1.2,pch=16)
mtext("average level", side = 2,adj = 0.7)
dev.off()