#### Composition of Changes ####

source("Figures/average of CAL 1989-2017.R")

## decomp of contributions ####

entropydiff <- entropyCAL2-CALentropyavg

entropyavg <- (entropyCAL2+CALentropyavg)/2

dispersion <- log(CALdagger2/CALdaggeravg)
measure <- log(CAL2/CALavg)

inequality <- dispersion*entropyavg
longevity <- -(measure*entropyavg)

equation1 <- entropydiff - (inequality+longevity)

## decomp of changes in contributions ####

centropydiff <- (entropydiff[2:8]-entropydiff[1:7])/4

centropyavg <- (entropyavg[2:8] - entropyavg[1:7])/4
entropydiffavg <- (entropydiff[1:7]+entropydiff[2:8])/2
entropyavgavg <- (entropyavg[1:7]+entropyavg[2:8])/2

cdispersion <- (dispersion[2:8]-dispersion[1:7])/4
clongevity <- (longevity[2:8]-longevity[1:7])/4

equation2 <- centropydiff - (centropyavg*entropydiffavg+entropyavgavg*(cdispersion+clongevity))


## compare ####
round(equation1,5)
round(equation2,5)