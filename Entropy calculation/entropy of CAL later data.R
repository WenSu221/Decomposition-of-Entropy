#### entropy of CAL cross-national decomposition ####

### Data fitting ####

source("Entropy calculation/entropy of CAL average.R")

## decomp ####

entropyavg <- (entropyCAL2[13:21]+CALHavg2)/2
entropydiff <- entropyCAL2[13:21]-CALHavg2

dispersion <- log(CALdagger2[13:21]/CALdavg2)
measure <- log(CAL2[13:21]/CALavg2)

right <- (dispersion - measure)*entropyavg
left <- entropydiff
round(right-left, 5)

inequality <- dispersion*entropyavg
longevity <- measure*entropyavg*-1

# inequality <- dispersion*-1/(measure+dispersion*-1)
# longevity <- measure/(measure+dispersion*-1)
# all <- inequality+longevity