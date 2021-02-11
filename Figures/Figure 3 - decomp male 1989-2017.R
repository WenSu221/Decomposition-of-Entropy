#### ggplot2 population decomp, male####

##packages and color
library(ggplot2)


## Source
source("Figures/figure 3 - average of CAL male 1989-2017.R")
source("US Data/USCAL male.R")

### Sweden ####
## decomp of contributions ###

entropydiff_SWE <- entropyCAL1-CALentropyavg

entropyavg_SWE <- (entropyCAL1+CALentropyavg)/2

dispersion_SWE <- log(CALdagger1/CALdaggeravg)
measure_SWE <- log(CAL1/CALavg)

variation_SWE <- dispersion_SWE*entropyavg_SWE
longevity_SWE <- (measure_SWE*entropyavg_SWE)*-1

equation1 <- entropydiff_SWE - (dispersion_SWE - measure_SWE)*entropyavg_SWE

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_SWE <- log(entropydiff_SWE[2:15]/entropydiff_SWE[1:14])/2
centropydiff_original_SWE <- entropydiff_SWE[1:14]*exp(1)^(centropydiff_relative_SWE)
centropydiff_derivative_SWE <- centropydiff_relative_SWE*centropydiff_original_SWE

#change in entropy average
centropyavg_relative_SWE <- log(entropyavg_SWE[2:15]/entropyavg_SWE[1:14])/2
centropyavg_original_SWE <- entropyavg_SWE[1:14]*exp(1)^(centropyavg_relative_SWE)
centropyavg_derivative_SWE <- centropyavg_relative_SWE*centropyavg_original_SWE

#average in entropy difference
entropydiffavg_relative_SWE <- log(entropydiff_SWE[2:15]/entropydiff_SWE[1:14])/2
entropydiffavg_original_SWE <- entropydiff_SWE[1:14]*exp(1)^(entropydiffavg_relative_SWE)

#average in average in entropy
entropyavgavg_relative_SWE <- log(entropyavg_SWE[2:15]/entropyavg_SWE[1:14])/2
entropyavgavg_original_SWE <- entropyavg_SWE[1:14]*exp(1)^(entropyavgavg_relative_SWE)


cvariation_relative_SWE <- log(variation_SWE[2:15]/variation_SWE[1:14])/2
cvariation_original_SWE <- variation_SWE[1:14]*exp(1)^(cvariation_relative_SWE)
cvariation_derivative_SWE <- cvariation_relative_SWE*cvariation_original_SWE

clongevity_relative_SWE <- log(longevity_SWE[2:15]/longevity_SWE[1:14])/2
clongevity_original_SWE <- longevity_SWE[1:14]*exp(1)^(clongevity_relative_SWE)
clongevity_derivative_SWE <- clongevity_original_SWE*clongevity_relative_SWE

equation2 <- centropydiff_derivative_SWE - (centropyavg_derivative_SWE*entropydiffavg_original_SWE+
                                              entropyavgavg_original_SWE*
                                              (cvariation_derivative_SWE+clongevity_derivative_SWE))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_SWE <- variation_SWE
dlongevity_SWE <- longevity_SWE
dtotal_SWE <- dvariation_SWE + dlongevity_SWE

clongevity_SWE <- clongevity_derivative_SWE*entropyavgavg_original_SWE
cvariation_SWE <- cvariation_derivative_SWE*entropyavgavg_original_SWE
centropychange_SWE <- centropyavg_derivative_SWE*entropydiffavg_original_SWE
ctotal_SWE <- cvariation_SWE+clongevity_SWE+centropychange_SWE

### Denmark ####

## decomp of contributions ###

entropydiff_DNK <- entropyCAL2-CALentropyavg

entropyavg_DNK <- (entropyCAL2+CALentropyavg)/2

dispersion_DNK <- log(CALdagger2/CALdaggeravg)
measure_DNK <- log(CAL2/CALavg)

variation_DNK <- dispersion_DNK*entropyavg_DNK
longevity_DNK <- -(measure_DNK*entropyavg_DNK)

equation1 <- entropydiff_DNK - (dispersion_DNK-measure_DNK)*entropyavg_DNK

## decomp of changes in contributions ###

centropydiff_DNK <- (entropydiff_DNK[2:15]-entropydiff_DNK[1:14])/2

centropyavg_DNK <- (entropyavg_DNK[2:15] - entropyavg_DNK[1:14])/2
entropydiffavg_DNK <- (entropydiff_DNK[1:14]+entropydiff_DNK[2:15])/2
entropyavgavg_DNK <- (entropyavg_DNK[1:14]+entropyavg_DNK[2:15])/2

cvariation_DNK <- (variation_DNK[2:15]-variation_DNK[1:14])/2
clongevity_DNK <- (longevity_DNK[2:15]-longevity_DNK[1:14])/2

equation2 <- centropydiff_DNK - (centropyavg_DNK*entropydiffavg_DNK+
                                   entropyavgavg_DNK*
                                   (cvariation_DNK+clongevity_DNK))


## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_DNK <- variation_DNK
dlongevity_DNK <- longevity_DNK
dtotal_DNK <- dvariation_DNK + dlongevity_DNK

clongevity_DNK <- clongevity_DNK*entropyavgavg_DNK
cvariation_DNK <- cvariation_DNK*entropyavgavg_DNK
centropychange_DNK <- centropyavg_DNK*entropydiffavg_DNK
ctotal_DNK <- cvariation_DNK+clongevity_DNK+centropychange_DNK


### France ####
## decomp of contributions ###

entropydiff_FRA <- entropyCAL3-CALentropyavg

entropyavg_FRA <- (entropyCAL3+CALentropyavg)/2

dispersion_FRA <- log(CALdagger3/CALdaggeravg)
measure_FRA <- log(CAL3/CALavg)

variation_FRA <- dispersion_FRA*entropyavg_FRA
longevity_FRA <- (measure_FRA*entropyavg_FRA)*-1

equation1 <- entropydiff_FRA - (dispersion_FRA-measure_FRA)*entropyavg_FRA

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_FRA <- log(entropydiff_FRA[2:15]/entropydiff_FRA[1:14])/2
centropydiff_original_FRA <- entropydiff_FRA[1:14]*exp(1)^(centropydiff_relative_FRA)
centropydiff_derivative_FRA <- centropydiff_relative_FRA*centropydiff_original_FRA

#change in entropy average
centropyavg_relative_FRA <- log(entropyavg_FRA[2:15]/entropyavg_FRA[1:14])/2
centropyavg_original_FRA <- entropyavg_FRA[1:14]*exp(1)^(centropyavg_relative_FRA)
centropyavg_derivative_FRA <- centropyavg_relative_FRA*centropyavg_original_FRA

#average in entropy difference
entropydiffavg_relative_FRA <- log(entropydiff_FRA[2:15]/entropydiff_FRA[1:14])/2
entropydiffavg_original_FRA <- entropydiff_FRA[1:14]*exp(1)^(entropydiffavg_relative_FRA)

#average in average in entropy
entropyavgavg_relative_FRA <- log(entropyavg_FRA[2:15]/entropyavg_FRA[1:14])/2
entropyavgavg_original_FRA <- entropyavg_FRA[1:14]*exp(1)^(entropyavgavg_relative_FRA)

#longevity and variation components
cvariation_relative_FRA <- log(variation_FRA[2:15]/variation_FRA[1:14])/2
cvariation_original_FRA <- variation_FRA[1:14]*exp(1)^(cvariation_relative_FRA)
cvariation_derivative_FRA <- cvariation_relative_FRA*cvariation_original_FRA

clongevity_relative_FRA <- log(longevity_FRA[2:15]/longevity_FRA[1:14])/2
clongevity_original_FRA <- longevity_FRA[1:14]*exp(1)^(clongevity_relative_FRA)
clongevity_derivative_FRA <- clongevity_original_FRA*clongevity_relative_FRA

equation2 <- centropydiff_derivative_FRA - (centropyavg_derivative_FRA*entropydiffavg_original_FRA+
                                              entropyavgavg_original_FRA*
                                              (cvariation_derivative_FRA+clongevity_derivative_FRA))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_FRA <- variation_FRA
dlongevity_FRA <- longevity_FRA
dtotal_FRA <- dvariation_FRA + dlongevity_FRA

clongevity_FRA <- clongevity_derivative_FRA*entropyavgavg_original_FRA
cvariation_FRA <- cvariation_derivative_FRA*entropyavgavg_original_FRA
centropychange_FRA <- centropyavg_derivative_FRA*entropydiffavg_original_FRA
ctotal_FRA <- cvariation_FRA+clongevity_FRA+centropychange_FRA


### Endland & Wales ####
## decomp of contributions ###

entropydiff_GBRTENW <- entropyCAL4-CALentropyavg

entropyavg_GBRTENW <- (entropyCAL4+CALentropyavg)/2

dispersion_GBRTENW <- log(CALdagger4/CALdaggeravg)
measure_GBRTENW <- log(CAL4/CALavg)

variation_GBRTENW <- dispersion_GBRTENW*entropyavg_GBRTENW
longevity_GBRTENW <- (measure_GBRTENW*entropyavg_GBRTENW)*-1

equation1 <- entropydiff_GBRTENW - (dispersion_GBRTENW-measure_GBRTENW)*entropyavg_GBRTENW

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_GBRTENW <- log(entropydiff_GBRTENW[2:15]/entropydiff_GBRTENW[1:14])/2
centropydiff_original_GBRTENW <- entropydiff_GBRTENW[1:14]*exp(1)^(centropydiff_relative_GBRTENW)
centropydiff_derivative_GBRTENW <- centropydiff_relative_GBRTENW*centropydiff_original_GBRTENW

#change in entropy average
centropyavg_relative_GBRTENW <- log(entropyavg_GBRTENW[2:15]/entropyavg_GBRTENW[1:14])/2
centropyavg_original_GBRTENW <- entropyavg_GBRTENW[1:14]*exp(1)^(centropyavg_relative_GBRTENW)
centropyavg_derivative_GBRTENW <- centropyavg_relative_GBRTENW*centropyavg_original_GBRTENW

#average in entropy difference
entropydiffavg_relative_GBRTENW <- log(entropydiff_GBRTENW[2:15]/entropydiff_GBRTENW[1:14])/2
entropydiffavg_original_GBRTENW <- entropydiff_GBRTENW[1:14]*exp(1)^(entropydiffavg_relative_GBRTENW)

#average in average in entropy
entropyavgavg_relative_GBRTENW <- log(entropyavg_GBRTENW[2:15]/entropyavg_GBRTENW[1:14])/2
entropyavgavg_original_GBRTENW <- entropyavg_GBRTENW[1:14]*exp(1)^(entropyavgavg_relative_GBRTENW)


cvariation_relative_GBRTENW <- log(variation_GBRTENW[2:15]/variation_GBRTENW[1:14])/2
cvariation_original_GBRTENW <- variation_GBRTENW[1:14]*exp(1)^(cvariation_relative_GBRTENW)
cvariation_derivative_GBRTENW <- cvariation_relative_GBRTENW*cvariation_original_GBRTENW

clongevity_relative_GBRTENW <- log(longevity_GBRTENW[2:15]/longevity_GBRTENW[1:14])/2
clongevity_original_GBRTENW <- longevity_GBRTENW[1:14]*exp(1)^(clongevity_relative_GBRTENW)
clongevity_derivative_GBRTENW <- clongevity_original_GBRTENW*clongevity_relative_GBRTENW

equation2 <- centropydiff_derivative_GBRTENW - (centropyavg_derivative_GBRTENW*entropydiffavg_original_GBRTENW+
                                                  entropyavgavg_original_GBRTENW*
                                                  (cvariation_derivative_GBRTENW+clongevity_derivative_GBRTENW))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_GBRTENW <- variation_GBRTENW
dlongevity_GBRTENW <- longevity_GBRTENW
dtotal_GBRTENW <- dvariation_GBRTENW + dlongevity_GBRTENW

clongevity_GBRTENW <- clongevity_derivative_GBRTENW*entropyavgavg_original_GBRTENW
cvariation_GBRTENW <- cvariation_derivative_GBRTENW*entropyavgavg_original_GBRTENW
centropychange_GBRTENW <- centropyavg_derivative_GBRTENW*entropydiffavg_original_GBRTENW
ctotal_GBRTENW <- cvariation_GBRTENW+clongevity_GBRTENW+centropychange_GBRTENW


### Norway ####
## decomp of contributions ###

entropydiff_NOR <- entropyCAL5-CALentropyavg

entropyavg_NOR <- (entropyCAL5+CALentropyavg)/2

dispersion_NOR <- log(CALdagger5/CALdaggeravg)
measure_NOR <- log(CAL5/CALavg)

variation_NOR <- dispersion_NOR*entropyavg_NOR
longevity_NOR <- (measure_NOR*entropyavg_NOR)*-1

equation1 <- entropydiff_NOR - (dispersion_NOR-measure_NOR)*entropyavg_NOR

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_NOR <- log(entropydiff_NOR[2:15]/entropydiff_NOR[1:14])/2
centropydiff_original_NOR <- entropydiff_NOR[1:14]*exp(1)^(centropydiff_relative_NOR)
centropydiff_derivative_NOR <- centropydiff_relative_NOR*centropydiff_original_NOR

#change in entropy average
centropyavg_relative_NOR <- log(entropyavg_NOR[2:15]/entropyavg_NOR[1:14])/2
centropyavg_original_NOR <- entropyavg_NOR[1:14]*exp(1)^(centropyavg_relative_NOR)
centropyavg_derivative_NOR <- centropyavg_relative_NOR*centropyavg_original_NOR

#average in entropy difference
entropydiffavg_relative_NOR <- log(entropydiff_NOR[2:15]/entropydiff_NOR[1:14])/2
entropydiffavg_original_NOR <- entropydiff_NOR[1:14]*exp(1)^(entropydiffavg_relative_NOR)

#average in average in entropy
entropyavgavg_relative_NOR <- log(entropyavg_NOR[2:15]/entropyavg_NOR[1:14])/2
entropyavgavg_original_NOR <- entropyavg_NOR[1:14]*exp(1)^(entropyavgavg_relative_NOR)


cvariation_relative_NOR <- log(variation_NOR[2:15]/variation_NOR[1:14])/2
cvariation_original_NOR <- variation_NOR[1:14]*exp(1)^(cvariation_relative_NOR)
cvariation_derivative_NOR <- cvariation_relative_NOR*cvariation_original_NOR

clongevity_relative_NOR <- log(longevity_NOR[2:15]/longevity_NOR[1:14])/2
clongevity_original_NOR <- longevity_NOR[1:14]*exp(1)^(clongevity_relative_NOR)
clongevity_derivative_NOR <- clongevity_original_NOR*clongevity_relative_NOR

equation2 <- centropydiff_derivative_NOR - (centropyavg_derivative_NOR*entropydiffavg_original_NOR+
                                              entropyavgavg_original_NOR*
                                              (cvariation_derivative_NOR+clongevity_derivative_NOR))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_NOR <- variation_NOR
dlongevity_NOR <- longevity_NOR
dtotal_NOR <- dvariation_NOR + dlongevity_NOR

clongevity_NOR <- clongevity_derivative_NOR*entropyavgavg_original_NOR
cvariation_NOR <- cvariation_derivative_NOR*entropyavgavg_original_NOR
centropychange_NOR <- centropyavg_derivative_NOR*entropydiffavg_original_NOR
ctotal_NOR <- cvariation_NOR+clongevity_NOR+centropychange_NOR


### Finland ####
## decomp of contributions ###

entropydiff_FIN <- entropyCAL6-CALentropyavg

entropyavg_FIN <- (entropyCAL6+CALentropyavg)/2

dispersion_FIN <- log(CALdagger6/CALdaggeravg)
measure_FIN <- log(CAL6/CALavg)

variation_FIN <- dispersion_FIN*entropyavg_FIN
longevity_FIN <- (measure_FIN*entropyavg_FIN)*-1

equation1 <- entropydiff_FIN - (dispersion_FIN-measure_FIN)*entropyavg_FIN

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_FIN <- log(entropydiff_FIN[2:15]/entropydiff_FIN[1:14])/2
centropydiff_original_FIN <- entropydiff_FIN[1:14]*exp(1)^(centropydiff_relative_FIN)
centropydiff_derivative_FIN <- centropydiff_relative_FIN*centropydiff_original_FIN

#change in entropy average
centropyavg_relative_FIN <- log(entropyavg_FIN[2:15]/entropyavg_FIN[1:14])/2
centropyavg_original_FIN <- entropyavg_FIN[1:14]*exp(1)^(centropyavg_relative_FIN)
centropyavg_derivative_FIN <- centropyavg_relative_FIN*centropyavg_original_FIN

#average in entropy difference
entropydiffavg_relative_FIN <- log(entropydiff_FIN[2:15]/entropydiff_FIN[1:14])/2
entropydiffavg_original_FIN <- entropydiff_FIN[1:14]*exp(1)^(entropydiffavg_relative_FIN)

#average in average in entropy
entropyavgavg_relative_FIN <- log(entropyavg_FIN[2:15]/entropyavg_FIN[1:14])/2
entropyavgavg_original_FIN <- entropyavg_FIN[1:14]*exp(1)^(entropyavgavg_relative_FIN)


cvariation_relative_FIN <- log(variation_FIN[2:15]/variation_FIN[1:14])/2
cvariation_original_FIN <- variation_FIN[1:14]*exp(1)^(cvariation_relative_FIN)
cvariation_derivative_FIN <- cvariation_relative_FIN*cvariation_original_FIN

clongevity_relative_FIN <- log(longevity_FIN[2:15]/longevity_FIN[1:14])/2
clongevity_original_FIN <- longevity_FIN[1:14]*exp(1)^(clongevity_relative_FIN)
clongevity_derivative_FIN <- clongevity_original_FIN*clongevity_relative_FIN


equation2 <- centropydiff_derivative_FIN - (centropyavg_derivative_FIN*entropydiffavg_original_FIN+
                                              entropyavgavg_original_FIN*
                                              (cvariation_derivative_FIN+clongevity_derivative_FIN))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_FIN <- variation_FIN
dlongevity_FIN <- longevity_FIN
dtotal_FIN <- dvariation_FIN + dlongevity_FIN

clongevity_FIN <- clongevity_derivative_FIN*entropyavgavg_original_FIN
cvariation_FIN <- cvariation_derivative_FIN*entropyavgavg_original_FIN
centropychange_FIN <- centropyavg_derivative_FIN*entropydiffavg_original_FIN
ctotal_FIN <- cvariation_FIN+clongevity_FIN+centropychange_FIN


### Italy ####
## decomp of contributions ###

entropydiff_ITA <- entropyCAL7-CALentropyavg

entropyavg_ITA <- (entropyCAL7+CALentropyavg)/2

dispersion_ITA <- log(CALdagger7/CALdaggeravg)
measure_ITA <- log(CAL7/CALavg)

variation_ITA <- dispersion_ITA*entropyavg_ITA
longevity_ITA <- (measure_ITA*entropyavg_ITA)*-1

equation1 <- entropydiff_ITA - (dispersion_ITA - measure_ITA)*entropyavg_ITA

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_ITA <- log(entropydiff_ITA[2:15]/entropydiff_ITA[1:14])/2
centropydiff_original_ITA <- entropydiff_ITA[1:14]*exp(1)^(centropydiff_relative_ITA)
centropydiff_derivative_ITA <- centropydiff_relative_ITA*centropydiff_original_ITA

#change in entropy average
centropyavg_relative_ITA <- log(entropyavg_ITA[2:15]/entropyavg_ITA[1:14])/2
centropyavg_original_ITA <- entropyavg_ITA[1:14]*exp(1)^(centropyavg_relative_ITA)
centropyavg_derivative_ITA <- centropyavg_relative_ITA*centropyavg_original_ITA

#average in entropy difference
entropydiffavg_relative_ITA <- log(entropydiff_ITA[2:15]/entropydiff_ITA[1:14])/2
entropydiffavg_original_ITA <- entropydiff_ITA[1:14]*exp(1)^(entropydiffavg_relative_ITA)

#average in average in entropy
entropyavgavg_relative_ITA <- log(entropyavg_ITA[2:15]/entropyavg_ITA[1:14])/2
entropyavgavg_original_ITA <- entropyavg_ITA[1:14]*exp(1)^(entropyavgavg_relative_ITA)

#longevity and variation components
cvariation_relative_ITA <- log(variation_ITA[2:15]/variation_ITA[1:14])/2
cvariation_original_ITA <- variation_ITA[1:14]*exp(1)^(cvariation_relative_ITA)
cvariation_derivative_ITA <- cvariation_relative_ITA*cvariation_original_ITA

clongevity_relative_ITA <- log(longevity_ITA[2:15]/longevity_ITA[1:14])/2
clongevity_original_ITA <- longevity_ITA[1:14]*exp(1)^(clongevity_relative_ITA)
clongevity_derivative_ITA <- clongevity_original_ITA*clongevity_relative_ITA

equation2 <- centropydiff_derivative_ITA - (centropyavg_derivative_ITA*entropydiffavg_original_ITA+
                                              entropyavgavg_original_ITA*
                                              (cvariation_derivative_ITA+clongevity_derivative_ITA))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_ITA <- variation_ITA
dlongevity_ITA <- longevity_ITA
dtotal_ITA <- dvariation_ITA + dlongevity_ITA

clongevity_ITA <- clongevity_derivative_ITA*entropyavgavg_original_ITA
cvariation_ITA <- cvariation_derivative_ITA*entropyavgavg_original_ITA
centropychange_ITA <- centropyavg_derivative_ITA*entropydiffavg_original_ITA
ctotal_ITA <- cvariation_ITA+clongevity_ITA+centropychange_ITA


### Scotland ####
## decomp of contributions ###
entropydiff_GBRSCO <- entropyCAL8-CALentropyavg

entropyavg_GBRSCO <- (entropyCAL8+CALentropyavg)/2

dispersion_GBRSCO <- log(CALdagger8/CALdaggeravg)
measure_GBRSCO <- log(CAL8/CALavg)

variation_GBRSCO <- dispersion_GBRSCO*entropyavg_GBRSCO
longevity_GBRSCO <- (measure_GBRSCO*entropyavg_GBRSCO)*-1

equation1 <- entropydiff_GBRSCO - (dispersion_GBRSCO - measure_GBRSCO)*entropyavg_GBRSCO

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_GBRSCO <- log(entropydiff_GBRSCO[2:15]/entropydiff_GBRSCO[1:14])/2
centropydiff_original_GBRSCO <- entropydiff_GBRSCO[1:14]*exp(1)^(centropydiff_relative_GBRSCO)
centropydiff_derivative_GBRSCO <- centropydiff_relative_GBRSCO*centropydiff_original_GBRSCO

#change in entropy average
centropyavg_relative_GBRSCO <- log(entropyavg_GBRSCO[2:15]/entropyavg_GBRSCO[1:14])/2
centropyavg_original_GBRSCO <- entropyavg_GBRSCO[1:14]*exp(1)^(centropyavg_relative_GBRSCO)
centropyavg_derivative_GBRSCO <- centropyavg_relative_GBRSCO*centropyavg_original_GBRSCO

#average in entropy difference
entropydiffavg_relative_GBRSCO <- log(entropydiff_GBRSCO[2:15]/entropydiff_GBRSCO[1:14])/2
entropydiffavg_original_GBRSCO <- entropydiff_GBRSCO[1:14]*exp(1)^(entropydiffavg_relative_GBRSCO)

#average in average in entropy
entropyavgavg_relative_GBRSCO <- log(entropyavg_GBRSCO[2:15]/entropyavg_GBRSCO[1:14])/2
entropyavgavg_original_GBRSCO <- entropyavg_GBRSCO[1:14]*exp(1)^(entropyavgavg_relative_GBRSCO)

#longevity and variation components
cvariation_relative_GBRSCO <- log(variation_GBRSCO[2:15]/variation_GBRSCO[1:14])/2
cvariation_original_GBRSCO <- variation_GBRSCO[1:14]*exp(1)^(cvariation_relative_GBRSCO)
cvariation_derivative_GBRSCO <- cvariation_relative_GBRSCO*cvariation_original_GBRSCO

clongevity_relative_GBRSCO <- log(longevity_GBRSCO[2:15]/longevity_GBRSCO[1:14])/2
clongevity_original_GBRSCO <- longevity_GBRSCO[1:14]*exp(1)^(clongevity_relative_GBRSCO)
clongevity_derivative_GBRSCO <- clongevity_original_GBRSCO*clongevity_relative_GBRSCO

equation2 <- centropydiff_derivative_GBRSCO - (centropyavg_derivative_GBRSCO*entropydiffavg_original_GBRSCO+
                                                 entropyavgavg_original_GBRSCO*
                                                 (cvariation_derivative_GBRSCO+clongevity_derivative_GBRSCO))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_GBRSCO <- variation_GBRSCO
dlongevity_GBRSCO <- longevity_GBRSCO
dtotal_GBRSCO <- dvariation_GBRSCO + dlongevity_GBRSCO

clongevity_GBRSCO <- clongevity_derivative_GBRSCO*entropyavgavg_original_GBRSCO
cvariation_GBRSCO <- cvariation_derivative_GBRSCO*entropyavgavg_original_GBRSCO
centropychange_GBRSCO <- centropyavg_derivative_GBRSCO*entropydiffavg_original_GBRSCO
ctotal_GBRSCO <- cvariation_GBRSCO+clongevity_GBRSCO+centropychange_GBRSCO


### Netherland ####
## decomp of contributions ###

entropydiff_NLD <- entropyCAL9-CALentropyavg

entropyavg_NLD <- (entropyCAL9+CALentropyavg)/2

dispersion_NLD <- log(CALdagger9/CALdaggeravg)
measure_NLD <- log(CAL9/CALavg)

variation_NLD <- dispersion_NLD*entropyavg_NLD
longevity_NLD <- (measure_NLD*entropyavg_NLD)*-1

equation1 <- entropydiff_NLD - (dispersion_NLD - measure_NLD)*entropyavg_NLD

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_NLD <- log(entropydiff_NLD[2:15]/entropydiff_NLD[1:14])/2
centropydiff_original_NLD <- entropydiff_NLD[1:14]*exp(1)^(centropydiff_relative_NLD)
centropydiff_derivative_NLD <- centropydiff_relative_NLD*centropydiff_original_NLD

#change in entropy average
centropyavg_relative_NLD <- log(entropyavg_NLD[2:15]/entropyavg_NLD[1:14])/2
centropyavg_original_NLD <- entropyavg_NLD[1:14]*exp(1)^(centropyavg_relative_NLD)
centropyavg_derivative_NLD <- centropyavg_relative_NLD*centropyavg_original_NLD

#average in entropy difference
entropydiffavg_relative_NLD <- log(entropydiff_NLD[2:15]/entropydiff_NLD[1:14])/2
entropydiffavg_original_NLD <- entropydiff_NLD[1:14]*exp(1)^(entropydiffavg_relative_NLD)

#average in average in entropy
entropyavgavg_relative_NLD <- log(entropyavg_NLD[2:15]/entropyavg_NLD[1:14])/2
entropyavgavg_original_NLD <- entropyavg_NLD[1:14]*exp(1)^(entropyavgavg_relative_NLD)


cvariation_relative_NLD <- log(variation_NLD[2:15]/variation_NLD[1:14])/2
cvariation_original_NLD <- variation_NLD[1:14]*exp(1)^(cvariation_relative_NLD)
cvariation_derivative_NLD <- cvariation_relative_NLD*cvariation_original_NLD

clongevity_relative_NLD <- log(longevity_NLD[2:15]/longevity_NLD[1:14])/2
clongevity_original_NLD <- longevity_NLD[1:14]*exp(1)^(clongevity_relative_NLD)
clongevity_derivative_NLD <- clongevity_original_NLD*clongevity_relative_NLD

equation2 <- centropydiff_derivative_NLD - (centropyavg_derivative_NLD*entropydiffavg_original_NLD+
                                              entropyavgavg_original_NLD*
                                              (cvariation_derivative_NLD+clongevity_derivative_NLD))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_NLD <- variation_NLD
dlongevity_NLD <- longevity_NLD
dtotal_NLD <- dvariation_NLD + dlongevity_NLD

clongevity_NLD <- clongevity_derivative_NLD*entropyavgavg_original_NLD
cvariation_NLD <- cvariation_derivative_NLD*entropyavgavg_original_NLD
centropychange_NLD <- centropyavg_derivative_NLD*entropydiffavg_original_NLD
ctotal_NLD <- cvariation_NLD+clongevity_NLD+centropychange_NLD

### Switzerland ####
## decomp of contributions ###

entropydiff_CHE <- entropyCAL10-CALentropyavg

entropyavg_CHE <- (entropyCAL10+CALentropyavg)/2

dispersion_CHE <- log(CALdagger10/CALdaggeravg)
measure_CHE <- log(CAL10/CALavg)

variation_CHE <- dispersion_CHE*entropyavg_CHE
longevity_CHE <- (measure_CHE*entropyavg_CHE)*-1

equation1 <- entropydiff_CHE - (dispersion_CHE - measure_CHE)*entropyavg_CHE

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_CHE <- log(entropydiff_CHE[2:15]/entropydiff_CHE[1:14])/2
centropydiff_original_CHE <- entropydiff_CHE[1:14]*exp(1)^(centropydiff_relative_CHE)
centropydiff_derivative_CHE <- centropydiff_relative_CHE*centropydiff_original_CHE

#change in entropy average
centropyavg_relative_CHE <- log(entropyavg_CHE[2:15]/entropyavg_CHE[1:14])/2
centropyavg_original_CHE <- entropyavg_CHE[1:14]*exp(1)^(centropyavg_relative_CHE)
centropyavg_derivative_CHE <- centropyavg_relative_CHE*centropyavg_original_CHE

#average in entropy difference
entropydiffavg_relative_CHE <- log(entropydiff_CHE[2:15]/entropydiff_CHE[1:14])/2
entropydiffavg_original_CHE <- entropydiff_CHE[1:14]*exp(1)^(entropydiffavg_relative_CHE)

#average in average in entropy
entropyavgavg_relative_CHE <- log(entropyavg_CHE[2:15]/entropyavg_CHE[1:14])/2
entropyavgavg_original_CHE <- entropyavg_CHE[1:14]*exp(1)^(entropyavgavg_relative_CHE)


cvariation_relative_CHE <- log(variation_CHE[2:15]/variation_CHE[1:14])/2
cvariation_original_CHE <- variation_CHE[1:14]*exp(1)^(cvariation_relative_CHE)
cvariation_derivative_CHE <- cvariation_relative_CHE*cvariation_original_CHE

clongevity_relative_CHE <- log(longevity_CHE[2:15]/longevity_CHE[1:14])/2
clongevity_original_CHE <- longevity_CHE[1:14]*exp(1)^(clongevity_relative_CHE)
clongevity_derivative_CHE <- clongevity_original_CHE*clongevity_relative_CHE

equation2 <- centropydiff_derivative_CHE - (centropyavg_derivative_CHE*entropydiffavg_original_CHE+
                                              entropyavgavg_original_CHE*
                                              (cvariation_derivative_CHE+clongevity_derivative_CHE))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_CHE <- variation_CHE
dlongevity_CHE <- longevity_CHE
dtotal_CHE <- dvariation_CHE + dlongevity_CHE

clongevity_CHE <- clongevity_derivative_CHE*entropyavgavg_original_CHE
cvariation_CHE <- cvariation_derivative_CHE*entropyavgavg_original_CHE
centropychange_CHE <- centropyavg_derivative_CHE*entropydiffavg_original_CHE
ctotal_CHE <- cvariation_CHE+clongevity_CHE+centropychange_CHE


### United States ####
entropydiff_USA <- entropyCAL11-CALentropyavg[12:15]

entropyavg_USA <- (entropyCAL11+CALentropyavg[12:15])/2

dispersion_USA <- log(CALdagger11/CALdaggeravg[12:15])
measure_USA <- log(CAL11/CALavg[12:15])

variation_USA <- dispersion_USA*entropyavg_USA
longevity_USA <- (measure_USA*entropyavg_USA)*-1

equation1 <- entropydiff_USA - (dispersion_USA - measure_USA)*entropyavg_USA

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_USA <- log(entropydiff_USA[2:4]/entropydiff_USA[1:3])/2
centropydiff_original_USA <- entropydiff_USA[1:3]*exp(1)^(centropydiff_relative_USA)
centropydiff_derivative_USA <- centropydiff_relative_USA*centropydiff_original_USA

#change in entropy average
centropyavg_relative_USA <- log(entropyavg_USA[2:4]/entropyavg_USA[1:3])/2
centropyavg_original_USA <- entropyavg_USA[1:3]*exp(1)^(centropyavg_relative_USA)
centropyavg_derivative_USA <- centropyavg_relative_USA*centropyavg_original_USA

#average in entropy difference
entropydiffavg_relative_USA <- log(entropydiff_USA[2:4]/entropydiff_USA[1:3])/2
entropydiffavg_original_USA <- entropydiff_USA[1:3]*exp(1)^(entropydiffavg_relative_USA)

#average in average in entropy
entropyavgavg_relative_USA <- log(entropyavg_USA[2:4]/entropyavg_USA[1:3])/2
entropyavgavg_original_USA <- entropyavg_USA[1:3]*exp(1)^(entropyavgavg_relative_USA)


cvariation_relative_USA <- log(variation_USA[2:4]/variation_USA[1:3])/2
cvariation_original_USA <- variation_USA[1:3]*exp(1)^(cvariation_relative_USA)
cvariation_derivative_USA <- cvariation_relative_USA*cvariation_original_USA

clongevity_relative_USA <- log(longevity_USA[2:4]/longevity_USA[1:3])/2
clongevity_original_USA <- longevity_USA[1:3]*exp(1)^(clongevity_relative_USA)
clongevity_derivative_USA <- clongevity_original_USA*clongevity_relative_USA

equation2 <- centropydiff_derivative_USA - (centropyavg_derivative_USA*entropydiffavg_original_USA+
                                              entropyavgavg_original_USA*
                                              (cvariation_derivative_USA+clongevity_derivative_USA))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_USA <- c(rep(NA,11),variation_USA)
dlongevity_USA <- c(rep(NA,11),longevity_USA)
dtotal_USA <- dvariation_USA + dlongevity_USA

clongevity_USA <- c(rep(NA,11),clongevity_derivative_USA*entropyavgavg_original_USA)
cvariation_USA <- c(rep(NA,11),cvariation_derivative_USA*entropyavgavg_original_USA)
centropychange_USA <- c(rep(NA,11),centropyavg_derivative_USA*entropydiffavg_original_USA)
ctotal_USA <- c(cvariation_USA+clongevity_USA+centropychange_USA)

# ### formula ####
# ## decomposition in differences
# total <- variation+longevity
# ## decomposition in change in differences
# longevitypart <- clongevity_derivative*entropyavgavg_original
# variationpart <- cvariation_derivative*entropyavgavg_original
# entropychange <- centropyavg_derivative*entropydiffavg_original
# total <- variationpart+longevitypart+entropychange

### differences figure ####
difference <- data.frame(
  c(rep("SWE",45),rep("DNK",45),rep("FRA",45),rep("GBRTENW",45),rep("NOR",45),
    rep("FIN",45),rep("ITA",45),rep("GBRSCO",45),rep("NLD",45),rep("CHE",45),
    rep("USA",45)),
  rep(rep(seq(1989,2017,2),3),11),
  rep(c(rep("2.Lifespan variation",15),rep("1.Longevity",15),rep("total",15)),11),
  c(dvariation_SWE,dlongevity_SWE,dtotal_SWE,
    dvariation_DNK,dlongevity_DNK,dtotal_DNK,
    dvariation_FRA,dlongevity_FRA,dtotal_FRA,
    dvariation_GBRTENW,dlongevity_GBRTENW,dtotal_GBRTENW,
    dvariation_NOR,dlongevity_NOR,dtotal_NOR,
    dvariation_FIN,dlongevity_FIN,dtotal_FIN,
    dvariation_ITA,dlongevity_ITA,dtotal_ITA,
    dvariation_GBRSCO,dlongevity_GBRSCO,dtotal_GBRSCO,
    dvariation_NLD,dlongevity_NLD,dtotal_NLD,
    dvariation_CHE,dlongevity_CHE,dtotal_CHE,
    dvariation_USA,dlongevity_USA,dtotal_USA
    
  ))
colnames(difference) <- c("population","year","type","relative_disparities")


ggplot(data =difference)+
  geom_col(data = subset(difference,type!="total"), mapping = aes(x=year,y=relative_disparities,fill=type),position = "stack")+
  geom_line(data = subset(difference,type=="total"),mapping = aes(x=year,y=relative_disparities),lwd=1,)+
  geom_point(data = subset(difference,type=="total"),mapping = aes(x=year,y=relative_disparities))+
  facet_wrap(~population)+
  scale_fill_manual(values = c("blue","red"))+
  theme(legend.position = c(1,0),legend.justification = c(1,0),
        legend.background = element_blank())+
  labs(x="Year",y="Relative Disparities",
       title="Decomposition of differences, male 1989-2017")
# ggsave("Output/decomposition of differences, male 1989-2017.png",
#        width = 6,height = 8,dpi = 300)

### changes figure ####
change <- data.frame(
  c(rep("SWE",56),rep("DNK",56),rep("FRA",56),rep("GBRTENW",56),rep("NOR",56),
    rep("FIN",56),rep("ITA",56),rep("GBRSCO",56),rep("NLD",56),rep("CHE",56),
    rep("USA",56)),
  rep(rep(seq(1990,2017,2),11),4),
  rep(c(rep("3.Lifespan variation",14),rep("2.Longevity",14),
        rep("1.Average entropy imrprovements",14),rep("total",14)),11),
  c(cvariation_SWE,clongevity_SWE,centropychange_SWE,ctotal_SWE,
    cvariation_DNK,clongevity_DNK,centropychange_DNK,ctotal_DNK,
    cvariation_FRA,clongevity_FRA,centropychange_FRA,ctotal_FRA,
    cvariation_GBRTENW,clongevity_GBRTENW,centropychange_GBRTENW,ctotal_GBRTENW,
    cvariation_NOR,clongevity_NOR,centropychange_NOR,ctotal_NOR,
    cvariation_FIN,clongevity_FIN,centropychange_FIN,ctotal_FIN,
    cvariation_ITA,clongevity_ITA,centropychange_ITA,ctotal_ITA,
    cvariation_GBRSCO,clongevity_GBRSCO,centropychange_GBRSCO,ctotal_GBRSCO,
    cvariation_NLD,clongevity_NLD,centropychange_NLD,ctotal_NLD,
    cvariation_CHE,clongevity_CHE,centropychange_CHE,ctotal_CHE,
    cvariation_USA,clongevity_USA,centropychange_USA,ctotal_USA
  ))
colnames(change) <- c("population","year","type","relative_disparities")

change$relative_disparities <- change$relative_disparities*100

ggplot(data =change)+
  geom_col(data = subset(change,type!="total"), mapping = aes(x=year,y=relative_disparities,fill=type),position = "stack")+
  geom_line(data = subset(change,type=="total"),mapping = aes(x=year,y=relative_disparities),lwd=1,)+
  geom_point(data = subset(change,type=="total"),mapping = aes(x=year,y=relative_disparities))+
  facet_wrap(~population)+
  scale_fill_manual(values = c("green4","blue","red"))+
  theme(legend.position = c(1,0),legend.justification = c(1,0),
        legend.background = element_blank())+
  labs(x="Year",y="Relative Disparities",
       title="Decomposition of changes in differences,
       male 1990-2016")
# ggsave("Output/decomposition of changes in differences, male 1990-2016.png",
#        width = 6,height = 8,dpi = 300)