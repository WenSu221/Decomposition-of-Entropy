#### ggplot2 population decomp, female####

##packages and color
library(ggplot2)
library(dplyr)

## Source
source("main/figure 4 - average of CAL female 1989-2017.R")
source("US Data/USCAL female.R")

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
centropydiff_relative_SWE <- log(entropydiff_SWE[2:30]/entropydiff_SWE[1:29])/2
centropydiff_original_SWE <- entropydiff_SWE[1:29]*exp(1)^(centropydiff_relative_SWE)
centropydiff_derivative_SWE <- centropydiff_relative_SWE*centropydiff_original_SWE

#change in entropy average
centropyavg_relative_SWE <- log(entropyavg_SWE[2:30]/entropyavg_SWE[1:29])/2
centropyavg_original_SWE <- entropyavg_SWE[1:29]*exp(1)^(centropyavg_relative_SWE)
centropyavg_derivative_SWE <- centropyavg_relative_SWE*centropyavg_original_SWE

#average in entropy difference
entropydiffavg_relative_SWE <- log(entropydiff_SWE[2:30]/entropydiff_SWE[1:29])/2
entropydiffavg_original_SWE <- entropydiff_SWE[1:29]*exp(1)^(entropydiffavg_relative_SWE)

#average in average in entropy
entropyavgavg_relative_SWE <- log(entropyavg_SWE[2:30]/entropyavg_SWE[1:29])/2
entropyavgavg_original_SWE <- entropyavg_SWE[1:29]*exp(1)^(entropyavgavg_relative_SWE)


cvariation_relative_SWE <- log(variation_SWE[2:30]/variation_SWE[1:29])/2
cvariation_original_SWE <- variation_SWE[1:29]*exp(1)^(cvariation_relative_SWE)
cvariation_derivative_SWE <- cvariation_relative_SWE*cvariation_original_SWE

clongevity_relative_SWE <- log(longevity_SWE[2:30]/longevity_SWE[1:29])/2
clongevity_original_SWE <- longevity_SWE[1:29]*exp(1)^(clongevity_relative_SWE)
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

centropydiff_DNK <- (entropydiff_DNK[2:30]-entropydiff_DNK[1:29])/2

centropyavg_DNK <- (entropyavg_DNK[2:30] - entropyavg_DNK[1:29])/2
entropydiffavg_DNK <- (entropydiff_DNK[1:29]+entropydiff_DNK[2:30])/2
entropyavgavg_DNK <- (entropyavg_DNK[1:29]+entropyavg_DNK[2:30])/2

cvariation_DNK <- (variation_DNK[2:30]-variation_DNK[1:29])/2
clongevity_DNK <- (longevity_DNK[2:30]-longevity_DNK[1:29])/2

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
centropydiff_relative_FRA <- log(entropydiff_FRA[2:30]/entropydiff_FRA[1:29])/2
centropydiff_original_FRA <- entropydiff_FRA[1:29]*exp(1)^(centropydiff_relative_FRA)
centropydiff_derivative_FRA <- centropydiff_relative_FRA*centropydiff_original_FRA

#change in entropy average
centropyavg_relative_FRA <- log(entropyavg_FRA[2:30]/entropyavg_FRA[1:29])/2
centropyavg_original_FRA <- entropyavg_FRA[1:29]*exp(1)^(centropyavg_relative_FRA)
centropyavg_derivative_FRA <- centropyavg_relative_FRA*centropyavg_original_FRA

#average in entropy difference
entropydiffavg_relative_FRA <- log(entropydiff_FRA[2:30]/entropydiff_FRA[1:29])/2
entropydiffavg_original_FRA <- entropydiff_FRA[1:29]*exp(1)^(entropydiffavg_relative_FRA)

#average in average in entropy
entropyavgavg_relative_FRA <- log(entropyavg_FRA[2:30]/entropyavg_FRA[1:29])/2
entropyavgavg_original_FRA <- entropyavg_FRA[1:29]*exp(1)^(entropyavgavg_relative_FRA)

#longevity and variation components
cvariation_relative_FRA <- log(variation_FRA[2:30]/variation_FRA[1:29])/2
cvariation_original_FRA <- variation_FRA[1:29]*exp(1)^(cvariation_relative_FRA)
cvariation_derivative_FRA <- cvariation_relative_FRA*cvariation_original_FRA

clongevity_derivative_FRA <- clongevity_FRA <- (longevity_FRA[2:30]-longevity_FRA[1:29])/2

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

centropydiff_GBRTENW <- (entropydiff_GBRTENW[2:30]-entropydiff_GBRTENW[1:29])/2

centropyavg_GBRTENW <- (entropyavg_GBRTENW[2:30] - entropyavg_GBRTENW[1:29])/2
entropydiffavg_GBRTENW <- (entropydiff_GBRTENW[1:29]+entropydiff_GBRTENW[2:30])/2
entropyavgavg_GBRTENW <- (entropyavg_GBRTENW[1:29]+entropyavg_GBRTENW[2:30])/2

cvariation_GBRTENW <- (variation_GBRTENW[2:30]-variation_GBRTENW[1:29])/2
clongevity_GBRTENW <- (longevity_GBRTENW[2:30]-longevity_GBRTENW[1:29])/2


equation2 <- centropydiff_GBRTENW - (centropyavg_GBRTENW*entropydiffavg_GBRTENW+
                                                  entropyavgavg_GBRTENW*
                                                  (cvariation_GBRTENW+clongevity_GBRTENW))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_GBRTENW <- variation_GBRTENW
dlongevity_GBRTENW <- longevity_GBRTENW
dtotal_GBRTENW <- dvariation_GBRTENW + dlongevity_GBRTENW

clongevity_GBRTENW <- clongevity_GBRTENW*entropyavgavg_GBRTENW
cvariation_GBRTENW <- cvariation_GBRTENW*entropyavgavg_GBRTENW
centropychange_GBRTENW <- centropyavg_GBRTENW*entropydiffavg_GBRTENW
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
centropydiff_relative_NOR <- log(entropydiff_NOR[2:30]/entropydiff_NOR[1:29])/2
centropydiff_original_NOR <- entropydiff_NOR[1:29]*exp(1)^(centropydiff_relative_NOR)
centropydiff_derivative_NOR <- centropydiff_relative_NOR*centropydiff_original_NOR

#change in entropy average
centropyavg_relative_NOR <- log(entropyavg_NOR[2:30]/entropyavg_NOR[1:29])/2
centropyavg_original_NOR <- entropyavg_NOR[1:29]*exp(1)^(centropyavg_relative_NOR)
centropyavg_derivative_NOR <- centropyavg_relative_NOR*centropyavg_original_NOR

#average in entropy difference
entropydiffavg_relative_NOR <- log(entropydiff_NOR[2:30]/entropydiff_NOR[1:29])/2
entropydiffavg_original_NOR <- entropydiff_NOR[1:29]*exp(1)^(entropydiffavg_relative_NOR)

#average in average in entropy
entropyavgavg_relative_NOR <- log(entropyavg_NOR[2:30]/entropyavg_NOR[1:29])/2
entropyavgavg_original_NOR <- entropyavg_NOR[1:29]*exp(1)^(entropyavgavg_relative_NOR)


cvariation_relative_NOR <- log(variation_NOR[2:30]/variation_NOR[1:29])/2
cvariation_original_NOR <- variation_NOR[1:29]*exp(1)^(cvariation_relative_NOR)
cvariation_derivative_NOR <- cvariation_relative_NOR*cvariation_original_NOR

clongevity_relative_NOR <- log(longevity_NOR[2:30]/longevity_NOR[1:29])/2
clongevity_original_NOR <- longevity_NOR[1:29]*exp(1)^(clongevity_relative_NOR)
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

centropydiff_derivative_FIN <- (entropydiff_FIN[2:30]-entropydiff_FIN[1:29])/2
centropyavg_derivative_FIN <- (entropyavg_FIN[2:30] - entropyavg_FIN[1:29])/2
entropydiffavg_original_FIN <- (entropydiff_FIN[1:29]+entropydiff_FIN[2:30])/2
entropyavgavg_original_FIN <- (entropyavg_FIN[1:29]+entropyavg_FIN[2:30])/2

cvariation_derivative_FIN <- (variation_FIN[2:30]-variation_FIN[1:29])/2
clongevity_derivative_FIN <- (longevity_FIN[2:30]-longevity_FIN[1:29])/2

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
centropydiff_relative_ITA <- log(entropydiff_ITA[2:30]/entropydiff_ITA[1:29])/2
centropydiff_original_ITA <- entropydiff_ITA[1:29]*exp(1)^(centropydiff_relative_ITA)
centropydiff_derivative_ITA <- centropydiff_relative_ITA*centropydiff_original_ITA

#change in entropy average
centropyavg_relative_ITA <- log(entropyavg_ITA[2:30]/entropyavg_ITA[1:29])/2
centropyavg_original_ITA <- entropyavg_ITA[1:29]*exp(1)^(centropyavg_relative_ITA)
centropyavg_derivative_ITA <- centropyavg_relative_ITA*centropyavg_original_ITA

#average in entropy difference
entropydiffavg_relative_ITA <- log(entropydiff_ITA[2:30]/entropydiff_ITA[1:29])/2
entropydiffavg_original_ITA <- entropydiff_ITA[1:29]*exp(1)^(entropydiffavg_relative_ITA)

#average in average in entropy
entropyavgavg_relative_ITA <- log(entropyavg_ITA[2:30]/entropyavg_ITA[1:29])/2
entropyavgavg_original_ITA <- entropyavg_ITA[1:29]*exp(1)^(entropyavgavg_relative_ITA)

#longevity and variation components
cvariation_relative_ITA <- log(variation_ITA[2:30]/variation_ITA[1:29])/2
cvariation_original_ITA <- variation_ITA[1:29]*exp(1)^(cvariation_relative_ITA)
cvariation_derivative_ITA <- cvariation_relative_ITA*cvariation_original_ITA

clongevity_relative_ITA <- log(longevity_ITA[2:30]/longevity_ITA[1:29])/2
clongevity_original_ITA <- longevity_ITA[1:29]*exp(1)^(clongevity_relative_ITA)
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
centropydiff_relative_GBRSCO <- log(entropydiff_GBRSCO[2:30]/entropydiff_GBRSCO[1:29])/2
centropydiff_original_GBRSCO <- entropydiff_GBRSCO[1:29]*exp(1)^(centropydiff_relative_GBRSCO)
centropydiff_derivative_GBRSCO <- centropydiff_relative_GBRSCO*centropydiff_original_GBRSCO

#change in entropy average
centropyavg_relative_GBRSCO <- log(entropyavg_GBRSCO[2:30]/entropyavg_GBRSCO[1:29])/2
centropyavg_original_GBRSCO <- entropyavg_GBRSCO[1:29]*exp(1)^(centropyavg_relative_GBRSCO)
centropyavg_derivative_GBRSCO <- centropyavg_relative_GBRSCO*centropyavg_original_GBRSCO

#average in entropy difference
entropydiffavg_relative_GBRSCO <- log(entropydiff_GBRSCO[2:30]/entropydiff_GBRSCO[1:29])/2
entropydiffavg_original_GBRSCO <- entropydiff_GBRSCO[1:29]*exp(1)^(entropydiffavg_relative_GBRSCO)

#average in average in entropy
entropyavgavg_relative_GBRSCO <- log(entropyavg_GBRSCO[2:30]/entropyavg_GBRSCO[1:29])/2
entropyavgavg_original_GBRSCO <- entropyavg_GBRSCO[1:29]*exp(1)^(entropyavgavg_relative_GBRSCO)

#longevity and variation components
cvariation_relative_GBRSCO <- log(variation_GBRSCO[2:30]/variation_GBRSCO[1:29])/2
cvariation_original_GBRSCO <- variation_GBRSCO[1:29]*exp(1)^(cvariation_relative_GBRSCO)
cvariation_derivative_GBRSCO <- cvariation_relative_GBRSCO*cvariation_original_GBRSCO

clongevity_relative_GBRSCO <- log(longevity_GBRSCO[2:30]/longevity_GBRSCO[1:29])/2
clongevity_original_GBRSCO <- longevity_GBRSCO[1:29]*exp(1)^(clongevity_relative_GBRSCO)
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


### Netherlands ####
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
centropydiff_relative_NLD <- log(entropydiff_NLD[2:30]/entropydiff_NLD[1:29])/2
centropydiff_original_NLD <- entropydiff_NLD[1:29]*exp(1)^(centropydiff_relative_NLD)
centropydiff_derivative_NLD <- centropydiff_relative_NLD*centropydiff_original_NLD

#change in entropy average
centropyavg_relative_NLD <- log(entropyavg_NLD[2:30]/entropyavg_NLD[1:29])/2
centropyavg_original_NLD <- entropyavg_NLD[1:29]*exp(1)^(centropyavg_relative_NLD)
centropyavg_derivative_NLD <- centropyavg_relative_NLD*centropyavg_original_NLD

#average in entropy difference
entropydiffavg_relative_NLD <- log(entropydiff_NLD[2:30]/entropydiff_NLD[1:29])/2
entropydiffavg_original_NLD <- entropydiff_NLD[1:29]*exp(1)^(entropydiffavg_relative_NLD)

#average in average in entropy
entropyavgavg_relative_NLD <- log(entropyavg_NLD[2:30]/entropyavg_NLD[1:29])/2
entropyavgavg_original_NLD <- entropyavg_NLD[1:29]*exp(1)^(entropyavgavg_relative_NLD)


cvariation_relative_NLD <- log(variation_NLD[2:30]/variation_NLD[1:29])/2
cvariation_original_NLD <- variation_NLD[1:29]*exp(1)^(cvariation_relative_NLD)
cvariation_derivative_NLD <- cvariation_relative_NLD*cvariation_original_NLD

clongevity_relative_NLD <- log(longevity_NLD[2:30]/longevity_NLD[1:29])/2
clongevity_original_NLD <- longevity_NLD[1:29]*exp(1)^(clongevity_relative_NLD)
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
centropydiff_relative_CHE <- log(entropydiff_CHE[2:30]/entropydiff_CHE[1:29])/2
centropydiff_original_CHE <- entropydiff_CHE[1:29]*exp(1)^(centropydiff_relative_CHE)
centropydiff_derivative_CHE <- centropydiff_relative_CHE*centropydiff_original_CHE

#change in entropy average
centropyavg_relative_CHE <- log(entropyavg_CHE[2:30]/entropyavg_CHE[1:29])/2
centropyavg_original_CHE <- entropyavg_CHE[1:29]*exp(1)^(centropyavg_relative_CHE)
centropyavg_derivative_CHE <- centropyavg_relative_CHE*centropyavg_original_CHE

#average in entropy difference
entropydiffavg_relative_CHE <- log(entropydiff_CHE[2:30]/entropydiff_CHE[1:29])/2
entropydiffavg_original_CHE <- entropydiff_CHE[1:29]*exp(1)^(entropydiffavg_relative_CHE)

#average in average in entropy
entropyavgavg_relative_CHE <- log(entropyavg_CHE[2:30]/entropyavg_CHE[1:29])/2
entropyavgavg_original_CHE <- entropyavg_CHE[1:29]*exp(1)^(entropyavgavg_relative_CHE)


cvariation_relative_CHE <- log(variation_CHE[2:30]/variation_CHE[1:29])/2
cvariation_original_CHE <- variation_CHE[1:29]*exp(1)^(cvariation_relative_CHE)
cvariation_derivative_CHE <- cvariation_relative_CHE*cvariation_original_CHE

clongevity_relative_CHE <- log(longevity_CHE[2:30]/longevity_CHE[1:29])/2
clongevity_original_CHE <- longevity_CHE[1:29]*exp(1)^(clongevity_relative_CHE)
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
entropydiff_USA <- entropyCAL11-CALentropyavg[23:30]

entropyavg_USA <- (entropyCAL11+CALentropyavg[23:30])/2

dispersion_USA <- log(CALdagger11/CALdaggeravg[23:30])
measure_USA <- log(CAL11/CALavg[23:30])

variation_USA <- dispersion_USA*entropyavg_USA
longevity_USA <- (measure_USA*entropyavg_USA)*-1

equation1 <- entropydiff_USA - (dispersion_USA - measure_USA)*entropyavg_USA

## decomp of changes in contributions ###

#change in entropy differences
centropydiff_relative_USA <- log(entropydiff_USA[2:8]/entropydiff_USA[1:7])/2
centropydiff_original_USA <- entropydiff_USA[1:7]*exp(1)^(centropydiff_relative_USA)
centropydiff_derivative_USA <- centropydiff_relative_USA*centropydiff_original_USA

#change in entropy average
centropyavg_relative_USA <- log(entropyavg_USA[2:8]/entropyavg_USA[1:7])/2
centropyavg_original_USA <- entropyavg_USA[1:7]*exp(1)^(centropyavg_relative_USA)
centropyavg_derivative_USA <- centropyavg_relative_USA*centropyavg_original_USA

#average in entropy difference
entropydiffavg_relative_USA <- log(entropydiff_USA[2:8]/entropydiff_USA[1:7])/2
entropydiffavg_original_USA <- entropydiff_USA[1:7]*exp(1)^(entropydiffavg_relative_USA)

#average in average in entropy
entropyavgavg_relative_USA <- log(entropyavg_USA[2:8]/entropyavg_USA[1:7])/2
entropyavgavg_original_USA <- entropyavg_USA[1:7]*exp(1)^(entropyavgavg_relative_USA)


cvariation_relative_USA <- log(variation_USA[2:8]/variation_USA[1:7])/2
cvariation_original_USA <- variation_USA[1:7]*exp(1)^(cvariation_relative_USA)
cvariation_derivative_USA <- cvariation_relative_USA*cvariation_original_USA

clongevity_relative_USA <- log(longevity_USA[2:8]/longevity_USA[1:7])/2
clongevity_original_USA <- longevity_USA[1:7]*exp(1)^(clongevity_relative_USA)
clongevity_derivative_USA <- clongevity_original_USA*clongevity_relative_USA

equation2 <- centropydiff_derivative_USA - (centropyavg_derivative_USA*entropydiffavg_original_USA+
                                              entropyavgavg_original_USA*
                                              (cvariation_derivative_USA+clongevity_derivative_USA))

## validation ###
round(equation1,5)
round(equation2,5)

## table ###
dvariation_USA <- c(rep(NA,22),variation_USA)
dlongevity_USA <- c(rep(NA,22),longevity_USA)
dtotal_USA <- dvariation_USA + dlongevity_USA

clongevity_USA <- c(rep(NA,22),clongevity_derivative_USA*entropyavgavg_original_USA)
cvariation_USA <- c(rep(NA,22),cvariation_derivative_USA*entropyavgavg_original_USA)
centropychange_USA <- c(rep(NA,22),centropyavg_derivative_USA*entropydiffavg_original_USA)
ctotal_USA <- c(cvariation_USA+clongevity_USA+centropychange_USA)