#FACT CHECKS

#### FEAMALE ####

if(!require(zoo)){
  install.packages("zoo")
  library(zoo)}else{
    library(zoo)}

rm(list = ls())
source("main/Figure 4 - decomp female 1989-2017.R")

Year_diff <- c(seq(1989,2018))
Year_change <- c(seq(1990,2018))

# The speed of CHE and NOR

# Sweden
rr.dvariation_SWE <- mean(log(dvariation_SWE[-1]/
                                dvariation_SWE[-length(dvariation_SWE)]))
rr.dlongevity_SWE <- mean(log(dlongevity_SWE[-1]/
                                dlongevity_SWE[-length(dlongevity_SWE)]))
rr.total_SWE <- mean(log(dtotal_SWE[-1]/
                           dtotal_SWE[-length(dtotal_SWE)]))
# Denmark
rr.dvariation_NLD <- mean(log(dvariation_NLD[-1]/
                                dvariation_NLD[-length(dvariation_NLD)]))
rr.dlongevity_NLD <- mean(log(dlongevity_NLD[-1]/
                                dlongevity_NLD[-length(dlongevity_NLD)]))
rr.total_NLD <- mean(log(dtotal_NLD[-1]/
                           dtotal_NLD[-length(dtotal_NLD)]))
# Switzerland
rr.dvariation_CHE <- mean(log(dvariation_CHE[-1]/
                                dvariation_CHE[-length(dvariation_CHE)]))
rr.dlongevity_CHE <- mean(log(dlongevity_CHE[-1]/
                                dlongevity_CHE[-length(dlongevity_CHE)]))
rr.total_CHE <- mean(log(dtotal_CHE[-1]/
                           dtotal_CHE[-length(dtotal_CHE)]))
# Norway
rr.dvariation_NOR <- mean(log(dvariation_NOR[-1]/
                                dvariation_NOR[-length(dvariation_NOR)]))
rr.dlongevity_NOR <- mean(log(dlongevity_NOR[-1]/
                                dlongevity_NOR[-length(dlongevity_NOR)]))
rr.total_NOR <- mean(log(dtotal_NOR[-1]/
                           dtotal_NOR[-length(dtotal_NOR)]))

# France's changing time
names(dlongevity_FRA) <- Year_diff
dlongevity_FRA

# THe Scottish proportion of contribution
rollmean(abs(dlongevity_GBRSCO)/
        (abs(dlongevity_GBRSCO)+abs(dvariation_GBRSCO)),5)

rollmean(abs(dvariation_GBRSCO)/
        (abs(dlongevity_GBRSCO)+abs(dvariation_GBRSCO)),5)

# and France

rollmean(abs(dvariation_FRA)/
           (abs(dlongevity_FRA)+abs(dvariation_FRA)),5)

# Italy approaching average
rollmean(abs(dlongevity_ITA)/
        (abs(dlongevity_ITA)+abs(dvariation_ITA)),5)

# low-inequality change in contribution shares

rollmean(abs(cvariation_NLD)/abs(ctotal_NLD),5)
rollmean(abs(cvariation_NOR)/abs(ctotal_NOR),5)
rollmean(abs(cvariation_SWE)/abs(ctotal_SWE),5)
rollmean(abs(cvariation_CHE)/abs(ctotal_CHE),5)

# Denmark change in diff
rollmean(abs(cvariation_DNK)/
  (abs(cvariation_DNK)+abs(clongevity_DNK)+abs(centropychange_DNK)),5)
rollmean(abs(clongevity_DNK)/
  (abs(cvariation_DNK)+abs(clongevity_DNK)+abs(centropychange_DNK)),5)

# Italy proportion in changes
rollmean((abs(clongevity_ITA)+abs(centropychange_ITA))/
  (abs(cvariation_ITA)+abs(clongevity_ITA)+abs(centropychange_ITA)),5)
cvariation_ITA

# US proportion to changes
rollmean(abs(cvariation_USA)/
  (abs(cvariation_USA)+abs(clongevity_USA)+abs(centropychange_USA)),na.rm=T,5)

# France Proportion
rollmean(abs(cvariation_FRA)/
  (abs(cvariation_FRA)+abs(clongevity_FRA)+abs(centropychange_FRA)),5)

rollmean(abs(clongevity_FRA)/
  (abs(cvariation_FRA)+abs(clongevity_FRA)+abs(centropychange_FRA)),5)

rollmean(abs(centropychange_FRA)/
  (abs(cvariation_FRA)+abs(clongevity_FRA)+abs(centropychange_FRA)),5)

ctotal_FRA


#### MALE ####
rm(list = ls())
source("main/Figure 4 - decomp male 1989-2017.R")

### France and Italy check

mean(log(dtotal_FRA[-1]/dtotal_FRA[-length(dtotal_FRA)]))
mean(log(dtotal_ITA[-1]/dtotal_ITA[-length(dtotal_ITA)]))

abs(dvariation_FRA)/(abs(dvariation_FRA)+abs(dlongevity_FRA))
abs(dvariation_ITA)/(abs(dvariation_ITA)+abs(dlongevity_ITA))

### Finland
dtotal_ITA
abs(dvariation_FIN)/(abs(dvariation_FIN)+abs(dlongevity_FIN))

### Netherlands Norway, and Sweden
mean(log(dlongevity_NLD[-1]/dlongevity_NLD[-length(dtotal_NLD)]))
mean(log(dlongevity_NOR[-1]/dlongevity_NOR[-length(dtotal_NOR)]))
mean(log(dlongevity_SWE[-1]/dlongevity_SWE[-length(dtotal_SWE)]))

### Switzerland
abs(dlongevity_CHE)/(abs(dvariation_CHE)+abs(dlongevity_CHE))

### Scotland and US
abs(dvariation_GBRSCO)/(abs(dvariation_GBRSCO)+abs(dlongevity_GBRSCO))
abs(dvariation_USA)/(abs(dvariation_USA)+abs(dlongevity_USA))

### change now

### Denmark
dtotal_DNK
round(ctotal_DNK*100,4)
abs(cvariation_DNK)/
  (abs(cvariation_DNK)+
     abs(clongevity_DNK)+abs(centropychange_DNK))
abs(clongevity_DNK)/
  (abs(cvariation_DNK)+
     abs(clongevity_DNK)+abs(centropychange_DNK))
abs(centropychange_DNK)/
  (abs(cvariation_DNK)+
     abs(clongevity_DNK)+abs(centropychange_DNK))

### Finland
round(ctotal_FIN*100,4)
abs(cvariation_FIN)/
  (abs(cvariation_FIN)+
     abs(clongevity_FIN)+abs(centropychange_FIN))
abs(clongevity_FIN)/
  (abs(cvariation_FIN)+
     abs(clongevity_FIN)+abs(centropychange_FIN))
abs(centropychange_FIN)/
  (abs(cvariation_FIN)+
     abs(clongevity_FIN)+abs(centropychange_FIN))

### France
round(ctotal_FRA*100,4)
abs(cvariation_FRA)/
  (abs(cvariation_FRA)+
     abs(clongevity_FRA)+abs(centropychange_FRA))
abs(clongevity_FRA)/
  (abs(cvariation_FRA)+
     abs(clongevity_FRA)+abs(centropychange_FRA))
abs(centropychange_FRA)/
  (abs(cvariation_FRA)+
     abs(clongevity_FRA)+abs(centropychange_FRA))

### Italy
round(ctotal_ITA*100,4)
abs(cvariation_ITA)/
  (abs(cvariation_ITA)+
     abs(clongevity_ITA)+abs(centropychange_ITA))
abs(clongevity_ITA)/
  (abs(cvariation_ITA)+
     abs(clongevity_ITA)+abs(centropychange_ITA))
abs(centropychange_ITA)/
  (abs(cvariation_ITA)+
     abs(clongevity_ITA)+abs(centropychange_ITA))

### England & Wales
abs(cvariation_GBRTENW)/
  (abs(cvariation_GBRTENW)+
     abs(clongevity_GBRTENW)+abs(centropychange_GBRTENW))

### Netherlands
round(ctotal_NLD*100,4)
abs(cvariation_NLD)/
  (abs(cvariation_NLD)+
     abs(clongevity_NLD)+abs(centropychange_NLD))
abs(clongevity_NLD)/
  (abs(cvariation_NLD)+
     abs(clongevity_NLD)+abs(centropychange_NLD))
abs(centropychange_NLD)/
  (abs(cvariation_NLD)+
     abs(clongevity_NLD)+abs(centropychange_NLD))

### Norway & Sweden
rr.ctotal_SWE <- mean(log(ctotal_SWE[-1]/
                                ctotal_SWE[-length(ctotal_SWE)]))
rr.ctotal_NOR <- mean(log(ctotal_NOR[-1]/
                                ctotal_NOR[-length(ctotal_NOR)]))
rr.cvariation_SWE <- log(cvariation_SWE[-1]/cvariation_SWE[-length(cvariation_SWE)])
rr.cvariation_SWE <- mean(rr.cvariation_SWE[!is.nan(rr.cvariation_SWE)])
rr.clongevity_SWE <- mean(log(clongevity_SWE[-1]/
                                clongevity_SWE[-length(clongevity_SWE)]))
rr.centropychange_SWE <- mean(log(centropychange_SWE[-1]/
                                    centropychange_SWE[-length(centropychange_SWE)]))
rr.clongevity_NOR <- mean(log(clongevity_NOR[-1]/
                                clongevity_NOR[-length(clongevity_NOR)]))

### Switzerland
abs(cvariation_CHE)/
  (abs(cvariation_CHE)+
     abs(clongevity_CHE)+abs(centropychange_CHE))

### leveling off
round(ctotal_NLD*100,4)
round(ctotal_NOR*100,4)
round(ctotal_SWE*100,4)
round(ctotal_CHE*100,4)

### Scotland & US
abs(cvariation_GBRSCO)/
  (abs(cvariation_GBRSCO)+
     abs(clongevity_GBRSCO)+abs(centropychange_GBRSCO))
abs(cvariation_USA)/
  (abs(cvariation_USA)+
     abs(clongevity_USA)+abs(centropychange_USA))
