#FACT CHECKS

#### FEAMALE
source("main/Figure 3 - decomp female 1989-2017.R")

Year_diff <- c(seq(1989,2017,2))
Year_change <- c(seq(1990,2016,2))

# The slope of CHE and NOR
lm(dlongevity_NLD~Year_diff)
lm(dlongevity_NOR~Year_diff)
lm(dlongevity_CHE~Year_diff)
lm(dlongevity_SWE~Year_diff)

lm(dvariation_NLD~Year_diff)
lm(dvariation_NOR~Year_diff)
lm(dvariation_CHE~Year_diff)
lm(dvariation_SWE~Year_diff)

# France's changing time
dlongevity_FRA
Year_diff

# THe Scottish proportion of contribution
dlongevity_GBRSCO/
  (dlongevity_GBRSCO+dvariation_GBRSCO)

# Italy approaching average
dlongevity_ITA/
  (dlongevity_ITA+dvariation_ITA)

# low-inequality change in contribution shares
abs(cvariation_NLD)/abs(ctotal_NLD)
abs(cvariation_NOR)/abs(ctotal_NOR)
abs(cvariation_SWE)/abs(ctotal_SWE)
abs(cvariation_CHE)/abs(ctotal_CHE)

# Denmark change in diff
abs(cvariation_DNK)/
  (abs(cvariation_DNK)+
                       abs(clongevity_DNK)+abs(centropychange_DNK))
abs(clongevity_DNK)/
  (abs(cvariation_DNK)+
                       abs(clongevity_DNK)+abs(centropychange_DNK))

# Italy proportion in changes
(abs(clongevity_ITA)+abs(centropychange_ITA))/
  (abs(cvariation_ITA)+
                       abs(clongevity_ITA)+abs(centropychange_ITA))
cvariation_ITA

# US proportion to changes
abs(cvariation_USA)/
  (abs(cvariation_USA)+
     abs(clongevity_USA)+abs(centropychange_USA))
# France & Finland Proportion
abs(cvariation_FRA)/
  (abs(cvariation_FRA)+
     abs(clongevity_FRA)+abs(centropychange_FRA))
abs(clongevity_FRA)/
  (abs(cvariation_FRA)+
     abs(clongevity_FRA)+abs(centropychange_FRA))
abs(centropychange_FRA)/
  (abs(cvariation_FRA)+
     abs(clongevity_FRA)+abs(centropychange_FRA))
ctotal_FRA


#### MALE 
rm(list = ls())
source("main/Figure 3 - decomp male 1989-2017.R")
