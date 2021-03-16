#### Relative rate of increase ####

## Source
source("Figures/figure 3 - decomp female 1989-2017.R")

## rate of increase ###

# Sweden
rr.dvariation_SWE <- mean(log(dvariation_SWE[-1]/
                                dvariation_SWE[-length(dvariation_SWE)]))
rr.dlongevity_SWE <- mean(log(dlongevity_SWE[-1]/
                                dlongevity_SWE[-length(dlongevity_SWE)]))
rr.total_SWE <- mean(log(dtotal_SWE[-1]/
                           dtotal_SWE[-length(dtotal_SWE)]))
rr.cvariation_SWE <- mean(log(cvariation_SWE[-1]/
                                cvariation_SWE[-length(cvariation_SWE)]))
rr.clongevity_SWE <- mean(log(clongevity_SWE[-1]/
                                clongevity_SWE[-length(clongevity_SWE)]))
rr.centropychange_SWE <- mean(log(centropychange_SWE[-1]/
                                    centropychange_SWE[-length(centropychange_SWE)]))
rr.ctotal_SWE <- mean(log(ctotal_SWE[-1]/
                            ctotal_SWE[-length(ctotal_SWE)]))
# Denmark
rr.dvariation_DNK <- mean(log(dvariation_DNK[-1]/
                                dvariation_DNK[-length(dvariation_DNK)]))
rr.dlongevity_DNK <- mean(log(dlongevity_DNK[-1]/
                                dlongevity_DNK[-length(dlongevity_DNK)]))
rr.total_DNK <- mean(log(dtotal_DNK[-1]/
                           dtotal_DNK[-length(dtotal_DNK)]))
rr.cvariation_DNK <- mean(log(cvariation_DNK[-1]/
                                cvariation_DNK[-length(cvariation_DNK)]))
rr.clongevity_DNK <- mean(log(clongevity_DNK[-1]/
                                clongevity_DNK[-length(clongevity_DNK)]))
rr.centropychange_DNK <- mean(log(centropychange_DNK[-1]/
                                    centropychange_DNK[-length(centropychange_DNK)]))
rr.ctotal_DNK <- mean(log(ctotal_DNK[-1]/
                            ctotal_DNK[-length(ctotal_DNK)]))
# Finland
rr.dvariation_FIN <- mean(log(dvariation_FIN[-1]/
                                dvariation_FIN[-length(dvariation_FIN)]))
rr.dlongevity_FIN <- mean(log(dlongevity_FIN[-1]/
                                dlongevity_FIN[-length(dlongevity_FIN)]))
rr.total_FIN <- mean(log(dtotal_FIN[-1]/
                           dtotal_FIN[-length(dtotal_FIN)]))
rr.cvariation_FIN <- mean(log(cvariation_FIN[-1]/
                                cvariation_FIN[-length(cvariation_FIN)]))
rr.clongevity_FIN <- mean(log(clongevity_FIN[-1]/
                                clongevity_FIN[-length(clongevity_FIN)]))
rr.centropychange_FIN <- mean(log(centropychange_FIN[-1]/
                                    centropychange_FIN[-length(centropychange_FIN)]))
rr.ctotal_FIN <- mean(log(ctotal_FIN[-1]/
                            ctotal_FIN[-length(ctotal_FIN)]))
# France
