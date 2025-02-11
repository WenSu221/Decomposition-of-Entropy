#### Relative rate of increase ####

## Source
source("main/figure 4 - decomp female 1989-2017.R")

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
rr.dvariation_NLD <- mean(log(dvariation_NLD[-1]/
                                dvariation_NLD[-length(dvariation_NLD)]))
rr.dlongevity_NLD <- mean(log(dlongevity_NLD[-1]/
                                dlongevity_NLD[-length(dlongevity_NLD)]))
rr.total_NLD <- mean(log(dtotal_NLD[-1]/
                           dtotal_NLD[-length(dtotal_NLD)]))
rr.cvariation_NLD <- mean(log(cvariation_NLD[-1]/
                                cvariation_NLD[-length(cvariation_NLD)]))
rr.clongevity_NLD <- mean(log(clongevity_NLD[-1]/
                                clongevity_NLD[-length(clongevity_NLD)]))
rr.centropychange_NLD <- mean(log(centropychange_NLD[-1]/
                                    centropychange_NLD[-length(centropychange_NLD)]))
rr.ctotal_NLD <- mean(log(ctotal_NLD[-1]/
                            ctotal_NLD[-length(ctotal_NLD)]))
# Switzerland
rr.dvariation_CHE <- mean(log(dvariation_CHE[-1]/
                                dvariation_CHE[-length(dvariation_CHE)]))
rr.dlongevity_CHE <- mean(log(dlongevity_CHE[-1]/
                                dlongevity_CHE[-length(dlongevity_CHE)]))
rr.total_CHE <- mean(log(dtotal_CHE[-1]/
                           dtotal_CHE[-length(dtotal_CHE)]))
rr.cvariation_CHE <- mean(log(cvariation_CHE[-1]/
                                cvariation_CHE[-length(cvariation_CHE)]))
rr.clongevity_CHE <- mean(log(clongevity_CHE[-1]/
                                clongevity_CHE[-length(clongevity_CHE)]))
rr.centropychange_CHE <- mean(log(centropychange_CHE[-1]/
                                    centropychange_CHE[-length(centropychange_CHE)]))
rr.ctotal_CHE <- mean(log(ctotal_CHE[-1]/
                            ctotal_CHE[-length(ctotal_CHE)]))
# Norway
rr.dvariation_NOR <- mean(log(dvariation_NOR[-1]/
                                dvariation_NOR[-length(dvariation_NOR)]))
rr.dlongevity_NOR <- mean(log(dlongevity_NOR[-1]/
                                dlongevity_NOR[-length(dlongevity_NOR)]))
rr.total_NOR <- mean(log(dtotal_NOR[-1]/
                           dtotal_NOR[-length(dtotal_NOR)]))
rr.cvariation_NOR <- mean(log(cvariation_NOR[-1]/
                                cvariation_NOR[-length(cvariation_NOR)]))
rr.clongevity_NOR <- mean(log(clongevity_NOR[-1]/
                                clongevity_NOR[-length(clongevity_NOR)]))
rr.centropychange_NOR <- mean(log(centropychange_NOR[-1]/
                                    centropychange_NOR[-length(centropychange_NOR)]))
rr.ctotal_NOR <- mean(log(ctotal_NOR[-1]/
                            ctotal_NOR[-length(ctotal_NOR)]))

