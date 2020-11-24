#### contribution figure ####

### data ####

#change the data entry and then run the#
#code in "CAL calculation/entropy of CAL"#

library(RColorBrewer)

coul <- brewer.pal(5,"Set1")


#### short data ####
years <- seq(1992,2017,5)

longevity <- longevity
inequality <- inequality
total <- longevity+inequality
decomp <- rbind(longevity,inequality,total)
rownames(decomp) <- c("longevity component", "lifespan inequality component","total")
colnames(decomp) <- c(seq(1992,2017,5))

windows(16,9)
bar.plot <- barplot(decomp[1:2,],col = coul[1:2],border = "white",
        xlab="Year",ylab = "relative disparities",
        # ylim = range(decomp[3,])*1.2, 
        ylim = c(0,max(decomp[3,])*1.2),
        legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
        main = "Decomposition of entropy of CAL between Sweden and Finland, both 1957-2017",
        sub = "source: Author's calculation based on HMD data")
lines(bar.plot,decomp[3,],lwd=1.5)
points(bar.plot,decomp[3,],pch=16)

#### Long data ####
years <- seq(1957,2017,5)

longevity <- longevity
inequality <- inequality
total <- longevity+inequality
decomp <- rbind(longevity,inequality,total)
rownames(decomp) <- c("longevity component", "lifespan inequality component","total")
colnames(decomp) <- c(seq(1957,2017,5))

windows(16,9)
bar.plot <- barplot(decomp[1:2,],col = coul[1:2],border = "white",
        xlab="Year",ylab = "relative disparities",
        ylim = range(decomp[3,])*1.2,
        # ylim = c(0,max(decomp[3,])*1.2),
        legend.text = TRUE,args.legend = list(x="bottomright",bg="white",box.col=0),
        main = "Decomposition of entropy of CAL between England&Wales and Denmark, both 1957-2017",
        sub = "source: Author's calculation based on HMD data")
lines(bar,decomp[3,],lwd=1.5)
points(bar,decomp[3,],pch=16)
