#### contribution figure ####

### data ####

#change the data entry and then run the#
#code in "CAL calculation/entropy of CAL"#

### function ####

library(RColorBrewer)

coul <- brewer.pal(4,"Set1")

years <- seq(1952,2017,5)

longevity <- longevity
inequality <- inequality
decomp <- rbind(longevity,inequality)
rownames(decomp) <- c("longevity component", "lifespan inequality component")
colnames(decomp) <- c(seq(1952,2017,5))

# decomp <- decomp*100
windows(8,5)
barplot(decomp,col = coul,border = "white",
        xlab="Year",ylab = "relative disparities",
        legend.text = TRUE,args.legend = list(x="topleft",bg="white"),
        main = "Decomposition of entropy of CAL between Sweden and Denmark, both 1952-2017",
        sub = "source: Author's calculation based on HMD data")
