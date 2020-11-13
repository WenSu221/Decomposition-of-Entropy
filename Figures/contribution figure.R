#### contribution figure ####

### data ####

#change the data entry and then run the#
#code in "CAL calculation/entropy of CAL"#

### function ####

library(RColorBrewer)

coul <- brewer.pal(5,"Set1")

years <- seq(1992,2017,5)

longevity <- longevity
inequality <- inequality
total <- longevity+inequality
decomp <- rbind(longevity,inequality,total)
rownames(decomp) <- c("longevity component", "lifespan inequality component","total")
colnames(decomp) <- c(seq(1992,2017,5))

# decomp <- decomp*100
windows(16,9)
barplot(decomp,col = coul[1:3],border = "white",beside = T,
        xlab="Year",ylab = "relative disparities",
        legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
        main = "Decomposition of entropy of CAL between France and Italy, both 1957-2017",
        sub = "source: Author's calculation based on HMD data")