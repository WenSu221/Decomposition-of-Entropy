#### contribution figure ####

### data ####

#change the data entry and then run the#
#code in "CAL calculation/entropy of CAL"#

### function ####

library(RColorBrewer)

coul <- brewer.pal(3,"Set1")

years <- seq(1952,2017,5)

longevity <- longevity*100
inequality <- inequality*100
decomp <- rbind(longevity,inequality)
rownames(decomp) <- c("logevity component", "lifespan inequality component")
colnames(decomp) <- c(seq(1952,2017,5))

windows(8,5)
barplot(decomp,col = coul,border = "white",xlab="Year",ylab = "percentage(%)",legend.text = TRUE,args.legend = list(bg="white"))
title("Decomposition of entropy of CAL between Sweden and England & Wales, male 1952-2017", cex=1)
