#### contribution figure ####

### data ####

#change the data entry and then run the#
#code in "CAL calculation/entropy of CAL"#


#### short data ####

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1992,2017,5)

longevity <- longevity
inequality <- inequality
total <- longevity+inequality
decomp <- rbind(inequality,longevity,total)
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

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1957,2017,5)

longevity <- longevity
inequality <- inequality
total <- inequality+longevity
decomp <- rbind(inequality,longevity)
rownames(decomp) <- c("lifespan inequality component","longevity component")
colnames(decomp) <- c(seq(1957,2017,5))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- range(total)*1.2

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
        ylim = myrange,
        xlab="Year",ylab = "relative disparities",
        legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
        main = "Decomposition of entropy of CAL between England&Wales and Denmark,both 1957-2017",
        sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T,
        ylim = myrange,
        xlab="Year",ylab = "relative disparities",
        legend.text = TRUE,
        args.legend = list(x="topright",bg="white",box.col=0),
        main = "Decomposition of entropy of CAL between England&Wales and Denmark,both 1957-2017",
        sub = "source: Author's calculation based on HMD data")

lines(b,total,lwd=1.5)
points(b,total,pch=16)