#### contribution figure ####

### data ####

#change the data entry and then run the#
#code in "CAL calculation/entropy of CAL"#


#### short data ####

library(RColorBrewer)
coul <- brewer.pal(5,"Set1")

years <- seq(1992,2017,3)

longevity <- longevity
inequality <- inequality
total <- longevity+inequality
decomp <- rbind(inequality,longevity)
rownames(decomp) <- c("lifespan inequality component","longevity component")
colnames(decomp) <- c(seq(1992,2017,3))

decomp1 <- decomp2 <- decomp
decomp1[decomp1>0] <- 0
decomp2[decomp2<0] <- 0
myrange <- range(total)*1.2

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
             main = "Decomposition of entropy of CAL between Denmark and average level,both 1992-2017",
             sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T,
             ylim = myrange,
             xlab="Year",ylab = "relative disparities",
             legend.text = TRUE,
             args.legend = list(x="topright",bg="white",box.col=0))
             
lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("average level", side = 2,adj = 0.5)



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
myrange <- c(-0.001,0.0253)

windows(11,9)
b <- barplot(decomp1,col = coul,border = "white",
        ylim = myrange,
        xlab="Year",ylab = "relative disparities",
        legend.text = TRUE,args.legend = list(x="topright",bg="white",box.col=0),
        main = "Decomposition of entropy of CAL between Sweden and Denmark,both 1957-2017",
        sub = "source: Author's calculation based on HMD data")

b <- barplot(decomp2,col = coul,border = "white",add = T,
        ylim = myrange,
        xlab="Year",ylab = "relative disparities",
        legend.text = TRUE,
        args.legend = list(x="topright",bg="white",box.col=0))
lines(b,total,lwd=1.5)
points(b,total,pch=16)
mtext("Sweden",side = 2,adj = 0)
