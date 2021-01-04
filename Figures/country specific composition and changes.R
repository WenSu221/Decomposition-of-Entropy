#### Figure3 country specific ####

### load the entropy data #####

source("Figures/average of CAL 1989-2017.R")


### Plotting country specific ####

names <- c("Sweden","Denmark","France Total","England & Wales",
           "Norway","Finland","Italy","Scotland","Netherlands")
entropynames <- list(entropyCAL1,entropyCAL2,entropyCAL3,entropyCAL4,
                  entropyCAL5,entropyCAL6,entropyCAL7,entropyCAL8,
                  entropyCAL9)
CALnames <- list(CAL1,CAL2,CAL3,CAL4,CAL5,CAL6,CAL7,CAL8,CAL9)
CALdaggernames <- list(CALdagger1,CALdagger2,CALdagger3,CALdagger4,CALdagger5,
                    CALdagger6,CALdagger7,CALdagger8,CALdagger9)

windows(12,9,rescale = "fit")
par(mfrow=c(3,3))

for(i in 1:9){
  entropyCAL <- unlist(entropynames[i])
  CAL <- unlist(CALnames[i])
  CALdagger <- unlist(CALdaggernames[i])
  
  entropyavg <- (entropyCAL+CALentropyavg)/2
  entropydiff <- entropyCAL-CALentropyavg
  
  dispersion <- log(CALdagger/CALdaggeravg)
  measure <- log(CAL/CALavg)
  
  variation <- dispersion*entropyavg
  longevity <- (measure*entropyavg)*-1
  
  library(RColorBrewer)
  coul <- brewer.pal(5,"Set1")
  years <- seq(1989,2017,4)
  
  total <- variation+longevity
  decomp <- rbind(variation,longevity)
  rownames(decomp) <- c("lifespan inequality component","longevity component")
  colnames(decomp) <- c(seq(1989,2017,4))
  
  decomp1 <- decomp2 <- decomp
  decomp1[decomp1>0] <- 0
  decomp2[decomp2<0] <- 0
  myrange <- c(-0.025,0.035)
  
  b <- barplot(decomp1,col = coul,border = "white",
               ylim = myrange,
               xlab="Year",ylab = "relative disparities",
               main = names[i])
  
  b <- barplot(decomp2,col = coul,border = "white",add = T)
  
  lines(b,total,lwd=1.5)
  points(b,total,pch=16)
  mtext("average level", side = 2,adj = 0.4)
}
