#### entropy as a standardized measure of inequality in mortality ####

library(dplyr)

### get mortality data from HMD ####
# by Pascariu
library(MortalityLaws)
data <- ReadHMD("LT_m",NULL,"1x1","u6897805@anu.edu.au","jYHy!m!6i5Ae6!8")


### LSV function ####
LSV <- function(Ydt){
  n <- c(diff(Ydt$Age),1)
  explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$Age)])
  ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
  A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
  return(A[1])
}

### fitting the data ####

dta <- data[["data"]]

table_final1 <- c()
table_final2 <- c()
average_final <- c()

for (x in 1950:2017){
  year <- x
  test <- filter(dta,dta[,2]==year)
  
  names <- c(test$country)
  names <- names[seq(1,length(names),111)]
  time <- rep(year,length(names))
  
  ezero <- filter(test,Age==0)
  ezero <- ezero$ex
  ezero_mean <- mean(ezero)
  
  test <- group_by(test,country)
  test <- group_split(test)
  
  edagger <- sapply(test,FUN = LSV)
  edagger_mean <- mean(edagger)
  entropy <- edagger/ezero
  entropy_mean <- edagger_mean/ezero_mean
  average <- cbind(ezero_mean,edagger_mean,entropy_mean,year)
  
  table <- cbind(ezero,edagger,entropy,time)
  row.names(table) <- names
  table <- as.data.frame(table)
  table <- cbind(names,table)
  
  table_test1 <- subset(table,table$edagger<average[,2])
  table_test1 <- subset(table_test1,table_test1$entropy>average[,3])
  
  table_final1 <- rbind(table_final1,table_test1)
  
  table_test2 <- subset(table,table$edagger>average[,2])
  table_test2 <- subset(table_test2,table_test2$entropy<average[,3])
  
  table_final2 <- rbind(table_final2,table_test2)
  
  average_final <- rbind(average_final,average)
}

### The Outliers ####

outliers1 <- subset(table_final1, table_final1$entropy > 0.215)
outliers1 <- subset(outliers, time > 1960)

outliers2 <- subset(table_final1, table_final1$edagger < 10.4)
outliers2 <- subset(outliers2, time > 2000)

### Plot ####

myrange1 <- range(average_final[,3])
myrange2 <- range(c(range(table_final1$edagger),
                    range(table_final2$edagger)))
myyears <- c(1950,2017)
myyears_con <- c(1950:2017)

windows(10,8)
par(mfrow = c(1,2))

plot(myyears,myrange1,col=0,xlab = "year",ylab = "entropy")
legend(1948,0.158, c("average level of entropy",
                    "countries with pseudo-lower inequality in mortality ",
                    "countries with pseudo-higher inequality in mortality",
                    "outliers in countries with pseudo-lower inequality in mortality",
                    "outliers in countries with pseudo-higher inequality in mortality"),
       col=c("black","red","blue","purple","green"),pch=16,box.lty=0)
lines(myyears_con,average_final[,3],lwd = 2)
points(table_final1$time,table_final1$entropy,col="red")
points(table_final2$time,table_final2$entropy,col="blue")
points(outliers1$time,outliers1$entropy, pch = 15, cex = 1.5,
       col = "purple")
points(outliers2$time,outliers2$entropy, pch = 17, cex = 1.5,
       col = "green")

plot(myyears,myrange2,col=0,xlab = "year",ylab = "lifespan variation")
legend(1948,10.8,c("average level of entropy",
                 "countries with pseudo-lower inequality in mortality",
                 "countries with pseudo-higher inequality in mortality",
                 "outliers in countries with pseudo-lower inequality in mortality",
                 "outliers in countries with pseudo-higher inequality in mortality"),
       col = c("black","red","blue","purple","green"),pch = 16,box.lty = 0)
lines(myyears_con,average_final[,2],lwd = 2)
points(table_final1$time,table_final1$edagger,col = "red")
points(table_final2$time,table_final2$edagger,col = "blue")
points(outliers1$time,outliers1$edagger, pch = 15, cex = 1.5,
       col = "purple")
points(outliers2$time,outliers2$edagger, pch = 17, cex = 1.5,
       col = "green")