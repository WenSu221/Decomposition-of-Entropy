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

table_final <- c()
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
  
  table_test <- subset(table,table$edagger<average[,2])
  table_test <- subset(table_test,table_test$entropy>average[,3])
  
  table_final <- rbind(table_final,table_test)
  average_final <- rbind(average_final,average)
}

myrange1 <- range(average_final[,3])
myrange2 <- range(table_final$edagger)
myyears1 <- c(1950,2017)
myyears1_con <- c(1950:2017)

windows(10,8)
par(mfrow = c(1,2))

plot(myyears1,myrange1,col=0,xlab = "year",ylab = "entropy")
legend(1948,0.15, c("average level of entropy",
                    "countries with lower lifespan variation"),
       col=c("black","red"),pch=16,box.lty=0)
points(myyears1_con,average_final[,3])
points(table_final$time,table_final$entropy,col="red")
points(1960,0.2384799,col = "blue",pch = 19)


plot(myyears1,myrange2,col=0,xlab = "year",ylab = "lifespan variation")
legend(1948,11,c("average level of entropy", 
                 "country with higer entropy"),
       col = c("black","red"),pch = 16,box.lty = 0)
points(myyears1_con,average_final[,2])
points(table_final$time,table_final$edagger,col = "red")
points(1960,13.92245,col = "blue",pch = 19)

