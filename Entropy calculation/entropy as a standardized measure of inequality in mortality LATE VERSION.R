#### entropy as a standardized measure of inequality in mortality ####

library(dplyr)

### get mortality data from HMD ####
# by M. Pascariu
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
  table <- as.data.frame(table)
  table <- cbind(names,table)
    
  table_test <- subset(table,table$edagger<average[,2])
  table_test <- subset(table_test,table_test$entropy>average[,3])
    
  table_final <- rbind(table_final,table_test)
}

output1 <- table(table_final$names)

countries <- c("AUT","BEL","CZE","DEUTE","DEUTW","EST","FIN","FRATNP",
               "HUN","LTU","LVA","NZL_MA","PRT","RUS","SVK","SVN")


for (i in countries){
  countrycode <- i
  countryyears <- subset(table_final,names == countrycode)
  countryyears <- countryyears$time
  print(c(countrycode,countryyears))
}
