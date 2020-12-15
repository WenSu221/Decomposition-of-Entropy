#### entropy as a standardized measure of inequality in mortality####

library(dplyr)
library(MortalityLaws)

### prepare the data ####
data <- ReadHMD("LT_m",NULL,"1x1","u6897805@anu.edu.au","jYHy!m!6i5Ae6!8")



#### entropy comparison ####

test <- data[["data"]]

test <- subset(test,Year==2017)

names <- c(test$country)
names <- names[seq(1,length(names),111)]

ezero <- subset(test,Age==0)
ezero <- ezero$ex

test <- group_by(test,country)
test <- group_split(test)

### edagger function
## as A. van Raalte developed

LSV <- function(Ydt){
  n <- c(diff(Ydt$Age),1)
  explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$Age)])
  ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
  A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
  return(A[1])
}

### analysis

edagger <- sapply(test,FUN = LSV)

entropy <- edagger/ezero

table <- cbind(ezero,edagger,entropy)
row.names(table) <- names
table <- as.data.frame(table)

# hypo_level <- 11/88
JPNindex <- table["JPN",]

table_test <- subset(table,table$edagger<JPNindex$edagger)
table_test <- subset(table_test,table_test$entropy>JPNindex$entropy)
table_test <- rbind(JPNindex, table_test)


### Survival Curve ####
names_selected <- c("CZE","EST","LTU","LVA")
test2 <- data[["data"]]

test2 <- subset(test2,Year==2017)
JPN <- subset(test2,country=="JPN")
JPNpx <- 1-(JPN$qx)
JPNpx <- c(1,JPNpx)
CZE <- subset(test2,country=="CZE")
CZEpx <- 1-(CZE$qx)
CZEpx <- c(1,CZEpx)
EST <- subset(test2,country=="EST")
ESTpx <- 1-(EST$qx)
ESTpx <- c(1,ESTpx)
LTU <- subset(test2,country=="LTU")
LTUpx <- 1-(LTU$qx)
LTUpx <- c(1,LTUpx)
LVA <- subset(test2,country=="LVA")
LVApx <- 1-(LVA$qx)
LVApx <- c(1,LVApx)

#### Survival Curve ####
agerange_plot <- c(0,109)
lxrange <- c(0,1)
plot(agerange_plot,lxrange,col=0,
     xlim = agerange_plot,ylim = lxrange,
     main = "JPN survival curve compared to CZE,EST,LTU,LVA",
     xlab = "Age",ylab = "lx")
agerange_lines <- c(1:112)
lines(agerange_lines,JPNpx,col="red",lwd = 2)
lines(agerange_lines,CZEpx)
lines(agerange_lines,ESTpx)
lines(agerange_lines,LTUpx)
lines(agerange_lines,LVApx)