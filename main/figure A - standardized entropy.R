#### entropy as a standardized measure of inequality in mortality ####

library(dplyr)
library(ggplot2)
library(metR)

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
table_total <- c()
average_final <- c()

for (x in 1957:2017){
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
  
  table <- cbind(x,ezero,edagger,entropy,time)
  row.names(table) <- names
  table <- as.data.frame(table)
  table <- cbind(names,table)
  
  table_total <- rbind(table_total,table)
  
  table_test1 <- subset(table,table$edagger<average[,2])
  table_test1 <- subset(table_test1,table_test1$entropy>average[,3])
  
  table_final1 <- rbind(table_final1,table_test1)
  
  table_test2 <- subset(table,table$edagger>average[,2])
  table_test2 <- subset(table_test2,table_test2$entropy<average[,3])
  
  table_final2 <- rbind(table_final2,table_test2)
  
  average_final <- rbind(average_final,average)
}

row.names(table_total) <- c(1:length(table_total$edagger))
row.names(table_final1) <- c(1:length(table_final1$edagger))
row.names(table_final2) <- c(1:length(table_final2$edagger))

# duplicate.no <- duplicated(c(table_total$entropy,table_final1$entropy,table_final2$entropy))
# table_total <- table_total[!duplicate.no,]
# table_total <- na.omit(table_total)
### Plot ####

# myrange1 <- range(table_final1$entropy)
# myrange2 <- range(table_final1$edagger)
# myyears <- c(1950,2017)
# 
# plot(myrange2,myrange1,col = 0)
# points(table_final1$edagger,table_final1$entropy,col= "darkblue")
# lines(average_final[,2],average_final[,3])
# 
# myrange3 <- range(table_final2$entropy)
# myrange4 <- range(table_final2$edagger)
# 
# plot(myrange4,myrange3,col = 0)
# points(table_final2$edagger,table_final2$entropy,col= "darkblue")
# lines(average_final[,2],average_final[,3])
# 
# myrange5 <- range(table_total$entropy)
# myrange6 <- range(table_total$edagger)
# points(table_total$edagger,table_total$entropy,col= "darkgrey")
# lines(average_final[,2],average_final[,3], col = "red", lwd = 2)
# points(table_final2$edagger,table_final2$entropy,col= "darkgreen", pch = 7)
# points(table_final1$edagger,table_final1$entropy,col= "darkblue")

### ggplot2

# 
# table_total <- as.matrix(table_total)
# table_final1 <- as.matrix(table_final1)
# table_final2 <- as.matrix(table_final2)
# 
# plotdata <- data.frame(
#   c(table_total[,2],table_final1[,2],table_final2[,2]),
#   c(rep("salv entropy",length(table_total[,4])),
#     rep("hta entropy",183),rep("lta entrpy",135)),
#   c(table_total[,4],table_final1[,4],table_final2[,4]),
#   c(table_total[,5],table_final1[,5],table_final2[,5])
# )
# 
# colnames(plotdata) <- c("years","type","lifespan_inequality","entropy")
# 
# plotdata$type <- factor(plotdata$type)
# plotdata$years <- as.numeric(plotdata$years)

# ggplot(data = plotdata,aes(lifespan_inequality,entropy,alpha = years,color = type))+
#   geom_point()+
#   geom_line(average_final,mapping = aes(edagger_mean,entropy_mean),alpha = year,color= "black")+
#   scale_color_manual(values = c("red","blue","grey"))

### contour

le.range <- round(range(table_total$ezero),1)
lsv.range <- round(range(table_total$edagger),1)

le.seq <- seq(le.range[1],le.range[2],0.1)
lsv.seq <- seq(lsv.range[1],lsv.range[2],0.1)

entropy.seq <- c()

for (i in 1:length(lsv.seq)){
  hx <- lsv.seq[i]/le.seq
  entropy.seq <- c(entropy.seq,hx)
}

data.seq <- data.frame(
  rep(lsv.seq,each = length(le.seq)),
  rep(le.seq,times = length(lsv.seq)),
  entropy.seq
)

colnames(data.seq) <- c("edagger","life.expectancy","entropy")

ggplot(data = data.seq,aes(x = edagger,y = life.expectancy))+
  geom_raster(aes(fill = entropy))+
  geom_contour(aes(z = entropy),color = "black",alpha = 0.5)+
  geom_point(data = table_total,aes(x = edagger,y = ezero,alpha = time),color = "darkseagreen")+
  geom_text_contour(aes(z = entropy),color = "black")+
  scale_fill_continuous(low = "yellow", high = "red")+
  scale_alpha_continuous(range = rev(c(0.1,0.6)))+
  labs(x = "lifespan inequality", y = "life expectancy")+
  scale_y_continuous(expand = c(0,0))+
  scale_x_continuous(breaks = scales::breaks_width(1), expand = c(0,0))
  