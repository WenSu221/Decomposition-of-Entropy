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

JPN <- table_total[table_total$names == "JPN",]
colnames(JPN) <- c("names","Years","ezero","edagger","entropy","Japan")

### contour

le.range <- round(range(table_total$ezero),1)
lsv.range <- round(range(table_total$edagger),1)

le.seq <- seq(le.range[1]-1,le.range[2]+1,0.1)
lsv.seq <- seq(lsv.range[1]-1,lsv.range[2]+1,0.1)

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

colnames(data.seq) <- c("edagger","life.expectancy","Entropy")

colnames(table_total) <- c("names","Years","life expectancy","lifespan variation","entropy","year")

ggplot(data = data.seq,aes(x = edagger,y = life.expectancy))+
  geom_raster(aes(fill = Entropy))+
  geom_contour(aes(z = Entropy),color = "white",alpha = 0.8)+
  geom_point(data = table_total,aes(x = `lifespan variation`,y = `life expectancy`,color = `Years`),size = 0.8)+
  geom_point(data = JPN,aes(x = `edagger`,y = `ezero`,size = `Japan`),color = "red")+
  geom_text_contour(aes(z = Entropy),color = "black", stroke = 0.2,stroke.color = "white")+
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c()+
  scale_size_continuous(range = rev(c(1,4)))+
  labs(x = "lifespan inequality", y = "life expectancy", title = "Figure 0. life table entropy surface, male 1957-2017")+
  scale_y_continuous(breaks = scales::breaks_width(5),expand = c(0,0))+
  scale_x_continuous(breaks = scales::breaks_width(1), expand = c(0,0))

ggsave("Output/gradient map for male period entropy 1957-2017.pdf", 
       width = 12, height = 6, dpi = 300)  
