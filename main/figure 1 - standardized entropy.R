#### entropy as a standardized measure of inequality in mortality ####

library(dplyr)
library(ggplot2)
library(metR)
library(ggpubr)

### get mortality data from HMD ####
# by Pascariu
# library(MortalityLaws)
# data <- ReadHMD("LT_m",NULL,"1x1","u6897805@anu.edu.au","jYHy!m!6i5Ae6!8")
data <- read.csv("data.csv")

### LSV function ####
LSV <- function(Ydt){
  n <- c(diff(Ydt$Age),1)
  explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$Age)])
  ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
  A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
  return(A[1])
}

### fitting the data ####

# dta <- data[["data"]]
dta <- data[,-1]

table_total <- c()

for (x in 1957:2018){
  year <- as.numeric(x)
  test <- filter(dta,dta[,2]==year)
  
  names <- unique(as.character(test$country))
  
  ezero <- filter(test,Age==0)
  ezero <- as.numeric(ezero$ex)
  
  test <- group_by(test,country)
  test <- group_split(test)
  
  edagger <- as.numeric(sapply(test,FUN = LSV))
 
  entropy <- as.numeric(edagger/ezero)
  
  table <- cbind(names,ezero,edagger,entropy,year)

  table <- as.data.frame(table)
  
  table_total <- rbind(table_total,table)

}

row.names(table_total) <- c(1:length(table_total$edagger))
table_total$ezero <- as.numeric(as.character((table_total$ezero)))
table_total$edagger <- as.numeric(as.character((table_total$edagger)))
table_total$entropy <- as.numeric(as.character((table_total$entropy)))
table_total$year <- as.numeric(as.character((table_total$year)))

JPN <- table_total[table_total$names == "JPN",]
names(JPN) <- c("JPN","ezero","edagger","entropy","Years")

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

colnames(table_total) <- c("names","life expectancy","lifespan variation","entropy","Years")

g1 <- 
  ggplot(data = data.seq,aes(x = edagger,y = life.expectancy))+
  geom_raster(aes(fill = Entropy))+
  geom_contour(aes(z = Entropy),color = "white",alpha = 0.8)+
  geom_point(data = table_total,aes(x = `lifespan variation`,y = `life expectancy`,color = `Years`))+
  geom_text_contour(aes(z = Entropy),color = "black",size = 2.5, stroke = 0.2,stroke.color = "white")+
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(breaks = scales::breaks_width(10),expand = c(0,0))+
  labs(x = paste0("lifespan variation"), 
       y = paste0("life expectancy"), title = "Figure 1A. life table entropy surface, male 1957-2017")+
  scale_y_continuous(breaks = scales::breaks_width(5),expand = c(0,0))+
  scale_x_continuous(breaks = scales::breaks_width(1), expand = c(0,0))

g2 <- 
ggplot(data = data.seq,aes(x = edagger,y = life.expectancy))+
  geom_raster(aes(fill = Entropy))+
  geom_contour(aes(z = Entropy),color = "white",alpha = 0.8)+
  geom_point(data = JPN,aes(x = `edagger`,y = `ezero`,color = `Years`))+
  geom_text_contour(aes(z = Entropy),color = "black",size=2.5, stroke = 0.2,stroke.color = "white")+
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(breaks = scales::breaks_width(10),expand = c(0,0))+
  labs(x = "lifespan variation", y = "life expectancy", title = "Figure 1B. life table entropy surface, Japanese male 1957-2017")+
  scale_y_continuous(breaks = scales::breaks_width(5),expand = c(0,0))+
  scale_x_continuous(breaks = scales::breaks_width(1), expand = c(0,0))

ggarrange(g1,g2,nrow = 2)

ggsave("Output/Entropy Surface 1957-2018.pdf", 
       width = 8, height = 8, dpi = 300)  

match <- table_total %>% mutate(round = round(entropy,2)) %>% filter(round == 0.18)
