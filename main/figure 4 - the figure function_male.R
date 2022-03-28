###
### the figure function_male.R
###
library(ggplot2)

source("main/figure 4 - decomp male 1989-2017.R")

### differences figure ####
difference <- data.frame(
  c(rep("Sweden",90),rep("Denmark",90),rep("France",90),rep("England & Wales",90),rep("Norway",90),
    rep("Finland",90),rep("Italy",90),rep("Scotland",90),rep("Netherlands",90),rep("Switzerland",90),
    rep("United States",90)),
  
  c(rep("A.Low-inequality",90),rep("B.Cross-over",90),
    rep("D.Improving",90),
    rep("B.Low-inequality",90),rep("A.Low-inequality",90),
    rep("C.Improving",90),rep("D.Improving",90),
    rep("D.Widening",90),rep("A.Low-inequality",90),
    rep("A.Low-inequality",90),rep("C.Widening",90)),
  
  rep(rep(seq(1989,2018),3),11),
  
  rep(c(rep("2.Lifespan variation",30),rep("1.Longevity",30),rep("total",30)),11),
  
  c(dvariation_SWE,dlongevity_SWE,dtotal_SWE,
    dvariation_DNK,dlongevity_DNK,dtotal_DNK,
    dvariation_FRA,dlongevity_FRA,dtotal_FRA,
    dvariation_GBRTENW,dlongevity_GBRTENW,dtotal_GBRTENW,
    dvariation_NOR,dlongevity_NOR,dtotal_NOR,
    dvariation_FIN,dlongevity_FIN,dtotal_FIN,
    dvariation_ITA,dlongevity_ITA,dtotal_ITA,
    dvariation_GBRSCO,dlongevity_GBRSCO,dtotal_GBRSCO,
    dvariation_NLD,dlongevity_NLD,dtotal_NLD,
    dvariation_CHE,dlongevity_CHE,dtotal_CHE,
    dvariation_USA,dlongevity_USA,dtotal_USA
    
  ))
colnames(difference) <- c("population","country_type","year","type","relative_disparities")


ggplot(data =difference)+
  geom_col(data = subset(difference,type!="total"), mapping = aes(x=year,y=relative_disparities,fill=type),position = "stack")+
  geom_line(data = subset(difference,type=="total"),mapping = aes(x=year,y=relative_disparities),lwd=0.5)+
  geom_point(data = subset(difference,type=="total"),mapping = aes(x=year,y=relative_disparities),cex=0.6)+
  facet_wrap(~country_type+population)+
  scale_x_continuous(breaks = seq(1995,2015,10))+
  scale_fill_manual(values = c("blue","red"))+
  theme(plot.margin = margin(t=0.5,r=2,b=0.5,l=0.5,"cm"),
        legend.position = c(0.95,0.1),
        legend.background = element_blank(),
        plot.title = element_text(size = 10))+
  labs(x="Year",y="Relative Disparities",
       fill = "Contributions",
       title="Figure A2. Decomposition of the male CAL entropy gap 
               between the average and specific populations into
               longevity and lifespan variation. 1989-2017")
ggsave("Output/decomposition of differences, male 1989-2017.pdf",
       width = 6,height = 8,dpi = 300)

### changes figure ####
change <- data.frame(
  c(rep("Sweden",116),rep("Denmark",116),rep("France",116),rep("England & Wales",116),rep("Norway",116),
    rep("Finland",116),rep("Italy",116),rep("Scotland",116),rep("Netherlands",116),rep("Switzerland",116),
    rep("United States",116)),
  
  c(rep("A.Low-inequality",116),rep("B.Cross-over",116),
    rep("D.Improving",116),rep("B.Low-inequality",116),
    rep("A.Low-inequality",116),rep("C.Improving",116),
    rep("D.Improving",116),rep("D.Widening",116),
    rep("A.Low-inequality",116),rep("A.Low-inequality",116),
    rep("C.Widening",116)),
  
  rep(rep(seq(1990.5,2018.5),11),4),
  
  rep(c(rep("3.Lifespan variation",29),rep("2.Longevity",29),
        rep("1.Average entropy imrprovements",29),rep("total",29)),11),
  
  c(cvariation_SWE,clongevity_SWE,centropychange_SWE,ctotal_SWE,
    cvariation_DNK,clongevity_DNK,centropychange_DNK,ctotal_DNK,
    cvariation_FRA,clongevity_FRA,centropychange_FRA,ctotal_FRA,
    cvariation_GBRTENW,clongevity_GBRTENW,centropychange_GBRTENW,ctotal_GBRTENW,
    cvariation_NOR,clongevity_NOR,centropychange_NOR,ctotal_NOR,
    cvariation_FIN,clongevity_FIN,centropychange_FIN,ctotal_FIN,
    cvariation_ITA,clongevity_ITA,centropychange_ITA,ctotal_ITA,
    cvariation_GBRSCO,clongevity_GBRSCO,centropychange_GBRSCO,ctotal_GBRSCO,
    cvariation_NLD,clongevity_NLD,centropychange_NLD,ctotal_NLD,
    cvariation_CHE,clongevity_CHE,centropychange_CHE,ctotal_CHE,
    cvariation_USA,clongevity_USA,centropychange_USA,ctotal_USA
  ))
colnames(change) <- c("population","country_type","year","type","relative_disparities")

change$relative_disparities <- change$relative_disparities*100

ggplot(data =change)+
  geom_col(data = subset(change,type!="total"), mapping = aes(x=year,y=relative_disparities,fill=type),position = "stack")+
  geom_line(data = subset(change,type=="total"),mapping = aes(x=year,y=relative_disparities),lwd=0.5)+
  geom_point(data = subset(change,type=="total"),mapping = aes(x=year,y=relative_disparities),cex=0.6)+
  facet_wrap(~country_type+population)+
  scale_x_continuous(breaks = seq(1995,2015,10))+
  scale_fill_manual(values = c("green4","blue","red"))+
  theme(plot.margin = margin(t=0.5,r=3,b=0.5,l=0.5,"cm"),
        legend.position = c(1.02,0.1),
        legend.background = element_blank(),
        plot.title = element_text(size=10))+
  labs(x="Year",y="Relative Disparities",
       fill = "Contributions",
       title="Figure A3. Decomposition of the time changes in male CAL entropy gap 
               between the average and specific population across time 
               into average entropy improvements, longevity, and lifespan variation. 
               1990-2016.")
ggsave("Output/decomposition of changes in differences, male 1990-2018.pdf",
       width = 6,height = 8,dpi = 300)
