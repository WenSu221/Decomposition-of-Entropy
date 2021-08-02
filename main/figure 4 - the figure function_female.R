###
### the figure function_female
###

source("main/figure 4 - decomp female 1989-2017.R")

### differences figure ####
difference <- data.frame(
  
  c(rep("Denmark",45),rep("England & Wales",45),
    rep("Finland",45),rep("Netherlands",45),
    rep("Norway",45), rep("Sweden",45),
    rep("Switzerland",45),rep("France",45),
    rep("Italy",45),rep("Scotland",45),rep("United States",45)),
  
  c(rep("B.Cross-over",45),rep("B.Cross-over",45),rep("B.Cross-over",45),
    rep("A.Low-inequality",45),rep("A.Low-inequality",45),
    rep("A.Low-inequality",45),rep("A.Low-inequality",45),
    rep("D.Improving",45),rep("D.Improving",45),
    rep("D.Improving",45),rep("C.Widening",45)),
  
  rep(rep(seq(1989,2017,2),3),11),
  
  rep(c(rep("2.Lifespan variation",15),rep("1.Longevity",15),rep("total",15)),11),
  
  c(dvariation_DNK,dlongevity_DNK,dtotal_DNK,
    dvariation_GBRTENW,dlongevity_GBRTENW,dtotal_GBRTENW,
    dvariation_FIN,dlongevity_FIN,dtotal_FIN,
    dvariation_NLD,dlongevity_NLD,dtotal_NLD,
    dvariation_NOR,dlongevity_NOR,dtotal_NOR,
    dvariation_SWE,dlongevity_SWE,dtotal_SWE,
    dvariation_CHE,dlongevity_CHE,dtotal_CHE,
    dvariation_FRA,dlongevity_FRA,dtotal_FRA,
    dvariation_ITA,dlongevity_ITA,dtotal_ITA,
    dvariation_GBRSCO,dlongevity_GBRSCO,dtotal_GBRSCO,
    dvariation_USA,dlongevity_USA,dtotal_USA))

colnames(difference) <- c("population","country_type","year","type","relative_disparities")

ggplot(data =difference)+
  geom_col(data = subset(difference,type!="total"), mapping = aes(x=year,y=relative_disparities,fill=type),position = "stack")+
  geom_line(data = subset(difference,type=="total"),mapping = aes(x=year,y=relative_disparities),lwd=0.5)+
  geom_point(data = subset(difference,type=="total"),mapping = aes(x=year,y=relative_disparities),cex=0.6)+
  scale_fill_manual(values = c("blue","red"))+
  facet_wrap(~country_type+population)+
  theme(plot.title = element_text(size = 10),
        plot.margin = margin(t=0.5,r=2,b=0.5,l=0.5,"cm"),
        legend.position = c(0.95,0.1),
        legend.background = element_blank())+
  labs(x="Year",y="Relative Disparities",
       fill="Contributions",
       title="Figure 3. Decomposition of the female CAL entropy gap 
               between the average and specific populations into
               longevity and lifespan variation. 1989-2017")
ggsave("Output/decomposition of differences, female 1989-2017.pdf",width = 6,height = 8,dpi = 300)

### changes figure ####
change <- data.frame(
  c(rep("Sweden",56),rep("Denmark",56),
    rep("France",56),rep("England & Wales",56),
    rep("Norway",56),rep("Finland",56),
    rep("Italy",56),rep("Scotland",56),
    rep("Netherlands",56),rep("Switzerland",56),
    rep("United States",56)),
  
  c(rep("A.Low-inequality",56),rep("B.Cross-over",56),rep("D.Improving",56),
    rep("B.Cross-over",56),rep("A.Low-inequality",56),
    rep("B.Cross-over",56),rep("D.Improving",56),
    rep("D.Improving",56),rep("A.Low-inequality",56),
    rep("A.Low-inequality",56),rep("C.Widening",56)),
  
  rep(rep(seq(1990,2017,2),11),4),
  
  rep(c(rep("3.Lifespan variation",14),rep("2.Longevity",14),
        rep("1.Average entropy improvements",14),rep("total",14)),11),
  
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
  geom_point(data = subset(change,type=="total"),mapping = aes(x=year,y=relative_disparities),cex = 0.6)+
  facet_wrap(~country_type+population)+
  scale_fill_manual(values = rev(c("red","blue","green4")))+
  theme(plot.margin = margin(t=0.5,r=3,b=0.5,l=0.5,"cm"),
        legend.position = c(1.02,0.1),
        legend.background = element_blank(),
        plot.title = element_text(size =10)
        )+
  labs(x="Year",y="Contributions to change",
       fill = "Contributions",
       title="Figure 4. Decomposition of the time changes in female CAL entropy gap 
               between the average and specific population across time 
               into average entropy improvements, longevity, and lifespan variation. 
               1990-2016.")
ggsave("Output/decomposition of changes in differences, female 1990-2016.pdf",width = 6,height = 8,dpi = 300)
