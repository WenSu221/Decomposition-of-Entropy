
###
### Plot Functions
###

library(data.table)
library(ggplot2)
library(latex2exp)
library(metR)

table_total <- fread("entropy surface_comp.csv")
table_total$measure <- factor(table_total$measure,
                              levels = c(
                                "period",
                                "cohort",
                                "CAL"
                              ))
table_total <- table_total[measure=="CAL",]

le.range <- round(range(table_total$longevity),1)
lsv.range <- round(range(table_total$`lifespan variation`),1)

le.seq <- seq(floor(le.range[1]),ceiling(le.range[2]),1)
lsv.seq <- seq(floor(lsv.range[1]),ceiling(lsv.range[2]),1)

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

colnames(data.seq) <- c("lifespan.variation","life.expectancy","Entropy")


ggplot(data = data.seq,
       aes(x = lifespan.variation,
           y = life.expectancy))+
  geom_raster(aes(fill = Entropy))+
  geom_contour(aes(z = Entropy),
               color = "white",alpha = 0.8)+
  geom_point(data = table_total,
             aes(x = `lifespan variation`,
                 y = `longevity`,color=years))+
  geom_text_contour(aes(z = Entropy),
                    color = "black",size = 2.5,
                    stroke = 0.2,
                    stroke.color = "white")+
  facet_wrap(~country)+
  scale_fill_viridis_c(option = "plasma")+
  scale_color_viridis_c(breaks = scales::breaks_width(10),
                        expand = c(0,0))+
  guides(color = guide_legend(title = "Years"))+
  labs(x = TeX("$CAL^\\dagger$"), 
       y = TeX("CAL"), 
       title = "Figure . CAL entropy surface, female 1957-2018")+
  scale_y_continuous(breaks = scales::breaks_width(5),
                     expand = c(0,0))+
  scale_x_continuous(breaks = scales::breaks_width(1),
                     expand = c(0,0))+
  theme_classic()

