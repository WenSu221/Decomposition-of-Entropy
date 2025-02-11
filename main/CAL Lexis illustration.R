
###
### TCAL illustrations
###

library(latex2exp)

years <- c(1907:2018)
age <- c(0:110)

png('Figure 1.png',width = 8,height = 8,units = "in",res=300)

plot(x = c(1905,2025), y=c(0,110),col = 0,
     xlab = 'Year', ylab = 'Age',
     axes = F)

axis(1,at=seq(1905,2025,5),labels = seq(1905,2025,5))
axis(2,at=seq(0,110,5),labels = seq(0,110,5))

polygon(x = c(1907,2018,2018), y=c(0,110,0),
        border = 'grey50')

polygon(x = c(1907,1907,2018,2018),y=c(0,0,110,0),
        border = 'grey50',col = 'pink')

# segments(x0=1955,x1=1955,y0=0,y1=50,lty=2,col='grey50')

segments(x0=1907,x1=2018,y0=0,y1=110,
         col='navy',lty=2,lwd=4)
# text(1940,55,expression(symbol("\110")['0,c'](1907)),
#      col='orange',cex=1.2)
text(1940,45,expression("Cohort Entropy (1907)"),
     col='navy',cex=0.9,srt = 45)

segments(x0=2018,x1=2018,y0=0,y1=110,
         col='forestgreen',lty=2,lwd=4)
text(2023,50,expression("Period Entropy (2018)"),
     col='forestgreen',cex=0.9,srt=90)

segments(x0=2008,x1=2018,y0=0,y1=10,
         col='black',lty=2,lwd=2)
text(2008,10,expression(l [c](10,2008)),
     col = "black",cex=0.9,
     srt=45)

segments(x0=1988,x1=2018,y0=0,y1=30,col='black',
         lty=2,lwd=2)
text(1998,20,expression(l[c](30,1988)),
     col = "black",cex=0.9,srt=45)

segments(x0=1968,x1=2018,y0=0,y1=50,col='black',
         lty=2,lwd=2)
text(1988,30,expression(l[c](50,1968)),
     col = "black",cex=0.9,srt=45)

segments(x0=1948,x1=2018,y0=0,y1=70,col='black',
         lty=2,lwd=2)
text(1978,40,expression(l[c](70,1948)),
     col = "black",cex=0.9,srt=45)

segments(x0=1928,x1=2018,y0=0,y1=90,col='black',
         lty=2,lwd=2)
text(1968,50,expression(l[c](90,1928)),
     col = "black",cex=0.9,srt=45)


title(main = paste('Figure 1. Illustration of CAL Entropy'),
      cex=0.5)



dev.off()
