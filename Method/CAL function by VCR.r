
#### here put the folder where you have your HMD data
CountryA<-"C:/Users/seven/Documents/R projects/CAL decomp"

####  here put the folder where you want to include your results
FOLDER<-"C:/Users/seven/Documents/R projects/CAL decomp"


#### here change the countries and 
####you should get the comparison of the two
Names<-c("AUS","USA")


################
## CAL decomposition
################

library(RColorBrewer)

mypalette<-rev(brewer.pal(8,"YlGnBu"))
mypalette2<-rev(brewer.pal(8,"YlOrRd"))

WildColors<-c(mypalette[1:4],"white","white",mypalette2[c(6,4,2,1)])

# the different levels of the Z values
levels<-c(-1,-0.1,-0.01,-0.001,-0.0001,0,.0001,.001,.01,.1,1)

options(scipen=10)

customAxis <- function() { 
n <- length(levels) 
y <- seq(min(levels), max(levels), length.out=n) 
rect(0, y[1:(n-1)], 1, y[2:n], col=WildColors) 
axis(4, at=y, labels=levels) 
} 



CALDecompFunction<-function(Mx1,Mx2,Y,Name1,Name2){
CALlx<-c()
CALlx1<-c()
CALlx2<-c()
PxCh<-c()

YM<-Y-Y1

for (x in 1:111){
  if (x <(YM+1)){
    px1<-c()
    px2<-c()
	for (z in 1:x){
		px1<-c(px1,Mx1[z,YM-x+z])
		px2<-c(px2,Mx2[z,YM-x+z])
	}
    pxCH<-c(log(px2/px1),rep(0,111-x)) 
 	
    lx1<-prod(px1)
    lx2<-prod(px2)
 

  }
  if (x >(YM)){
    px1<-c()
    px2<-c()
	for (z in (x-YM+1):x){
		px1<-c(px1,Mx1[z,YM-x+z])
		px2<-c(px2,Mx2[z,YM-x+z])
	}

	px1<-c(rep(1,(x-YM)),px1)
	px2<-c(rep(1,(x-YM)),px2)
    pxCH<-c(log(px2/px1),rep(0,111-x))	 	

    lx1<-prod(px1)
    lx2<-prod(px2)

  }
 CALlx1<-c(CALlx1,lx1)
 CALlx2<-c(CALlx2,lx2)

 PxCh<-cbind(PxCh,pxCH)

}
CALlx<- t(matrix(rep((CALlx1+ CALlx2)/2,111),111))

PxCh[is.na(PxCh)]<-0

## as Guillot calculates this plus a one for l(0)
A1<-sum(c(1,CALlx1))+.5
A2<-sum(c(1,CALlx2))+.5
A3<-sum(CALlx2)-sum(CALlx1)
A4<-sum(PxCh* CALlx)


print(rbind(c(paste("CAL-",Name1),paste("CAL-",Name2),"Diff","est-Diff"),round(c(A1,A2,A3,A4),2)))
return(PxCh* CALlx)
}




Country<-paste(CountryA,Names[1],"/STATS",sep="")
setwd(Country)
A1<-read.table("USA.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
Country<-paste(CountryA,Names[2],"/STATS",sep="")
setwd(Country)
A2<-read.table("AUS.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
setwd(FOLDER)


Y1<-max(range(A1$Year)[1],range(A2$Year)[1])
Y2<-2010
Year1<-Y1
Year2<-Y2




A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]


qx1<-matrix(1-A1$qx,111)
qx2<-matrix(1-A2$qx,111)


CALlxDecomp<-CALDecompFunction(qx1,qx2,Y2,Names[1],Names[2])



# The correct assignment of contributions and the cummulative changes
CALlxD<-matrix(0,111,111)
CALlxDS<-CALlxD
	
Age<-c(0:110)

CALlxD<-CALlxD
CALlxDS<-CALlxDS

YEARS<-c((Year2-110):Year2)

Age<-c(0:110)


# The correct assignment of contributions and the cummulative changes
CALlxD<-matrix(0,111,111)
CALlxDS<-CALlxD
	
for (y in 1:111){
		for (x in 1:y){
			CALlxD[x,(111-y+x)]<-CALlxDecomp[x,y]			
			CALlxDS[x,(111-y+x)]<-sum(CALlxDecomp[(1:x),y])
		}
	}




filled.contour(YEARS,Age,t(CALlxDS),levels=levels,
               col=WildColors,key.axes=customAxis(),ylab="Age-contribution",xlab="Year")

mtext("Source: HMD",1,3,adj=-.1,cex=1)

Nm<-paste("Fig.png",sep="")
dev.copy2eps(file=Nm)      

