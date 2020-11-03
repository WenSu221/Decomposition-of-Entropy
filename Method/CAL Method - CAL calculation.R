rm(list=ls())

#### here change the countries and 

################
## CAL decomposition
################

####you should get the comparison of the two
Names<-c("SWE","FRA")


A1<-read.table("SWE.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2<-read.table("FRATNP.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1<-max(range(A1$Year)[1],range(A2$Year)[1])
Y2<-2013
Year1<-Y1
Year2<-Y2

A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]

qx1<-matrix(1-A1$qx,111)
qx2<-matrix(1-A2$qx,111)

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
  
 CALlx1<-c(CALlx1,lx1)
 CALlx2<-c(CALlx2,lx2)

 PxCh<-cbind(PxCh,pxCH)

}
CALlx<- t(matrix(rep((CALlx1+ CALlx2)/2,111),111))

PxCh[is.na(PxCh)]<-0

A1<-sum(c(1,CALlx1))+.5
A2<-sum(c(1,CALlx2))+.5
A3<-sum(CALlx2)-sum(CALlx1)
A4<-sum(PxCh* CALlx)

print(rbind(c(paste("CAL-",Name1),paste("CAL-",Name2),"Diff","est-Diff"),round(c(A1,A2,A3,A4),2)))
return(PxCh* CALlx)
}

CALlxDecomp<-CALDecompFunction(qx1,qx2,Y2,Names[1],Names[2])
