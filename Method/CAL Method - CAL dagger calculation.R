#####CAL dagger

### Year setting----
A1<-read.table("DATA/SWE.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2<-read.table("DATA/FRATNP.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)
Names<-c("SWE","FRA")

Y1<-max(range(A1$Year)[1],range(A2$Year)[1])
Y2<-2013
Year1<-Y1
Year2<-Y2
A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]

### get the lx----
library(dplyr)
dx1<-A1%>%select("Year","Age","lx","dx")%>%group_by(Year)%>%mutate(cumsum=cumsum(dx))%>%mutate(qx=cumsum/sum(dx))
dx2<-A2%>%select("Year","Age","lx","dx")%>%group_by(Year)%>%mutate(cumsum=cumsum(dx))%>%mutate(qx=cumsum/sum(dx))
dx1$qx[dx1$dx==0]<-0
dx2$qx[dx2$dx==0]<-0
dx1<-matrix(dx1$qx,111)
dx2<-matrix(dx2$qx,111)
dlog1<-(-log(dx1))
dlog2<-(-log(dx2))
dlog1[dlog1==Inf]<-0
dlog2[dlog2==Inf]<-0
qx1<-dlog1*dx1
qx2<-dlog2*dx2

### Just get the discrete integral then----
CALdagger<-function(Mx1,Mx2,Y,Name1,Name2){
  
  CALlx1<-c()
  CALlx2<-c()
  
  YM<-Y-Y1
  
  for (x in 1:111){
    if (x <(YM+1)){
      px1<-c()
      px2<-c()
      for (z in 1:x){
        px1<-c(px1,sum(Mx1[z,YM-x+z])/x)
        px2<-c(px2,sum(Mx2[z,YM-x+z])/x)
      }
    }

    CALlx1<-c(CALlx1,px1)
    CALlx2<-c(CALlx2,px2)
    
  }
  
  A1<-sum((CALlx1))
  A2<-sum((CALlx2))
  A3<-sum(CALlx2)-sum(CALlx1)
  
  print(cbind(c(paste("CAL dagger-",Name1),paste("CAL dagger-",Name2),"Diff"),round(c(A1,A2,A3),4)))
}

###results----
CALd<-CALdagger(qx1,qx2,Y2,Names[1],Names[2])
