#### Life Tables
# Life Table function starting from mx 

LifeT_mx<-function(mx,AX){
  
  N<-length(mx)
  ax<-AX
  
  qx<- mx  / ( 1 + ((1-ax) * mx))
  
  qx[N]<-1
  
  #qx<-round(qx,5)
  
  px<-1-qx
  
  lx<-c(100000)
  for(y in 1:110){
    lx[y+1]<-lx[y]*px[y]
  }
  
  dx<-lx*qx
  
  Lx<- lx - (dx*ax)
  
  Lx[N]<- lx[N]/mx[N]
  
  Tx<-c()
  for (x in 1:N){
    Tx[x]<-sum(Lx[x:N])}
  
  ex<-Tx/lx
  LTnew<-cbind(mx,ax,qx,dx,lx,Lx,Tx,ex)
  return(LTnew)
}
