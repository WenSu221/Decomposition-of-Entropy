#### figure 2 Entropy Plot ####

#### Entropy of e0 ####

### e0 function # ----

LSV <- function(dt,Y){
    Ydt<-subset(dt, Year==Y)
    age_length_equal <- all.equal(length(Ydt$age),length(Ydt$dx),length(Ydt$lx),length(Ydt$ex),length(Ydt$ax))
    stopifnot(age_length_equal)
    n <- c(diff(Ydt$age),1)
    explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$age)])
    ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
    A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
    return(A[1])
    }


### e dagger function # ----

LE <- function(dt, Y1, Y2){
      
  for (i in Y1:Y2){
       e<-subset(A1,Year==i)
       ei<-e[1,10]
       e0<-c(e0,ei)
    }
  return(e0)
  }

e0<-c()

### data fitting # ----

A1 <- read.table("Data/ISL.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)

 ## e # ----

e0<-LE(A1,1841,2017)

## e dagger # ----
edagger <- c()

for (i in seq(1841,2017,1)){
  edagger <- c(edagger, LSV(A1,i))
  }
        
entropye0 <- edagger/e0
        