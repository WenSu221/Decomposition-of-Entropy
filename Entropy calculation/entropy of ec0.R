#### figure 2 entropy of cohort life expectancy ####

#### CLE ####


### Data # ----
A1 <- read.table("Data/SWE.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A2 <- read.table("Data/DNK.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A3 <- read.table("Data/FRA.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A4 <- read.table("Data/GBR.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)
A5 <- read.table("Data/ISL.fltcoh_1x1.txt",header=TRUE,fill=TRUE,skip=1)

Y1 <- 1905
Y2 <- 1927

A5<-A5[(A5$Year>Y1)&(A5$Year<(Y2+1)),]
A4<-A4[(A4$Year>Y1)&(A4$Year<(Y2+1)),]
A3<-A3[(A3$Year>Y1)&(A3$Year<(Y2+1)),]
A2<-A2[(A2$Year>Y1)&(A2$Year<(Y2+1)),]
A1<-A1[(A1$Year>Y1)&(A1$Year<(Y2+1)),]

### e dagger # ----

LSV <- function(dt,Y){
  Ydt<-subset(dt, Year==Y)
  age_length_equal <- all.equal(length(Ydt$age),length(Ydt$dx),
                                length(Ydt$lx),length(Ydt$ex),
                                length(Ydt$ax))
  stopifnot(age_length_equal)
  n <- c(diff(Ydt$age),1)
  explusone <- c(Ydt$ex[-1],Ydt$ex[length(Ydt$age)])
  ex_average <- Ydt$ex + Ydt$ax / n * (explusone - Ydt$ex)
  A<-rev(cumsum(rev(Ydt$dx * ex_average))) / Ydt$lx
  return(A[1])
}


###e0 function # ----

LE <- function(dt, Y1, Y2){
  e0<-c()
  for (i in Y1:Y2){
    e<-subset(dt,Year==i)
    ei<-e[1,10]
    e0<-c(e0,ei)
  }
  return(e0)
}


### Data time! # ----
ec0SWE<-LE(A1,1906,1927)
ec0SWE <- as.numeric(ec0SWE)
ec0DNK<-LE(A2,1906,1927)
ec0DNK <- as.numeric(ec0DNK)
ec0FRA<-LE(A3,1906,1927)
ec0FRA <- as.numeric(ec0FRA)
ec0GBR<-LE(A4,1906,1927)
ec0GBR <- as.numeric(ec0GBR)
ec0ISL<-LE(A5,1906,1927)
ec0ISL <- as.numeric(ec0ISL)

# SWE
A1 <- as.data.frame(sapply(A1, as.numeric))
A1[is.na(A1)]<-0
edSWE <- c()
for (i in 1906:1927){
  edSWE <- c(edSWE, LSV(A1,i))
}

#DNK
A2 <- as.data.frame(sapply(A2, as.numeric))
A2[is.na(A2)]<-0
edDNK <- c()
for (i in 1906:1927){
  edDNK <- c(edDNK, LSV(A2,i))
}

#FRA
A3 <- as.data.frame(sapply(A3, as.numeric))
A3[is.na(A3)]<-0
edFRA <- c()
for (i in 1906:1927){
  edFRA <- c(edFRA, LSV(A3,i))
}

#GBR
A4 <- as.data.frame(sapply(A4, as.numeric))
A4[is.na(A4)]<-0
edGBR <- c()
for (i in 1906:1927){
  edGBR <- c(edGBR, LSV(A4,i))
}

#ISL
A5 <- as.data.frame(sapply(A5, as.numeric))
A5[is.na(A5)]<-0
edISL <- c()
for (i in 1906:1927){
  edISL <- c(edISL, LSV(A5,i))
}


### H # ----

entropycSWE <- edSWE/ec0SWE
entropycDNK <- edDNK/ec0DNK
entropycFRA <- edFRA/ec0FRA
entropycGBR <- edGBR/ec0GBR
entropycISL <- edISL/ec0ISL