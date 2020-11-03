##### e dagger by Alysson van Raalte

A1<-read.table("FRATNP.fltper_1x1.txt",header=TRUE,fill=TRUE,skip=1)


ineq_edag <- function(age, dx, lx, ex, ax){
  age_length_equal <- all.equal(length(age),length(dx),
                                length(lx),length(ex),
                                length(ax))
  stopifnot(age_length_equal)
  n <- c(diff(age),1)
  explusone <- c(ex[-1],ex[length(age)])
  ex_average <- ex + ax / n * (explusone - ex[-length(age)])
  rev(cumsum(rev(dx * ex_average))) / lx 
}

e<-subset(A1,Year==1935)
evv<-ineq_edag(0:110,e$dx,e$lx,e$ex,e$ax)