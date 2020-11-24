#### Overlapping ####

windows(8,5)
plot(c(0,222),c(0,111),col=0,xlab="years",ylab="age")
polygon(c(0,111,111),c(0,111,0))
polygon(c(50,161,161),c(0,111,0))
polygon(c(50,111,111),c(0,0,61),col=2)
title("Overlapping Issue in Comparing CAL in same country over time")
