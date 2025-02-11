###
### sensitiviyt check
###

source("US data/USCAL female.R")
SWE <- read.table("Data/SWE.fltper_1x1.txt", header = T,skip = 2)

qx.1932.SWE <- subset(SWE, Year == 1932)
qx.1932.SWE <- qx.1932.SWE$qx[1:32]
qx.1932.US <- USAmale[1:32,32]

range(qx.1932.SWE - qx.1932.US)

qx.1925.SWE <- subset(SWE, Year == 1925)
qx.1925.SWE <- qx.1925.SWE$qx[1:25]
qx.1925.US <- USAmale[1:25,25]

range(qx.1925.SWE - qx.1925.US)

qx.1918.SWE <- subset(SWE, Year == 1918)
qx.1918.SWE <- qx.1918.SWE$qx[1:18]
qx.1918.US <- USAmale[1:18,18]

range(qx.1918.SWE - qx.1918.US)

qx.1910.SWE <- subset(SWE, Year == 1910)
qx.1910.SWE <- qx.1910.SWE$qx[1:10]
qx.1910.US <- USAmale[1:10,10]

range(qx.1910.SWE - qx.1910.US)
