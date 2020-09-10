load("/Volumes/qac/qac201/Studies and Codebooks/GSS/DATA/GSS_all.RData")

library(descr)

GSSsub <- gss[gss$YEAR >= 2012,]

freq(GSSsub$HAPPY)
freq(GSSsub$MAJOR1)
freq(GSSsub$MAJOR2)
