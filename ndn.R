library(outliers)
library(ggplot2)

library(rcompanion)

library(FSA)

ndnD = read.table("NDN-R-DataSet.txt", header=TRUE)
names(ndnD)
pairs(ndnD)

ndnResFactor<-lm(ISR~factor(NI)+factor(NN)+factor(RB)+factor(RD),data=ndnD)
summary(ndnResFactor)
plot(ndnResFactor)
