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

########################## Interaction effect start #############################

# interaction for number of nodes with number of intererst
interaction.plot(x.factor     = ndnD$NN,
                 trace.factor = ndnD$NI, 
                 response     = ndnD$ISR, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green","blue"), 
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 trace.label = "NI",
                 xlab =  "NN",
                 ylab = "mean of ISR",
                 leg.bty = "o")
# interaction for number of nodes with RB
interaction.plot(x.factor     = ndnD$NN,
                 trace.factor = ndnD$RB, 
                 response     = ndnD$ISR, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green","blue"),  
                 pch=c(19, 17, 15),           
                 fixed=TRUE,                  
                 trace.label = "RB",
                 xlab =  "NN",
                 ylab = "mean of ISR",
                 leg.bty = "o")
# interaction for number of nodes with RD
interaction.plot(x.factor     = ndnD$NN,
                 trace.factor = ndnD$RD, 
                 response     = ndnD$ISR, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green","blue"), 
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 trace.label = "RD",
                 xlab =  "NN",
                 ylab = "mean of ISR",
                 leg.bty = "o")
# interaction for NI with RB
interaction.plot(x.factor     = ndnD$NI,
                 trace.factor = ndnD$RB, 
                 response     = ndnD$ISR, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green","blue"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 trace.label = "RD",
                 xlab =  "NN",
                 ylab = "mean of ISR",
                 leg.bty = "o")
# interaction for NI with RD
interaction.plot(x.factor     = ndnD$NI,
                 trace.factor = ndnD$RD, 
                 response     = ndnD$ISR, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green","blue"),  
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 trace.label = "RD",
                 xlab =  "NN",
                 ylab = "mean of ISR",
                 leg.bty = "o")
# interaction for RB with RD
interaction.plot(x.factor     = ndnD$RB,
                 trace.factor = ndnD$RD, 
                 response     = ndnD$ISR, 
                 fun = mean,
                 type="b",
                 col=c("black","red","green","blue"), 
                 pch=c(19, 17, 15),             
                 fixed=TRUE,                    
                 trace.label = "RD",
                 xlab =  "NN",
                 ylab = "mean of ISR",
                 leg.bty = "o")

########################## Interaction effect end #############################



######################### Box plot start ##################################

boxplot(ISR ~ NN, data = ndnD, xlab = "Number of Nodes",
        ylab = "Interest Satisfaction Rate", main = "Network Data")

boxplot(ISR ~ NI, data = ndnD, xlab = "Number of Interests",
        ylab = "Interest Satisfaction Rate", main = "Network Data")

boxplot(ISR ~ RB, data = ndnD, xlab = "Router Bandwidth",
        ylab = "Interest Satisfaction Rate", main = "Network Data")

boxplot(ISR ~ RD, data = ndnD, xlab = "Number of Nodes",
        ylab = "Interest Satisfaction Rate", main = "Network Data")

#################### Box plot end #####################################



########################## Multiple Regression start #############################

# multiple regression with all factors
ndnres1<-lm(ISR~NI+NN+RD+RB,data=ndnD)
summary(ndnres1)
plot(ndnres1) 

# Stepwise variable selection
ndnres2 <-stepAIC(ndnres1)
plot(ndnres2)
summary(ndnres2)

# multiple regression after removing RD
ndnD2<-ndnD[row.names(ndnD)!="RD",]
ndnres3<-lm(ISR~NI+NN+RB,data=ndnD2)
plot(ndnres3) 
summary(ndnres3)

########################## Multiple Regression end #############################




########################## Multiple Regression with interactions start #############################

ndnres4<-lm(ISR~NI+NN+RD+RB+NN*RD+NI*RB+NI*RD+RB*RD,data=ndnD)
summary(ndnres4)
outlierTest(ndnres4)
plot(ndnres4)

# Stepwise variable selection
ndnres5 <-stepAIC(ndnres4)
plot(ndnres5)
summary(ndnres5)


########################## Multiple Regression with interactions end #############################





##################### model comparison start ###############################
anova(ndnres3,ndnres4)

anova(ndnres3)

###################### model comparison end #############################




########################## mean start #############################
Sum = Summarize(ISR ~   NI, 
                data=ndnD)

Sum$se = Sum$sd / sqrt(Sum$n)
Sum$se
###  This creates a new variable for standard error in our summary data frame
###  Replace n with nvalid if there are missing values 

Sum
qplot(x    = NI , 
      y    = mean,
      data = Sum)  +
  
  
  geom_errorbar(aes(ymin = mean - se, 
                    ymax = mean + se, 
                    width = 0.15))

########################## mean end #############################
