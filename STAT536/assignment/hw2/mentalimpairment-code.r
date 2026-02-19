setwd("~/Dropbox/Classes/536/CaseStudies/MentalImpairment")

data <- read.table("mentalimpairment-data.txt",header=TRUE)
attach(data)


#first do not take into consideration the order
library(nnet)
M0 = multinom(Impairment ~ 1)
Ms = multinom(Impairment ~ SES)
Me = multinom(Impairment ~ Events)
M = multinom(Impairment ~ SES + Events)

1-pchisq(M0$deviance-Me$deviance,length(coef(Me))-length(coef(M0)))

BIC(M0)
BIC(Ms)
BIC(Me)
BIC(M)

AIC(M0)
AIC(Ms)
AIC(Me)
AIC(M)


#now take the order into account
library(MASS)

Impairment = factor(Impairment,levels=c("Well","Mild","Moderate","Impaired"))

orderM0 = polr(Impairment ~ 1)
orderMs = polr(Impairment ~ SES)
orderMe = polr(Impairment ~ Events)
orderM = polr(Impairment ~ SES + Events)

#get the slopes
coef(orderM)
#get the intercepts
orderM$zeta
#fitted probabilities
orderM$fitted
