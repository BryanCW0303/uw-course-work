setwd("~/Dropbox/Classes/536/CaseStudies/PoliceStops")

frisk <- read.table("frisk.dat",header=TRUE)
attach(frisk)

n <- length(unique(precinct))*length(unique(eth))
mystops <- numeric(n)
myarrests <- numeric(n)
myprecinct <- numeric(n)
myeth <- numeric(n)

k <- 1
for(i in 1:length(unique(precinct)))
{
	for(j in 1:length(unique(eth)))
	{
		myprecinct[k] = i
		myeth[k] = j
		mystops[k] = sum(frisk[(precinct==i) & (eth==j),1])
		myarrests[k] = sum(frisk[(precinct==i) & (eth==j),3])
		k = k+1
	}
}

data <- read.table("policestops.txt",header = TRUE)
attach(data)

mydata <- cbind(mystops,myarrests,myprecinct,myeth)
write.table(mydata,file="policestops.txt",row.names=FALSE,col.names=FALSE)

model1A <- glm(mystops ~ 1,family=poisson(link = "log"))
summary(model1A)

model2A <- glm(mystops ~ factor(myeth),family=poisson(link = "log"))
summary(model2A)

model3A <- glm(mystops ~ factor(myeth)+factor(myprecinct),family=poisson(link = "log"))
summary(model3A)

###########################################
model1B <- glm(mystops ~ log(myarrests),family=poisson(link = "log"))
summary(model1B)

model2B <- glm(mystops ~ log(myarrests)+factor(myeth),family=poisson(link = "log"))
summary(model2B)

model3B <- glm(mystops ~ log(myarrests)+factor(myeth)+factor(myprecinct),family=poisson(link = "log"))
summary(model3B)

#################################################
model1 <- glm(mystops ~ 1,family=poisson(link = "log"),offset=log(myarrests))
summary(model1)

model2 <- glm(mystops ~ factor(myeth),family=poisson(link = "log"),offset=log(myarrests))
summary(model2)

model3 <- glm(mystops ~ factor(myeth)+factor(myprecinct),family=poisson(link = "log"),offset=log(myarrests))
summary(model3)

p = length(coef(model3))
yhat = predict(model3,type="response")
#residual plot
z = (mystops-yhat)/sqrt(yhat)
par(mfrow=c(1,2))
plot(yhat,mystops-yhat,xlab="Predicted values",ylab="Raw residuals")
plot(yhat,z,xlab="Predicted values",ylab="Standardized residuals")

#overdispersion ratio
sum(z^2/(n-p))
#p-value
1-pchisq(sum(z^2),n-p)

#Negative binomial model
library(MASS)
model4 <- glm.nb(mystops ~ factor(myeth)+factor(myprecinct))
summary(model4)




################################################################
data <- read.table("policestops.txt",header = TRUE)
attach(data)

model1 <- glm(stops ~ factor(ethnicity)+factor(precinct),family=poisson(link = "log"),offset=log(arrests))

model2 <- glm(stops ~ factor(precinct),family=poisson(link = "log"),offset=log(arrests))

model3 <- glm(stops ~ factor(ethnicity),family=poisson(link = "log"),offset=log(arrests))

model4 <- glm(stops ~ 1,family=poisson(link = "log"),offset=log(arrests))

################################################################

ratio1 = rep(0,75)
ratio2 = rep(0.75)

for(i in 1:75)
{
 ratio1[i] = arrests[(precinct==i)&(ethnicity==2)]*exp(coef(model1)["factor(ethnicity)2"])/arrests[(precinct==i)&(ethnicity==1)]
 ratio2[i] = arrests[(precinct==i)&(ethnicity==3)]*exp(coef(model1)["factor(ethnicity)3"])/arrests[(precinct==i)&(ethnicity==1)]	
}

plot(1:75,ratio2,type="l",lwd=2,xlab="Precinct",ylab="Ratio of expected stops")
lines(1:75,ratio1,lwd=2,lty="dotted")
abline(h==1)