#read the data; the file is saved in STATA format, so you need this library:
library(foreign)
data <- read.dta('MROZ.dta');

#the wage of the non-working women is zero, hence the variables
#wage and lwage (columns 7 and 21) will contain missing values
#we eliminate these variables from the data

data <- data[,c(1,3:6,8:19,22)]

#tranform some variables
data[,"age"] = log(data[,"age"])
data[,"hushrs"] = log(data[,"hushrs"])

#display the names of the variables
colnames(data)

for(i in 2:18)
{
	a = data[,i]
	data[,i] = (a-mean(a))/sd(a)	
}

#now the data contains 753 samples and 19 variables
attach(data)

#fit the model involving all the 18 indendent variables
fullmodel = glm(inlf~.,family=binomial(link=logit),data=data)

#find the correlation of each explanatory variable with the response
for(i in 2:19)
{
	cat(colnames(data)[i],"[",i,"] = ",cor(data[,1],data[,i]),"\n");	
}

#fit a logistic regression with these variables
M0 = glm(inlf~kidslt6+educ+repwage+exper,family=binomial(link=logit),data=data)


#make a plot of the fitted values
myind = 1:length(inlf)
plot(myind,M0$fitted.values,xlab="Observation number",ylab="Fitted probabilities")
points(myind[inlf==0],M0$fitted.values[inlf==0],col="blue")
points(myind[inlf==1],M0$fitted.values[inlf==1],col="red")
abline(h=0.5)

Mempty = glm(inlf~1,family=binomial(link=logit),data=data)
Mfull = glm(inlf~.,family=binomial(link=logit),data=data)
M1 = step(Mempty,trace=TRUE,direction="both",scope=list(upper = Mfull,lower = Mempty))

#now try this model
mylogit <- glm(inlf~hours+kidslt6+educ+exper,family=binomial(link=logit))

#one more
mylogit <- glm(inlf~kidslt6+educ+exper+expersq,family=binomial(link=logit))

#make a plot of the fitted values
myind = 1:length(inlf)
plot(myind,mylogit$fitted.values,xlab="Observation number",ylab="Fitted probabilities")
points(myind[inlf==0],mylogit$fitted.values[inlf==0],col="blue")
points(myind[inlf==1],mylogit$fitted.values[inlf==1],col="red")
abline(h=0.5)

#determine the standardized residuals
myres = (inlf-M0$fitted.values)/sqrt(M0$fitted.values*(1-M0$fitted.values))
#calculate the p-value for the chisq test
1-pchisq(sum(myres^2),length(inlf)-length(coef(M0)))

#make an index plot of standardized residuals against observation number
plot(1:length(inlf),myres,xlab="Observation number",ylab="Standardized Residual")

#the outliers are outside the (-2,2) interval
a = (1:length(inlf))[abs(myres)>=2]
points(a,myres[a],col='red')


#calculate the Brier Score
brier =  mean((inlf-mylogit$fitted.values)^2)

#calculate the error rate = proportion of incorrectly predicted samples
error.rate <- mean((mylogit$fitted.values>0.5 & inlf==0) | (mylogit$fitted.values<0.5 & inlf==1))

#determine the standardized residuals
myres = (inlf-mylogit$fitted.values)/sqrt(mylogit$fitted.values*(1-mylogit$fitted.values))
#calculate the p-value for the chisq test
1-pchisq(sum(myres^2),length(inlf)-length(coef(mylogit)))

#make an index plot of standardized residuals against observation number
plot(1:length(inlf),myres,xlab="Observation number",ylab="Standardized Residual")

#the outliers are outside the (-2,2) interval
a = (1:length(inlf))[abs(myres)>=2]
points(a,myres[a],col='red')

#calculate the aic with the formula
mylogit$deviance+2*mylogit$rank
#you can also get it directly
mylogit$aic

#calculate the bic
bic = mylogit$deviance+mylogit$rank*log(length(inlf))

#now use the stepwise function to see if we can improve the model
library(stats)
mylogit1 <- glm(inlf~.,family=binomial(link=logit),data=data)
newmodel <- step(mylogit1,trace=TRUE)