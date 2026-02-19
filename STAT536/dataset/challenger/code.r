#this is the inverse logit function
invlogit <- function(x)
{
 exp(x)/(1+exp(x))	
}

#read the data
#This dataset gives information about the 23 space shuttle flights #before the Challenger disaster. We know the temperature of the time
#of the flight and whether at least one primary O-ring suffered
#thermal distress
data <- read.table('data.txt',header=TRUE);
attach(data);

#preliminary examination of the data
boxplot(Temp~TD,names=c("TD=No","TD=Yes"),ylab="Temperature")

#fit a logistic regression model
mylogit <- glm(TD~Temp,family=binomial(link=logit))

#examine the coefficients of the regression
summary(mylogit)

#determine which summaries are available
names(mylogit)

#plot the fitted values
#first plot everything
plot(Temp,mylogit$fitted.values,xlab="Temperature",ylab="Fitted probabilities")
#plot only the "no thermal distress" observations in blue
points(Temp[TD==0],mylogit$fitted.values[TD==0],col="blue")
#plot only the "thermal distress" observations in red
points(Temp[TD==1],mylogit$fitted.values[TD==1],col="red")
abline(h=0.5)

#the function "fitted(mylogit)" also gives you the fitted values

#how good is the fit of the model?
#calculate the Brier Score
brier = sum((TD-mylogit$fitted.values)^2)

#calculate the error rate = proportion of incorrectly predicted samples
error.rate <- mean((mylogit$fitted.values>0.5 & TD==0) | (mylogit$fitted.values<0.5 & TD==1))

#what about Challenger? The temperature at its lauch was 31F
#calculate the predicted probability of thermal distress
challenger = invlogit(coef(mylogit)%*%c(1,31))
#how could they ever have decided to launch the shuttle that day?

#now let's do the prediction with a more evolved function
predict(mylogit,data.frame(Temp=31),type='response')


#determine the standardized residuals
myres = (TD-mylogit$fitted.values)/sqrt(mylogit$fitted.values*(1-mylogit$fitted.values))
#calculate the p-value for the chisq test
1-pchisq(sum(myres^2),length(TD)-2)

#make an index plot of standardized residuals against observation number
plot(Ft,myres,xlab="Observation number",ylab="Standardized Residual")

#the outliers are outside the (-2,2) interval
a = Ft[abs(myres)>=2]
points(a,myres[a],col='red')

#calculate the aic with the formula
mylogit$deviance+2*mylogit$rank
#you can also get it directly
mylogit$aic

#calculate the bic
bic = mylogit$deviance+mylogit$rank*log(length(TD))

######################################################
#fit two other logistic regressions and compare them
#this is the model with no repressors
mylogit1 <- glm(TD~1,family=binomial(link=logit))
#compute the Brier score, error rate, aic, bic
brier1 = sum((TD-mylogit1$fitted.values)^2)
error.rate1 <- mean((mylogit1$fitted.values>0.5 & TD==0) | (mylogit1$fitted.values<0.5 & TD==1))
mylogit1$aic
bic1 = mylogit1$deviance+mylogit1$rank*log(length(TD))

#now let's include a quadratic term
Temp2 = Temp^2
mylogit2 <- glm(TD~Temp+Temp2,family=binomial(link=logit))
#compute the Brier score, error rate, aic, bic
brier2 = sum((TD-mylogit2$fitted.values)^2)
error.rate2 <- mean((mylogit2$fitted.values>0.5 & TD==0) | (mylogit2$fitted.values<0.5 & TD==1))
mylogit2$aic
bic2 = mylogit2$deviance+mylogit2$rank*log(length(TD))

#test to see if we can delete temperature from the first model
1-pchisq(mylogit1$deviance-mylogit$deviance,1)

#test to see if we can delete temperature^2 from the third model
1-pchisq(mylogit$deviance-mylogit2$deviance,1)