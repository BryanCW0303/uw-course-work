data <- read.table("alligatorfood-data.txt",header=TRUE)
attach(data)

#we need this library for the 'multinom' function
library(nnet)

y = cbind(Fish,Invertebrate,Reptile,Bird,Other)


M1 = multinom(y ~ 1)
M2 = multinom(y ~ Lake)
M3 = multinom(y ~ Gender)
M4 = multinom(y ~ Size)
M5 = multinom(y ~ Lake + Gender)
M6 = multinom(y ~ Lake + Size)
M7 = multinom(y ~ Gender + Size)
M8 = multinom(y ~ Lake+Gender+Size)

#LRT
1-pchisq(Msmall$deviance-Mbig$deviance,length(coef(Mbig))-length(coef(Msmall)))
#p-value is 0.696, we fail to reject, hence there does not seem to be a gender effect



