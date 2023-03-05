#use pdfetch command to import data for unemployment 

library(pdfetch)
library(urca)

#in the USA (UNRATE using FRED)

unemployment<-c("UNRATE")
FRED<-pdfetch_FRED(unemployment)

colnames(FRED) [1]  <- "unemployment"

#plot results 
plot(FRED$unemployment)
#if conducing ggplot, write following 
#ts.plot(fred)
#plot(fred)

#take the first difference if you think there is non-stationary
diffFRED<-diff(FRED$unemployment) #creates new value/dataset 
plot(diffFRED, las=2)

#test for unit root using the augmented dickey fuller method
#UR evident; fail to reject null hypothesis
#can also use the FRED to get inflation + run dataset;
#to then do both regression taking 1st diff and not+compare
adf <- ur.df(FRED$unemployment, type="none")
adf
summary(print(adf))

#test for unit root using the pp method 
#no UR evident 
pp <- ur.pp(FRED$unemployment)
pp
summary(print(pp))

#test for unit root using the kpss method
kpss <- ur.kpss(FRED$unemployment)
kpss
print(summary(kpss))

