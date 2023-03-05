#Seminar 3 - unit roots w/ drift and trend

library(urca) #ADF, PP and KPSS tests
library(wooldridge)
library(dynlm)
library(stargazer)

#import phillips curve data
data("phillips")

#use subset for 1948-96 (<97)
phillips96 <- subset(phillips, year<1997)
attach(phillips96)

#test if inflation variable has unit root: ADF, PP and KPSS tests

#check inflation plot
plot(year,inf)
lines(year, inf)

#Augmented Dickey Fuller
adf <- ur.df(inf, type="none")
adf
print(summary(adf))

#w/ drift (constant)
adf2 <- ur.df(inf, type="drift")
print(summary(adf2))

#w/ drift and trend
adf3 <- ur.df(inf, type="trend")
print(summary(adf3))

#create t variable
phillips96$t <- c(1:49)

#remove variable (if you made a mistake)
phillips96 <- phillips96[ -c(8) ]

#transform to zoo
phillips96.zoo<-zoo(phillips96)
phillips96.zoo$diff<-diff(phillips96.zoo$inf)

#run regression with trend
inflation<-dynlm(diff~inf_1+diff(inf_1)+t, data = phillips96.zoo)
text_model <- stargazer(inflation, type = "text")
write.table(text_model, file = "S3_PG.doc", sep = " ", quote = FALSE, row.names = F)

resid<-inflation$residuals
plot(resid)

#plot differenced series
diff<-diff(inf)
plot(diff)
