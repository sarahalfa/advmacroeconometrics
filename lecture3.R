#Lecture 3 - Trends

library(readxl) #import excel files
library(dynlm) #dynamic models
library(stargazer) #export nice tables

#import from excel
rgdp <- read_excel("C:/Users/thereza/OneDrive - SOAS University of London/SOAS/Modules/Advanced Econometrics/2022-23/Lecture 3 - Trends/rgdp.xls")

#attach dataset (doesn't work with ts)
attach(rgdp)

#transform to TS
tsdata<-ts(rgdp, start = c(1947,1), end=c(2012,4), frequency = 4)
plot(DATE, RGDP, data=rgdp)


#run the Real GDP model with only deterministic trends
model1<-dynlm(RGDP~t+I(t^2)+I(t^3), data = tsdata)
model1
text_model <- stargazer(model1, type = "text")
write.table(text_model, file = "model1PG.doc", sep = " ", quote = FALSE, row.names = F)

#residuals
Resids=model1$residuals
plot(model1$residuals)

#ACF residuals
acf(RGDP)
acf(Resids)


