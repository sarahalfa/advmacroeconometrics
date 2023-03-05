#Seminar 4; Structural breaks and data smoothing

library(strucchange) #test for structural changes, incl. chow test
library(stargazer) #export tables
library(dynlm) #use dynamic linear model
library(TSstudio) # nicer TS plots
library(pdfetch) #import data
library(urca)
library(ggplot2)
library(xts)

#using pdfetch, load the real GDP data from FRED
fred<-pdfetch_FRED(c("GDPC1"))

#data frequency?

#rename the variable to "rgdp"
colnames(fed) [1] <- "rgdp"

#create a graph w the series
ts_info(fred)
ts_plot(fred)
ts_plot(fred$unemp,
        title = "US Real GDP (1947-2022)",
        Xtitle = "",
        Ytitle = "%")
model1<-dynlm(fred$GDPC1)

#check for unit roots using the ADF test (none, drift and trend versions)
adf<-ur.df(rgdp, type="NONE")
adf
summary(adf)
#which model do you think is closer to the "true" model of US real GDP?
#write in equation format



