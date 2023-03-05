#class 5

library(pdfetch) #import data
library(dynlm) #dynlm regression between variables
library(strucchange) #structural change
library(dynlm) #use dynamic linear model
library(ARDL) #find best ARDL model
library(wooldridge) #import wooldridge dataset
library(urca) #ADF, PP and KPSS tests

US_DATA<-pdfetch_FRED(c("DGS10", "DFF"))

colnames(US_DATA)[1]<- "Market_Yield_10_Year"
colnames(US_DATA)[2]<- "FED_Funds_Effective_Rate"

par(mfrow=c(1,2))
plot(US_DATA$Market_Yield_10_Year, xlab="Year", ylab="Rate", main= "Mkt Yield on US Treasuries")
plot(US_DATA$FED_Funds_Effective_Rate, xlab="Year", ylab="Rate", main="Federal Reserve Effective Rate")

fred_data = data.frame(US_DATA)
fred_data<-na.omit(US_DATA)
fred_data$t<-c(1:15272)

model<-dynlm(Market_Yield_10_Year~FED_Funds_Effective_Rate, data = fred_data)

chow_fred<-Fstats(model, data = fred_data)
sctest(chow_fred)

breakpoints(Chow_fred)





#Transform data into data frame
fred_data = data.frame(FRED_DATA)

#Remove N/A observations
fred_data<-na.omit(FRED_DATA)

#Create trend variable
fred_data$t<-c(1:15270)

#Create Model
model1<-dynlm(Market_Yield_10_Year ~ FED_Funds_Effective_Rate, data=fred_data)

#Chow test
Chow_fred<-Fstats(model1, data = fred_data)

#actual test; chow test restrictive as only gives 1 breakpoint; cusum test gives multiple breakpoints
sctest(Chow_fred)

#Where is the change?
breakpoints(Chow_fred)



#check for unit roots using the ADF test (none, drift and trend versions)
adf.drift<- ur.df(fred_data$Market_Yield_10_Year, type ="drift")
adf.drift
summary(adf.drift)

#dummy variable



