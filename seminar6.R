#Exercise 6; Diagnostic Tests
#Group Presentation 

library(pdfetch) #import data
library(dynlm) #dynlm regression between variables
library(ARDL) #find best ARDL model
library(stargazer) #export regressions
library(zoo) #transform data to zoo

#Import data using pdfetch, for; 
  #Market Yield on US Treasury Securities at 10-Year Constant Maturity 
  #& Federal Funds Effective Rate 
fred<-pdfetch_FRED(c("DGS10", "DFF"))
  #Change variable names
colnames(fred) [1] <- "MktYield"
colnames(fred) [2] <- "FedRate"

#Plot dataset, using the par command
par(mfrow=c(1,2))
plot(fred$MktYield, xlab="Year", ylab="Rate", main= "Mkt Yield on US Treasuries")
plot(fred$FedRate, xlab="Year", ylab="Rate", main="Federal Reserve Effective Rate")
#Ensure there are no missing values in the dataset
fred<-na.omit(fred)

#Use the auto_ardl command w AIC and BIC in order to select the most appropriate ARDL model 
  #on the effects of interest rates and bond yield in the US
#AIC approach
AICARDL<-auto_ardl(Federal_Rate~Mkt_Yield, selection = "AIC", data = fred, max_order = 5)
AICARDL
#BIC approach
BICARDL<-auto_ardl(Federal_Rate~Mkt_Yield, selection = "BIC", data = fred, max_order = 5)
BICARDL
#run ARDL regressions - convert to zoo first!
fred.zoo<-zoo(fred)
#AIC suggested model
reg1<-dynlm(Federal_Rate~L(Federal_Rate,1)+ L(Federal_Rate,2)+ Mkt_Yield+ L(Mkt_Yield,1) 
            +L(Mkt_Yield,2) +L(Mkt_Yield,3), data = fred.zoo)
print(summary(reg1))


---------
  
  
  #seminar 6
  
library(pdfetch) #import data
library(dynlm) #dynlm regression between variables
library(strucchange) #structural change
library(ARDL)
library(stargazer)
library(lmtest)


#Import data using pdfetch
FRED_DATA<-pdfetch_FRED(c("DGS10", "DFF"))

#Change variable names
colnames(FRED_DATA)[1]<- "Market_Yield_10_Year"
colnames(FRED_DATA)[2]<- "FED_Funds_Effective_Rate"

#Plot dataset, using the par command
par(mfrow=c(1,2))
plot(FRED_DATA$Market_Yield_10_Year, xlab="Year", ylab="Rate", main= "Mkt Yield on US Treasuries")
plot(FRED_DATA$FED_Funds_Effective_Rate, xlab="Year", ylab="Rate", main="Federal Reserve Effective Rate")

#Transform data into data frame
fred_data = data.frame(FRED_DATA)

#Remove N/A observations
fred_data<-na.omit(fred_data)

#transform to zoo 
fred_data_zoo = zoo(fred_data)


####lag selection
ardl_aic_2<-auto_ardl(Market_Yield_10_Year ~ FED_Funds_Effective_Rate,
                      selection="AIC", data = fred_data, max_order = 5)

ardl_aic_2$best_order

ardl_bic_2<-auto_ardl(Market_Yield_10_Year ~ FED_Funds_Effective_Rate,
                      selection="BIC", data = fred_data, max_order = 5)
ardl_bic_2$best_order

#ARDL model 
#using dynlm
model1 <- dynlm(Market_Yield_10_Year ~ L(Market_Yield_10_Year,1:2 ) + FED_Funds_Effective_Rate + L(FED_Funds_Effective_Rate, 1), data = fred_data_zoo)
print(summary(model1))

#using ardl
model2 <- ardl(Market_Yield_10_Year ~ FED_Funds_Effective_Rate, data = fred_data_zoo, order = c(2,1) )
print(summary(model2))


#export results
text_model <- stargazer(model1, type = "text")
write.table(text_model, file = "seminar6.doc", sep = " ", quote = FALSE, row.names = F)

#Test for serial correlation
dwtest(model1)

#dynlm ARDL model
reg3<-ardl(Market_Yield_10_Year~FED_Funds_Effective_Rate, data= fred_data_zoo, order = c(2,1))
reg3 #cool result but way measured is slightly diff #w this one, may be diff to use the stargazer so potentially stick to above


#Multiplier effect; short v long run multiplier
#gives same result as above dynlm ARDL model but could be helpful if u want to double check what the short run and long run is w this command 
multipliers(reg3, type ="sr") #small but positive in short run 
multipliers(reg3, type = "lr") #long run no effect when looking at bond yield and federal funds rate
#likely as there WAS a strong relationship of low IR and bond yield doesnt accompany this THUS may have had policy introduced that disturbs this relationship 
#lr here not the cumulative effect, as result is 0.7, thus this is equilibrium long run, which is calculated differently
#cumulative represents best long run effect, not this command, so do it manuall using text_model<-stargazer table where u add each ie add 1.064+-0.065+0.010 etc [see table of dependent variables]
