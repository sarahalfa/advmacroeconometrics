#lecture 1; time series and stationary 
 
install.packages("pdfetch") #fetch data 
#imports data from internet database

library(pdfetch)

#time series of financial data
#^gspc=S&P 500, ^ixic=NASDAQ Composite, AAPL=Apple Inc
#3 diff variables
#first create a vector for these
stock<-c("^gspc", "^ixic", "AAPL")

#import data
#runs time series dataset from 2000-01-01 to last trading day
yahoo<-pdfetch_YAHOO(stock, fields = "adjclose",
                     from="2000-01-01")

#the last data id left out so get the most recent one
Sys.Date()

#rename variables
colnames(yahoo) [1]  <- "SP500"
colnames(yahoo) [2] <- "NASDAQ"
colnames(yahoo) [3] <- "Apple"

#plot data on s&p500 stock prices
#las means labels are parallel (=0) or perpendicular
plot(yahoo$SP500, las=2)

#plot differencing to create stationary time series
diffSP500<-diff(yahoo$SP500) #creates new value//dataset
plot(diffSP500, las=2)






