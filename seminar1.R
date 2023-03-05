#use pdfetch to import data for bitcoin in £ using highest price of day
#start 14/9/2014; plot; take 1st difference if non stationary

library(pdfetch)

cryptocurrency<-c("BTC-GBP")

yahoo<-pdfetch_YAHOO(cryptocurrency, fields = "high",
                     from="2014-09-14")

colnames(yahoo) [1]  <- "Bitcoin"

plot(yahoo$Bitcoin, las=2)

diffBitcoin<-diff(yahoo$Bitcoin) 
plot(diffBitcoin, las=2)
