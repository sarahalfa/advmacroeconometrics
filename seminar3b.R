
library(pdfetch)
library(urca)
library(ggplot2)
library(xts)

#find corresponding code for the series mentioned
#import series into R using pfetch command
ANLT<-pdfetch_ONS("ANLT","PUSF")
colnames(ANLT) <- c("PublicSpend")

#frequency of series
summary(ANLT)
periodicity(ANLT)

#plot graph of the series + analyse
plot(ANLT)

ggplot(ANLT, aes(x = Index, y = PublicSpend)) +
  geom_line(color="dark blue") +
  xlab("month/year") +
  ylab("£m CPNA")+
  ggtitle("PS: Total current expenditure (£m CPSNA), 04/1997-12/2022")+
  scale_y_continuous(lables = scales::comma)+
  scale_x_date(breaks = seq(as.Date("1997-04-30"), as.Date("2022-12-31"), by="18 months"), date_labels = "%b\n%Y")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size= 8))+
  #plot.title = element_text(size = 10, face="bold") axis.text.y = element_text(size = 8) axis.title = element_text(size = 8)

#test for the potential stationarity of the series using the ADF test (no intercept nor trend)
#address it if necessary

adf <- ur.df(ANLT, type="none")
adf
print(summary(adf))

diffPublicSpend<-diff(ANLT)
ggplot(diffPublicSpend, aes(x = Index, y = PublicSpend))+
  geom_line()+
  ggtitle("Correcting Unit Root w First Difference")+
  xlab("Date")+
  ylab("£m CPNSA")

#add intercept and trend
#analyse same variable but w the phillips peron test
pp <- ur.pp(ANLT$PublicSpend)
pp
summary(print(pp))



