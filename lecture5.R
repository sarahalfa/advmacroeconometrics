#Lecture 5; ARDL

library(stargazer) #export tables
library(dynlm) #use dynamic linear model
library(ARDL) #find best ARDL model
library(wooldridge) #import wooldridge dataset

#import data
data("phillips")

#graph
par(mfrow=c(2,1))
  dev.off()

plot(phillips$year, phillips$unem)
plot(phillips$year, phillips$inf)

#transform data to TS
tsphil<-ts(phillips, start=c(1948), frequency=1)

#lag selection
ardl_aic<-auto_ardl(inf ~ unem, selection="AIC", data = tsphil, max_order = 5)
ardl_aic

#bic
ardl_bic<-auto_ardl(inf ~ unem, selection="BIC", data = tsphil, max_order = 5)

#estimate ardl model
model1<- dynlm(inf ~ L(inf) + L(inf, 2) + L(inf, 3) + unem + L(unem, 1:5), data =tsphil)
model1

#export to word
text_model<-stargazer(model1, type = "text")
write_table(text_model, file = "ardl_aic.doc", sep = "", quote = FALSE, row.names(x))

#with the ARDL command
model2<-ardl(inf~unem, data=tsphil, order=c(3,5,))
summary(model2)

#short v long run effect
multipliers(model2, type="sr")
multipliers(model2, type="lr")





