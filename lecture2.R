#lecture 2; unit roots

install.packages("urca") #unit root tests
install.packages("wooldridge")

library(urca)
library(wooldridge)

#import phillips curve data
data("phillips")

#use subset for 1948-96 (<97)
#useful for assignment
phillips96 <- subset(phillips, year<1997)
attach(phillips96)

#test if inflation variable has unit root
#use of ADP, PP and KPSS

#check inflation plot
#can also use gpplot, but pretty much the same 
plot(year,inf)
lines(year,inf)

#augmented dickey fuller
adf <- ur.df(inf, type="none") #can use diff types such as trend/constant rather than none
#here we wanted no trend
adf
print(summary(adf))

#PP w constant (by default or, model="constant")
#by default, includes constant but can also write;
#pp <- ur.pp(inf.model="constant")
#produces ^ same result
pp <- ur.pp(inf)
pp
print(summary(pp))

#KPSS (, mu= constant) 
kpss <- ur.kpss(inf)
kpss
print(summary(kpss))






