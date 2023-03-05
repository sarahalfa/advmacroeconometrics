#Lecture 4; Structural breaks + Data Smoothing 

install.packages("strucchange")
install.packages("TSstudio")
install.packages("strucchange")
install.packages("stargazer")
install.packages("dynlm")
install.packages("pdfetch")

library(strucchange) #test for structural changes, incl. chow test
library(stargazer) #export tables
library(dynlm) #use dynamic linear model
library(TSstudio) # nicer TS plots
library(pdfetch) #import data

#Unemployment rate in the US
fred<-pdfetch_FRED(c("UNRATE","CPIAUCSL"))

#rename it
colnames(fred) [1] <- "unemp"
colnames(fred) [2] <- "infl"


#nicer TS plot
#for other types of dataset (e.g. XTS, Zoo): https://cran.r-project.org/web/packages/TSstudio/vignettes/Plotting_Time_Series.html
ts_info(fred)
ts_plot(fred)

#customise
ts_plot(fred$unemp,
        title = "US Unemployment rate (1948-2022)",
        Xtitle = "",
        Ytitle = "%")

#####structural break#####

#transform to zoo data
fred.zoo<-zoo(fred)

#but also ts
fred.ts<-ts(fred, start = 1948, end=2022, frequency = 12)

#break points - individual series
bp <- breakpoints(fred.zoo$unemp ~ 1)
summary(bp) 

plot(fred.zoo$unemp, xlab="", ylab="%", 
     main="Unemployment rate - USA")
lines(bp)

#coefficient change
#set as data frame
fred1 = data.frame(fred.zoo)
fred1$t<-c(1:913)

#model 1
model1<-dynlm(infl~unemp+t, data=fred1)

#export to word
text_model <- stargazer(model1, type = "text")
write.table(text_model, file = "model1cusum.doc", sep = " ", quote = FALSE, row.names = F)

#cusum
cusum <- efp(model1, type="OLS-CUSUM", data=fred1)
dev.off()
plot(cusum)

#you can check the residuals
resid<-resid(model1)
plot(resid)

#more precise
sctest(cusum)

#chow test
chow<-Fstats(model1, data = fred1)

#plot
plot(chow)

#actual test
sctest(chow)

#where is the change?
breakpoints(chow)
