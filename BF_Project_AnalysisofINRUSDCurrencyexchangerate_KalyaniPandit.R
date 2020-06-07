# LOAD YOUR PACKAGES
library(ggplot2)
library(forecast)
library(plotly)
library(ggfortify)
library(tseries)
library(gridExtra)
library(docstring)
library(readr)
library(lubridate)

#Taking CSV data into a dataframe
data_exchng<-read.csv("C:/Alok/OneDrive/Rutgers_MITA/Semester1/BusinessForecasting_Turkoz/BF_Project/BF_Project_Kals_USDINRExchange/USD_INR_2_Finaldataset.csv")
data_exchng
View(data_exchng)
#total rows = 3957 
#Data from 01/01/2009 to 11/01/2019
attach(data_exchng)
names(data_exchng)

#Splitting data into training a(01012009 to 123120018) and testing data (01012019 till 11012019)
#Creating Training data from 01012009 till 12312018 #rOWS 1 TO 3652
df1<-data_exchng[c(1:3652),]
View(df1)

#Creating Test data from 01012019 till 11012019 #rows 3653 till 3957
df2<-data_exchng[c(3653:3957),]
View(df2)

#cRATING TIME SERIES tRAINING DATA
#INRUSDTrain <- ts(df1$Price,frequency = 365)

inds <- seq(as.Date("2009-01-01"), as.Date("2019-01-01"), by = "day")
INRUSDTrain <- ts(df1$Price,     # random data
                  start = c(2009, as.numeric(format(inds[1], "%j"))),
                  end = c(2019, as.numeric(format(inds[1], "%j"))),
                  frequency = 365)

View(INRUSDTrain)
plot(INRUSDTrain)
#cRATING TIME SERIES Testing DATA
#INRUSDTest <- ts(df2$Price, frequency = 365)
INRUSDTest <- ts(df2$Price,     # random data
                 start = c(2019, as.numeric(format(inds[1], "%j"))),
                 #                  end = c(2019, as.numeric(format(inds[1], "%j"))),
                 frequency = 365)
View(INRUSDTest)
plot(INRUSDTest)

#Dataframe with entire data which is ony used for initially plotting the data
INRUSD_Price2 = ts(data_exchng$Price, start=c(2009, 1), freq=365.25)
INRUSD_Price2
View(INRUSD_Price2)
plot(INRUSD_Price2,col='blue') 

#Checking if there are any null values in the Price column od data
sum(is.na(data_exchng[,2]))
data_exchng
summary(data_exchng$Price)
#Compactly Display the Structure of an Arbitrary R Object
str(Price)
dim(data_exchng)

#Fitting models onto our training dataset now
#STL decomposition 

fitstl <- stl(INRUSDTrain, s.window=7)
fitstl
#plotting S T L decomposition
plot(fitstl)
#Seasonaly adjusted data Yt-St
seasadj(fitstl)
plot(seasadj(fitstl))
#Plotting only seasonal data
plot(fitstl$time.series[,1], col="gray", main="Seasonality component from STL decomposition", ylab="seasonality", xlab="")
#Plotting only trend data
plot(INRUSDTrain, col="black")
plot(fitstl$time.series[,2], col="blue", main="Trend component from STL decomposition", ylab="trend", xlab="")

#Plotting original data and seasonally adjusted data toether to chk if seasonality
plot(INRUSDTrain,col="black")
lines(seasadj(fitstl),col="blue")

ggseasonplot(INRUSDTrain, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Price") +
  ggtitle("Seasonal plot: INR USD EXCHNG Rate")

ggseasonplot(INRUSDTrain, polar=TRUE) +
  ylab("Price") +
  ggtitle("Seasonal plot: INR USD EXCHNG Rate")

ggsubseriesplot(INRUSDTrain) +
  ylab("Price") +
  ggtitle("Seasonal subseries plot: INRUSDEXCHNGRATE")

#Holt winters linear method (Linear as our data doesnt have seasonality)

fcholt <- holt(INRUSDTrain, h=305)                 
fitted(fcholt)
fcholt
round(accuracy(f=fcholt,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)

autoplot(INRUSDTrain) +
  autolayer(fcholt, series="Holt's forecast", PI=FALSE) +
  autolayer(fitted(fcholt),series = "Holt fitted",PI=FALSE)+
  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))

#Or to plot
#plot(INRUSDTrain,col="red")
#lines(fitted(fcholt),col="blue")#fitted data with arima model which r estimated values
#plot(fcholt,col="yellow") #this plots forcast with the original data
#lines(INRUSDTest,col="red")
#======SNaive method not needed as our data is nonseasonal========
#snfit = snaive(INRUSDTrain,h=305)
#fitted(snfit)
#snfc = forecast(snfit,h=305)
#snfc
#round(accuracy(f=snfc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)
#autoplot(INRUSDTrain) +
#  autolayer(snfc, series="Seasonal Naive forecast", PI=FALSE) +
#  autolayer(fitted(snfit),series = "Seasonal Naive fitted",PI=FALSE)+
#  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
#  ggtitle("Forecasts from Seasonal Naive method") + xlab("Year") +
#  ylab("INR USD Exchange rate in RS") +
#  guides(colour=guide_legend(title="Forecast"))

#==============Naive Method=========
nfit = naive(INRUSDTrain,h=305)
fitted(nfit)
nfc = forecast(nfit,h=305)
nfc
round(accuracy(f=nfc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)

autoplot(INRUSDTrain) +
  autolayer(nfc, series="Naive forecast", PI=FALSE) +
  autolayer(fitted(nfit),series = "Naive fitted",PI=FALSE)+
  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
  ggtitle("Forecasts from Naive method") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))

#===============Mean Method=============
mnfit = meanf(INRUSDTrain,h=305)
fitted(mnfit)
mnfc = forecast(mnfit,h=305)
mnfc
round(accuracy(f=mnfc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)

autoplot(INRUSDTrain) +
  autolayer(mnfc, series="Mean forecast", PI=FALSE) +
  autolayer(fitted(mnfit),series = "Mean fitted",PI=FALSE)+
  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
  ggtitle("Forecasts from Mean method") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))

#===============================
#tests for stationarity
#KPSS test ==> H0=data is stationary
#small p-values (e.g., less than 0.05) suggest that differencing is required. 
#Test=kpss.test(INRUSDTrain) 
#summary(Test)

#Test 2 ADF test H0 = Time series is not stationary
test2=adf.test(INRUSDTrain)
test2
#Pvalue 0.3235 > 0.05 so we can not reject null hypothesis
#So time series is not stationary

#//////////////

#As data is nonstationary, differencing

tsDiff <- diff(INRUSDTrain)
plot(tsDiff)
test2r=adf.test(tsDiff)
test2r
#P value for 1st difference data is p-value = 0.01 so we reject Ho

#To Crosscheck how much differencing is needd
ndiffs(INRUSD_Price2)
#-----------
#==============ARIMA Method========== 

plot(Acf(INRUSDTrain,lag.max=100))
plot(Pacf(INRUSDTrain,lag.max = 100))

plot(Acf(tsDiff,lag.max = 100))
plot(Pacf(tsDiff,lag.max = 100))

#To know p and q parameters using
#auto.arima()
#arimafit<-auto.arima(INRUSDTrain) #(3,1,4)
arimafit<-auto.arima(INRUSDTrain, seasonal = FALSE) #(3,1,4)
arimafit
fitted(arimafit)
arimafc = forecast(arimafit,h=305)
arimafc
round(accuracy(f=arimafc,x=INRUSDTest,test=NULL,d=NULL,D=NULL), 3)

autoplot(INRUSDTrain) +
  autolayer(arimafc, series="ARIMA forecast", PI=FALSE) +
  autolayer(fitted(arimafit),series = "ARIMA fitted",PI=FALSE)+
  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
  ggtitle("Forecasts from ARIMA method") + xlab("Year") +
  ylab("INR USD Exchange rate in RS") +
  guides(colour=guide_legend(title="Forecast"))

#//////////////////////////
#Plotting all the method graphs together

autoplot(INRUSDTrain) +
  autolayer(meanf(INRUSDTrain, h=305),
            series="Mean", PI=FALSE) +
  autolayer(naive(INRUSDTrain, h=305),
            series="Naïve", PI=FALSE) +
#  autolayer(snaive(INRUSDTrain, h=305),
#            series="Seasonal Naïve", PI=FALSE) +
  autolayer(holt(INRUSDTrain, h=305),
            series="HoltWinter", PI=FALSE) +
  autolayer(INRUSDTest,series = "Test Data",PI=FALSE)+
  autolayer(arimafc,
            series="ARIMA", PI=FALSE) +
  ggtitle("INR - USD -Currency Exchange Rate") +
  xlab("Year") + ylab("Price (INR)") +
  guides(colour=guide_legend(title="Forecast"))

#///////////////
#Our next step is to run residual diagnostics to ensure our residuals are 
#white noise under our initial assumptions. For this we will use the ggtsdisplay()
library(ggplot2)
library(ggfortify) #installed pkg ggfortify to use ggtsdiag function
library(urca)
# RESIDUAL DIAGNOSTICS for ARIMA model
ggtsdiag(arimafit) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line.y = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray"))

#In Standardized residual plot, 
#The large positive residual is a result of the unexpected price jump between 2013 and 2014.
#In ACF of Residuals
#The lack of correlation suggesting the forecasts are good.

plot(INRUSDTrain)

?ggplot
?geom_histogram
#Plotting histogram of residuals  
residFit <- ggplot(data=arimafit, aes(residuals(arimafit))) +
  geom_histogram(aes(y =..density..),  
                 binwidth = 5,
                 col="turquoise4", fill="white") +
  geom_density(col=1) +
  theme(panel.background = element_rect(fill = "gray98"),
        panel.grid.minor = element_blank(),
        axis.line   = element_line(colour="gray"),
        axis.line.x = element_line(colour="gray")) +
  ggtitle("Plot of INRUSD Price ARIMA Model Residuals")


#Plotting residual histogram
plot(residFit)

