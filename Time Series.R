#Time Series Data ----
#autocorrelations exists in a time series data[level of gasoline data at a gas station]
require(WDI)
countriesc <- c('US','CA','GB','NG','CN','JP','SG','IL')
indicators <- c('NY.GDP.PCAP.CD','NY.GDP.MKTP.CD')
gdpcountries <- WDI(country = countriesc,indicator = indicators,start=1960,end = 2018)
names(gdpcountries) <- c('Iso2c', 'Country','Year','PerCapGDP','GDP')
head(gdpcountries)

require(ggplot2)
require(scales) #dollar is a special function in the scales package. You need to load the scales package
ggplot(gdpcountries,aes(Year,PerCapGDP, color=Country, linetype=Country))+geom_line()+
    scale_y_continuous(label=dollar)

us <- gdpcountries$PerCapGDP[gdpcountries$Country=='United States']
View(us)
us <- ts(us,start = min(gdpcountries$Year), end=max(gdpcountries$Year))
us
plot(us, ylab='Per Capita GDP', xlab='Year')

#ACF - Auto correlation function
acf(us)
#PACF - partial autocorrelation function
pacf(us)



#Arima----
#Auto Regressive Integrated Moving Average
require(forecast)
usBest <- auto.arima(x=us)
usBest #-An ARIMA(0,2,2) model is shown. an AR component of 0, a Moving Average of 2, and a dataset difference Twice(2)
acf(usBest$residuals) #None of the lines crossed the dotted horizontal lines. A good fit
pacf(usBest$residuals) #A lag crossed the horizontal line

coef(usBest) #Time series to predict price of stock
predict(usBest, n.ahead=5, se.fit=TRUE)

theforecast <- forecast(usBest,h = 5)
theforecast
plot(theforecast)



#Multiple Time Series using VAR---- 
#VAR - Vector Autoregressive Model
head(gdpcountries)
require(reshape2)
#gdpCast <- dcast(gdpcountries, Iso2c+Country~Year, value.var = 'GDP')
gdpCast <- dcast(gdpcountries,Year~Country, value.var = 'PerCapGDP')
head(gdpCast)
gdpTS <- ts(gdpCast[,-1], start = min(gdpCast$Year), end = max(gdpCast$Year))
head(gdpTS)
plot(gdpTS)
plot(gdpTS, plot.type = 'single',col=1:8)
legend('topleft', legend=colnames(gdpTS), ncol=2, lty=1, col = 1:8, cex=0.9)


numDiffs <- ndiffs(gdpTS)
numDiffs
gdpDiffed <- diff(gdpTS, differences=numDiffs)
plot(gdpDiffed, plot.type = 'single', col=1:7)

require(vars)
gdpVar <-VAR(gdpDiffed, lag.max=12)
gdpVar$p # Number of Lags

names(gdpVar$varresult)
class(gdpVar$varresult$Nigeria)

head(coef(gdpVar$varresult$Nigeria), 10)

require(coefplot)
coefplot(gdpVar$varresult$Nigeria)
coefplot(gdpVar$varresult$Canada)

predict(gdpVar, n.ahead = 5)

#GARCH ----

require(quantmod)
att <- getSymbols('T')
require(xts)
head(att)
plot(att)

chartSeries(att)
addBBands()
addMACD()
addMACD(32,50,12)

attClose <- att$T.Close
class(attClose)
head(attClose)

