#Non Linear Least Squares
#To find location of Wifi around an office
load('C:/Users/user/Downloads/wifi.rdata')
head(wifi)
require(ggplot2)
ggplot(wifi, aes(x=x,y=y,color=Distance)) +geom_point() + scale_color_gradient2(low='blue', mid='white', high='red',
                                                                                midpoint = mean(wifi$Distance, na.rm=TRUE))
wifiMod1 <- nls(Distance~ sqrt((betax - x)^2+(betay-y)^2), data=wifi,start = list(betax=50, betay=50))
summary(wifiMod1)
ggplot(wifi, aes(x=x,y=y,color=Distance)) +geom_point() + scale_color_gradient2(low='blue', mid='white', high='red',
                                                                                midpoint = mean(wifi$Distance, na.rm=TRUE))+
    geom_point(data = as.data.frame(t(coef(wifiMod1))), aes(x=betax, y=betay), size=6, color='green')


t(as.data.frame(coef(wifiMod1)))
as.data.frame(t(coef(wifiMod1)))
#SPline------------------------
#Splines to fit a smooth curve over a data. usually messy data that is non linear
data("diamonds")
diaspline1 <- smooth.spline(x=diamonds$carat, y=diamonds$price)
diaspline2 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=2)
diaspline3 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=10)
diaspline4 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=20)
diaspline5 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=50)
diaspline6 <- smooth.spline(x=diamonds$carat, y=diamonds$price, df=200)

get.spline.info <- function(object){data.frame(x=object$x, y=object$y, df=object$df)}

require(plyr)
#Use of ldply to run a simultaneous operations on the data
splineDF <- ldply(list(diaspline1,diaspline2,diaspline3,diaspline4,diaspline5,diaspline6),get.spline.info)
head(splineDF)


g <- ggplot(diamonds)+aes(x=carat, y=price)+ geom_point()
g+ geom_line(data=splineDF, aes(x=x,y=y, color=factor(round(df,0)), group=df))+scale_color_discrete('Degrees of\nFreedom')
#From the plot, more degrees of freedom makes the lines squidly of distorted. lesser degrees of freedom gives the line a 
#more smooth shape

#A natural cubic spline
require(splines)
head(ns(diamonds$carat, df=1)) 
head(ns(diamonds$carat, df=2))

g+ stat_smooth(method='lm',formula = y~ns(x,6), color = 'blue')
g+ stat_smooth(method='lm',formula = y~ns(x,3), color = 'red')
