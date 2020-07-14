require(ggplot2)
data("diamonds", stringAsFactor = F)
head(diamonds);tail(diamonds,5)
summary(diamonds)
cut= diamonds[['cut']]
cut = as.character(cut)

#Histograms
#Data to be plotted must be numeric
hist(diamonds[[8]], xlab ='Price Distribution')
hist(diamonds$carat, main='Carat Histogram')

#Barchart
dat = diamonds[c('color','price')]
names(diamonds[['price']]) <- diamonds[['color']]
barplot(diamonds[['price']], width = 1, space = 0.1, legend.text = T, horiz = F,)

#Boxplot
boxplot(diamonds[,1])
boxplot(diamonds$carat)

#Scatter plots
plot(diamonds[[1]],diamonds[[5]])

plot(diamonds$carat,diamonds$price, main= 'Relationship between Price of Diamond to Carats', xlab = 'carats', ylab='price')
#Use of formular to plot a scatter type of plot
plot(price ~ carat, data =diamonds)


#Using the ggplot package for plotting graphics in R
library(ggplot2)
ggplot2::aes(diamonds$carat, diamonds$price)
ggplot(data=diamonds)
geom_histogram()
geom_point()
geom_line()

#Plotting a scatter plot
ggplot(data = diamonds) + geom_point(aes(diamonds$carat,diamonds$price))
gg <- ggplot(data = diamonds, aes(carat, price))
gg + geom_point(aes(color=color, shape=cut))

#Plotting a histogram
ggplot(diamonds) + geom_histogram(aes(carat), binwidth = 0.6)

#Visualizing data using a density plot
ggplot(diamonds) + geom_density(aes(carat), fill= 'grey50',color = 'orange3')

#Plotting a violin plot 
ggplot(data = diamonds) + geom_violin(aes(x,y))
ggplot(data = diamonds) + geom_violin(aes(diamonds$carat,diamonds$price))

vp <- ggplot(diamonds, aes(carat, cut)) 
vp + geom_violin(aes(y=cut), color = 'cyan') +geom_point() #1
vp + geom_point()+geom_violin(color = 'cyan') #2
vp+ geom_jitter() + geom_violin()

#Plotting a line graph
ggplot(diamonds) + geom_line(aes(carat,price))

#Plotting a box plot
ggplot(diamonds)+ geom_boxplot (aes(carat))

ggplot(diamonds, aes(carat, fill='magenta')) +geom_boxplot(aes(y=cut))


#Line plot using the economics data available
data("economics")
str(economics)
head(economics,5)

#Visualizing population over time

ggplot(economics, aes(date,pop)) + geom_line(color='magenta')

require(lubridate)
economics$year = year(economics$date)
economics$month = month(economics$date)
head(economics)

#creating a new data set with dates from 2000 till present
date=as.Date('1999/12/31')
econ2000 = data.frame(economics[economics$date> date,])#1
econ2000 = economics[economics$date> date,] #2 - Multiple column return
econ2000 = economics[which(economics$year>=2000),] #3
head(econ200)
econ200$month = month(econ200$month, label = T)


require(scales)
gl <- ggplot(econ200, aes(month, pop))
gl <- gl+ geom_line(aes(color= factor(year), group = year))
gl <- gl+ scale_color_discrete(name='Year') #Color scaling
gl <- gl + scale_y_continuous(labels=comma) #Formatting the thousand seperator to be a comma (y axis)
gl <- gl+ labs(title = 'Population Growth', x ='Month', y='Month') #Labels of the plot
gl <- gl+ theme(axis.text.x=element_text(angle=90, hjust = 1)) #Adjusting its theme
gl

#Creating Multiple (Edward - Modern guru of visualization)
mp <- ggplot(diamonds, aes(carat, price))
mp+ geom_point(aes(color=color))+facet_wrap(~color) #wraps data
mp + geom_point(aes(color=color)) + facet_grid(cut~clarity)

ggplot(diamonds,aes(carat)) + geom_histogram()+ facet_wrap(~clarity)
head(diamonds)

#Aesthetic mappings
ggplot(diamonds,aes(carat,price, color=color, shape=cut)) + geom_point() + facet_wrap(~clarity)

#Adding themes to the graph
require(ggthemes)

gt <- ggplot(diamonds, aes(carat, price, color = color)) + geom_point()
gt
gt + ggthemes::theme_wsj()
gt + ggthemes::theme_excel_new()
gt + ggthemes::theme_gdocs()
gt + theme_economist_white() + scale_colour_economist()
gt + theme_tufte()
