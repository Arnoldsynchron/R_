data(package= .packages(all.available = TRUE))
head(father.son)

ggplot(father.son, aes(fheight, sheight, color = 'Magenta')) + geom_point(aes(group=1)) + geom_smooth(method='lm')+
    labs(x="Fathers' height", y="Sons' height")
# y = mx +b

#fitting a model
heightslm = lm(sheight~fheight,father.son)
heightslm
summary(heightslm)

#ANOVA - Analysis of Variance
library(reshape2)
data(tips)
head(tips)

#Comparing tips by day. using ANOVA, -1 refers to not using an intercept
tipsAnova <- aov(tip~day-1,tips)

#comparing tips by day using LM, -1 refers to not using an intercept
tipsLM <- lm(tip~day-1,tips)

#---------------------------------------------------
summary(tipsAnova)
summary(tipsLM)

#Multiple regression: A natural extension of linear regression; 
#
#
#EDA- Most essential
housing <- read.table('C:/Users/user/Downloads/Housing.csv', header = TRUE, sep = ',', stringsAsFactors=FALSE)
head(housing)
str(housing)
colnames(housing) <- c('Neighborhood','Class','Units', 'YearBuilt','SqFt','Income','IncomePerSqFt','Expense',
                       'ExpensePerSqFt','NetIncome','Value', 'ValueperSqFt','Boro')
colnames(housing)
#1
ggplot(housing)+ aes(x=ValueperSqFt,color=Boro)+geom_histogram()+labs(x='Value per SquareFoot',y='Frequency')+ggtitle('Histogram plot')

ggplot(housing)+ aes(x=ValueperSqFt,fill=Boro)+geom_histogram()+labs(x='Value per SquareFoot',y='Frequency')+ggtitle('Histogram plot')

#Small Multiples
ggplot(housing)+ aes(x=ValueperSqFt,color=Boro, fill=Boro)+geom_histogram()+
    labs(x='Value per SquareFoot',y='Frequency')+ggtitle('Histogram plot') +facet_wrap(~Boro)

#2
ggplot(housing)+ aes(x=SqFt,color=Boro)+geom_histogram(bins=10)+
    labs(x='SquareFoot',y='Frequency')+ggtitle('Histogram plot')#First histogram plot


ggplot(housing)+ aes(x=SqFt,y=ValueperSqFt,color=Boro)+geom_point()+
    labs(x='SquareFoot',y='ValueperSqFt')+ggtitle('Histogram plot') #Scatter plot, to identify presence of outliers


h <- housing[!housing$SqFt %in% boxplot.stats(housing$SqFt)$out,]
ggplot(h)+ aes(x=SqFt,fill=Boro)+geom_histogram()+
    labs(x='SquareFoot')+ggtitle('Histogram plot') #Outliers are not plotted in this dataset

ggplot(housing[housing$Units<1000,])+ aes(x=SqFt,y=ValueperSqFt,color=Boro)+geom_point()+
    labs(x='SquareFoot',y='ValueperSqFt')+ggtitle('Histogram plot') #Scatter plot where house units less than 1000 are removed

sum(housing$Units>=1000) #checks for number is less than 1000 units and counts


#3 Fitting Multiple Regression
str(housing)
house1 <- lm(ValueperSqFt~Units+SqFt+Boro, housing)
summary(house1)
#Returning coefficients
house1$coefficients
coef(house1)
coefficients(house1)
#Plotting coefficient plots
require(coefplot)
coefplot(house1)
#Coefficients away from zero are significant

#check the model matrix parameters
head(model.matrix(ValueperSqFt~Boro, housing))

house2 <- lm(ValueperSqFt~Units*SqFt+Boro, housing) #The multiplication sign accounts and represents interaction
summary(house2)
coefplot(house2)
#CHCK the unit*SqFt variable
head(model.matrix(ValueperSqFt~Units*SqFt, housing))
head(model.matrix(ValueperSqFt~Units:SqFt, housing)) #Shows interaction and excludes individual variables

house3 <- lm(ValueperSqFt~Units:SqFt+Boro, housing)
coefplot(house3)

house4 <- lm(ValueperSqFt~SqFt*Units*Income, housing)
house4$coefficients
coefplot(house4)

house5 <- lm(ValueperSqFt~Class*Boro, housing)
house5$coefficients
coefplot(house5)

house6 <- lm(ValueperSqFt~I(SqFt/Units)+Boro,housing) #The I function or I(), wraps the equation around
                        #and creates a new variable off the eqation or calculation
house6$coefficients

house7 <- lm(ValueperSqFt~(Units+SqFt)^2, housing) #Similar to using the multiplication sign in house8
house7$coefficients
house8<- lm(ValueperSqFt~Units*SqFt, housing)
house8$coefficients

house9 <- lm(ValueperSqFt~I(Units+SqFt)^2, housing) #remember to use I(), in order to create a new variable
house9$coefficients

#To compare multiple models, using the coefplots. Multiple coeffplots

multiplot(house1,house2,house3)
