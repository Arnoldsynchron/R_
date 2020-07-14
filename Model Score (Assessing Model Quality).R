housing <- read.table('C:/Users/user/Downloads/Housing.csv', header = TRUE, sep = ',', stringsAsFactors=FALSE)
head(housing)
str(housing)
colnames(housing) <- c('Neighborhood','Class','Units', 'YearBuilt','SqFt','Income','IncomePerSqFt','Expense',
                       'ExpensePerSqFt','NetIncome','Value', 'ValueperSqFt','Boro')
colnames(housing)

#Assesing model peformance using, the residuals

house1 <- lm(ValueperSqFt~Units+SqFt+Boro, housing)
coefplot(house1)
head(fortify(house1))

h1 <- ggplot(aes(x=.fitted,y=.resid), data=house1)+geom_point()+ geom_hline(yintercept = 0)+geom_smooth(se=FALSE)
h1
h1+geom_point(aes(color=Boro))

#Use (q-q plot)quantile plot, to determine model performance
plot(house1, which = 2)

ggplot(house1, aes(sample= .stdresid))+stat_qq()+geom_abline()
ggplot(house1, aes(x=.resid)) + geom_histogram()

#Compare Models --------------------------------------------------
house1 <- lm(ValueperSqFt~Units+SqFt+Boro, housing)

house2 <- lm(ValueperSqFt~Units*SqFt+Boro, housing)

house3 <- lm(ValueperSqFt~Units+SqFt*Boro+Class, housing)

house4 <- lm(ValueperSqFt~Units+SqFt*Boro+SqFt*Class, housing)

house5 <- lm(ValueperSqFt~Boro+Class, housing)

multiplot(house1,house2, house3,house4,house5, pointSize = 2)

#ANOVA for comaparing models, and AIC with BIC, AIC and BIC are to account for complex models

anova(house1, house2, house3,house4,house5) #RSS - Residual Sum of Squares

AIC(house1, house2, house3,house4,house5)
BIC(house1, house2, house3,house4,house5)

#-------(To run a linear regression using the glm(generalized linear model), set family = gaussian)
housing$highvalue <- with(housing,ValueperSqFt>=150)

high1 <- glm(highvalue~Units+SqFt+Boro, housing, family = binomial(link='logit'))

high2 <- glm(highvalue~Units*SqFt+Boro, housing, family = binomial(link='logit'))

high3 <- glm(highvalue~Units+SqFt*Boro+Class, housing, family = binomial(link='logit'))

high4 <- glm(highvalue~Units+SqFt*Boro+SqFt*Class, housing, family = binomial(link='logit'))

high5 <- glm(highvalue~Boro+Class, housing, family = binomial(link='logit'))

#anova of each models 
#RuleofThumb: for any additional coefficient, the deviance should drop by at least 2
anova(high1,high2,high3,high4,high5)

AIC(high1,high2,high3,high4,high5)
BIC(high1,high2,high3,high4,high5)
#--anova and aic gives similar results, different from bic

head(model.matrix(high4))




