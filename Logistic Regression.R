acs <- read.table('C:/Users/user/Downloads/acs_ny.csv', header = TRUE, sep = ',', stringsAsFactors=FALSE)
head(acs)
acs$Income <- with(acs,FamilyIncome>=150000) #acs$FamilyIncome>=150000
View(acs)

require(ggplot2)
require(useful)
ggplot(acs) +aes(x=FamilyIncome) + geom_density(fill='grey', color='grey') + geom_vline(xintercept = 150000)+
    scale_x_continuous(label=multiple.dollar, limits=c(0,1000000))

#Fitting a logistic regression model
#Binomial function is used to model a yes or no occurence. It is used for a 0 or 1 data modelling.

income1 <- glm(Income ~ NumBedrooms+NumRooms+NumUnits+OwnRent+NumPeople, acs, family=binomial(link='logit'))


income1$coefficients
coefplot(income1)

invcoeflogit <- function(x){y <- 1/(1+exp(-x))
return(y)}
invcoeflogit(income1$coefficients) 
