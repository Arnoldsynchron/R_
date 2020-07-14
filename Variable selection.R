#Use of variable selection to improve model. To curb the curse of dimensionality
#1 Regularization Method
require(useful); require(glmnet)
acs <- read.table('C:/Users/user/Downloads/acs_ny.csv', header = TRUE, sep = ',', stringsAsFactors=FALSE)
acs$Income <- with(acs,FamilyIncome>=150000)
head(acs)
#Multicollinearlity is not an issue

acsX <- build.x(Income~NumBedrooms+NumChildren+NumPeople+NumRooms+NumVehicles+NumWorkers+NumUnits+OwnRent+YearBuilt+ElectricBill+
                    FoodStamp+HeatingFuel+Insurance+Language-1,acs, contrasts = FALSE)

class(acsX)
dim(acsX)
topleft(acsX,6)
topright(acX,6)

acsY <- build.y(Income~NumBedrooms+NumChildren+NumPeople+NumRooms+NumVehicles+NumWorkers+NumUnits+OwnRent+YearBuilt+ElectricBill+
                    FoodStamp+HeatingFuel+Insurance+Language-1,acs)
head(acsY)
#glmnet - generalized linear method with elastic nets
set.seed(1863561)
ascCV1 <- cv.glmnet(x=acsX,y = acsY, family='binomial', nfolds = 5)
ascCV1$lambda.min #lamda that minimizes standard error
ascCV1$lambda.1se
plot(ascCV1)
coef(ascCV1, s='lambda.1se')
plot(ascCV1$glmnet.fit, xvar='lambda')
abline(v=log(c(ascCV1$lambda.min,ascCV1$lambda.1se)), lty=2)


#Using Ridge regression solely - where alpha equals zero
ascCV2 <- cv.glmnet(acsX,acsY,nfolds = 5,family='binomial',alpha=0)
ascCV2$lambda.min
ascCV2$lambda.1se
plot(ascCV2)
coef(ascCV2, s='lambda.1se')
plot(ascCV2$glmnet.fit, xvar='lambda')
abline(v=log(c(ascCV2$lambda.min,ascCV2$lambda.1se)), lty=2)

save(acs,file = 'acs.rdata')


#2 Bayesian Shrinkage

load('C:/Users/user/Downloads/ideo.Rdata')
head(ideo)
theYears <- unique(ideo$Year)
Results <- vector(mode='list', length=length(theYears))
names(Results) <- theYears
Results

for (i in theYears)
    {
    Results[[as.character(i)]] <- glm(Vote~Gender+Race+Education+Income, data=ideo, 
                                      family=binomial(link='logit'), subset = Year==i)
    }
head(Results)

require(coefplot)
voteinfo <- multiplot(Results, coefficients = 'Raceblack',plot=FALSE)
head(voteinfo)
multiplot(Results, coefficients = 'Raceblack',secret.weapon = TRUE)
multiplot(Results, coefficients = 'Raceblack',secret.weapon =TRUE) + coord_flip(xlim=c(-20,10)) #Zooimng into the plot

resultsb <- vector(mode='list', length=length(theYears))
names(resultsb) <- unique(theYears)
resultsb

#prior.scale of 2.5 and prior.df is reported to give a good fit, according to andy

#Bayesian technique is used to get an out of control variable, under control.
for (i in theYears){
    resultsb[[as.character(i)]] <- arm::bayesglm(Vote~Gender+Race+Education+Income, data=ideo[ideo$Year==i,],
                                                 family=binomial(link='logit'), prior.scale = 2.5, prior.df = 1)
}

multiplot(resultsb, coefficients = 'Raceblack', secret.weapon = TRUE)
