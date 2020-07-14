#Poisson regression is used to model count data. Number of misprints, children, accidents.

ggplot(acs, aes(NumChildren))+ geom_histogram(binwidth = 2)
str(acs)

children1 <- glm(NumChildren~FamilyIncome+FamilyType+OwnRent,acs, family =poisson(link='log') )
summary(children1)
children1$coefficients
coefplot(children1)

#Check for over dispersion of the poisson data. 
# for poisson distribution, the fitted values are taken to be the mean
z <- (acs$NumChildren-children1$fitted.values)/sqrt(children1$fitted.values)
sum(z^2)/children1$df.residual
#children1$df.residual - the degree of freedom

#Using the chisquare - pvalue
pchisq(sum(z^2), df=children1$df.residual) #the p-value is 1, which reveals and indicates over dispersion of the data

children2 <- glm(NumChildren~FamilyIncome+FamilyType+OwnRent,acs, family =quasipoisson(link='log') )
multiplot(children1,children2)

