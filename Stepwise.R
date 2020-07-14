#Using Stepwise method to evaluate or choose a better model
head(housing)
nullmodel <- lm(ValueperSqFt~1, data=housing)
fullmodel <- lm(ValueperSqFt~Units*SqFt+Boro*Class, housing)

#Using stepwise to choose a better model
housestep <- step(nullmodel, scope=list(upper=fullmodel, lower=nullmodel),direction ='both')
housestep$rank
housestep
