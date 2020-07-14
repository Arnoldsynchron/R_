#K-Fold Cross Validation (a 10 K cross fold validation)
require(boot) #using the K-Fold cross validation for a linear model requires  the (glm) function
head(housing)
house1 <- glm(ValueperSqFt~Units+SqFt+Boro, housing, family = gaussian(link='identity'))
houseCVq <- cv.glm(housing,house1,K=5)
houseCVq$delta

house2 <- glm(ValueperSqFt~Units*SqFt+Boro, housing, family = gaussian(link='identity'))
house3 <- glm(ValueperSqFt~Units+SqFt*Boro+Class, housing,family = gaussian('identity'))
house4 <- glm(ValueperSqFt~Units+SqFt*Boro+SqFt*Class, housing, family=gaussian('identity'))
house5 <- glm(ValueperSqFt~Boro+Class, housing, family = gaussian('identity'))

#Cross Validations
housecv2 <- cv.glm(housing,house2, K=5)
housecv2$delta
housecv3 <- cv.glm(housing,house3, K=5)
housecv3$delta
housecv4 <- cv.glm(housing,house4, K=5)
housecv4$delta
housecv5 <- cv.glm(housing,house5, K=5)
housecv5$delta

cvResults <- as.data.frame(rbind(houseCVq$delta,housecv2$delta,housecv3$delta,housecv4$delta,housecv5$delta))
colnames(cvResults) <- c('Error','Adjusted.Error')
rownames(cvResults) <- c('Model.1','Model.2','Model.3','Model.4','Model.5')

#ANOVA, AIC and BIC for the models
cvanova <- anova(house1,house2,house3,house4,house5)
cvaic <- AIC(house1,house2,house3,house4,house5)
cvbic <- BIC(house1,house2,house3,house4,house5)
cvResults <- cbind(cvResults,Anova = cvanova$`Resid. Dev`,AIC=cvaic$AIC,BIC=cvbic$BIC) # fo spaced named variables use ' ' to enclosed them e.g housing$'f jk'
cvResults
