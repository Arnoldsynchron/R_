#Package for decision tree and RandomForest
require(rpart)
head(credit)
creditTree <- rpart(Credit~CreditAmount+Age+CreditHistory+Employment, credit)
summary(creditTree)
require(rpart.plot)
rpart.plot(creditTree,extra = 4)

#Random forests fit decisons trees, and makes a good fit of it
require(randomForest)
creditformula <- Credit~CreditAmount+Age+CreditHistory+Employment+Purpose+Duration
creditX <- build.x(creditformula,credit)
creditY <- build.y(creditformula,credit)
creditForest <- randomForest(creditX,creditY)
summary(creditForest)
creditForest
