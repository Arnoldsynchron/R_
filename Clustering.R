#Unsupervised learning - clustering
url <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/wine/'
wine <- read.table('C:/Users/user/Downloads/wine.data', header = FALSE, stringsAsFactors=FALSE, sep = ',')
names(wine) <- c('Cultivar','Alcohol','Malic.Ash','Ash','Alkalinity.of.ash','Magnesium','Total.phenols',
                 'Flavanoids','Nonflavanoid.phenols', 'Proanthocyanins', 'Color.intensity','Hue','OD','Proline')
head(wine)
set.seed(278613)
winetrain <- wine[,which(names(wine)!= 'Cultivar')] #removes the cultivar column
winetrain
winek3 <- kmeans(x=winetrain, centers = 3)
winek3

require(useful)
plot(winek3, winetrain) #Similar to Principal Component Analysis

set.seed(278613)
winek3r25 <- kmeans(winetrain, centers = 3, nstart = 25)
winek3$size

winek3r25
####Determine the number of cluster----
#Hartigan's rule
wineBest <- FitKMeans(winetrain, max.clusters = 20, nstart = 25, seed=278613)
PlotHartigan(wineBest)

#comparing - using confusion matrix
t <- table(wine$Cultivar,winek3r25$cluster)
plot(t)

#GAP statistics
require(cluster)
theGAP <- clusGAP(winetrain)