#apply,sapply, lapply, mapply
#mapply - comparison of lists. multiple application of a function over lists simultaneously
#apply -matrix appply
#lapply - apply to a datastructure and returns a list
#sapply - apply to a data structure and returns a list


dmat <- matrix(1:9, nrow = 3)
dmat
apply(dmat, MARGIN = 1, mean)
apply(dmat, MARGIN=1, sum)
apply(dmat, MARGIN=2, sum)

colSums(dmat, na.rm=T)
rowSums(dmat, na.rm=T)

dlist <- list(A=matrix(1:9, nrow=3), B=matrix(7:15, ncol = 3), C=1:45, D=3)
sapply(dlist, sum) #returns a vector
lapply(dlist, mean) #return a list

#mapply
thelist <- list(A=matrix(1:16, nrow = 4), B=matrix(1:16,2), C=1:5)
secondlist <- list(A=matrix(1:16,4), B=matrix(1:16,8), C=15:1)

#check using mapply
mapply(identical, thelist, secondlist)

simp <- function(x,y){NROW(x)+NROW(y)}
mapply(simp,thelist,secondlist)
f=1:8

#Aggregation
data("diamonds")
head(diamonds) 
mean(diamonds$price)
aggregate(price~cut, diamonds, mean, na.rm =T)
aggregate(price~cut+color, diamonds, mean, na.rm =T)

aggregate(price~cut, diamonds, each(mean, median)) #each

#Use cbind() to apply a function on two variables
aggregate(cbind(price,carat)~cut,diamonds,mean)
aggregate(cbind(price,carat)~cut+color,diamonds,mean)



#The Plyr package
require(plyr)
data("baseball")
baseball
head(baseball)
tail(baseball)
str(baseball)
baseball$sf[which(baseball$year<1954)] <- 0 #1
baseball$sf[baseball$year<1954] <- 0 #2
any(is.na(baseball$sf))


baseball$hbp[is.na(baseball$hbp)] <- 0
baseball <- baseball[baseball$ab>49,]
#onbasepercentage(obp) = (bb+hbp+h/(sf+bb+ab+hbp+))
baseball$obp = with(baseball,(h+bb+hbp)/(sf+bb+ab+hbp))
#use with or within
head(baseball)

obp <- function(){obp = (h+bb+hbp)/(sf+bb+ab+hbp)
retrun (obp)}

obpcorr <- function(data){OBP = c(OBP=with(data,sum(h+hbp+bb)/sum(ab+hbp+bb+sf))); return (OBP)}

#ddply
careerOBP <- ddply(baseball,.variables = 'id', obpcorr)
head(careerOBP)

careerOBP <- careerOBP[order(careerOBP$OBP, decreasing = TRUE),]
head(careerOBP)

#The most popular of the plyr function is the ddply

#llply - lapply
#laply - sapply
#ldply
#dlply

colwise()
numcolwise(sum,na.rm=TRUE)(diamonds)
sapply(diamonds[,sapply(diamonds,is.numeric)], sum)


#Combine datasets - use cbind (used on vectors and matrix)

sport <- c('Hockey', 'Baseball', 'Football')
league <- c('NHL', 'MLB', 'NFL')
trophy <- c('Stanley Cup', "Comminsioner's Trophy", 'Trophy')
cbind(sport, league,trophy)


#for dataframes use rbind(used on dataframes, lists, vectors, matrix)

sport2 = data.frame(sport=c('Soccer', 'Tennis'), league=c('Laliga', 'IAAF'), trophy=c('Copa del rey', 'French Open'))
sport2
#Names of dataframes to be combine must match
sports <- rbind(sport2, cbind(sport, league, trophy))
sports


#Joining Dataset

codes <- read.table('C:/Users/user/Downloads/countryCodes.csv', sep=',', header=T, stringsAsFactors=T)
countries <- read.csv('C:/Users/user/Downloads/GovType.csv', sep = ',', header = T, stringsAsFactors=T)
View(codes)
View(countries)
countryMerged = merge(codes,countries, by.x ='Country.name', by.y = 'Country' )
View(countryMerged)
#Merge can be slow when used on a large dataset

names(codes[c('Country')]) <-  c(Country.name='Country')
head(codes)

require(plyr)
codes <- rename(codes, c(Country='Country.name'))
countryJoin = join(codes,countries, by='Country', type='full', match='all')


#Changing Data from Wide to Long (Much Similar to Pivoting and Unpivoting)
require(reshape2)
data("airquality")
head(airquality)
dim(airquality)

airMelt <- melt(airquality,id=c('Month', 'Day'), value.name='Value', variable.name = 'Metric')
head(airMelt)
dim(airMelt)

airCast <- dcast(airMelt, Month+Day~Metric, value.var = 'Value')
head(airCast)
