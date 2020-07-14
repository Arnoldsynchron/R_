#Tidying data using tidyr
require(tidyr)
require(readxl)
require(reshape2)
file <- file.choose()
datasheet1 <- read_excel(file, col_names = TRUE, sheet = 1)
datasheet2 <- read_excel(file, sheet = 2)
datasheet1 <- datasheet1[-8]
 View(datasheet1) 
View(datasheet2, 'Data2')

#Using gather to change dataframe from wide to long - could be done too using reshape2
datasheet1 <- datasheet1 %>% gather(2:7, key='Year', value = 'Number of Oil Wells')
datasheet1$`Number of Oil Wells` <-   as.numeric(datasheet1$`Number of Oil Wells`)

datasheet2 <- gather(datasheet2,2:7, key='Year', value = 'Number of Oil Wells')
datasheet2 

#To change from wide to long data format use the 'Spread command'
datasheet1 <- spread(datasheet1,'Year','Number of Oil Wells')
datasheet1

#reshaping the data using the reshape package
library(reshape2)

dataset1 <- melt(datasheet1, id ='State', variable.name = 'Year', value.name='Number of Wells')
dataset1
#reshape 
dataset1 <- dcast(dataset1, State~Year )
dataset1

#Reshaping another data 
file <- file.choose()
prod_reshape2 <- read_excel(file, sheet = 2)
prod_reshape1 <- read_excel(file, sheet = 1)
View(prod_reshape2)

prod_reshape <- prod_reshape2 %>% melt( id=c('State','Class'), variable.name='Year', value.name = 'Number of Wells', na.rm = TRUE)
prod_reshape$`Number of Wells`<- as.numeric(prod_reshape$`Number of Wells`); head(prod_reshape)

dcast(prod_reshape, State~Year,sum, value.var = 'Number of Wells')


dcast(prod_reshape, State~Class,sum, value.var = 'Number of Wells')

dcast(prod_reshape, State+Class~Year,)


x <- dcast(prod_reshape, State~Class+Year, sum)
x
melt(x,id='State', variable.name = 'Class_Year', value.name = 'Wells', na.rm = TRUE)