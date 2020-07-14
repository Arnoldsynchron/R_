#Quiz ggplot2----
require(readxl)
file <- file.choose()
prod_data <- read_excel(file, sheet=4)
head(prod_data)
require(reshape2)
prod_data <- melt(prod_data, id="Place", variable.name = 'year', value.name = 'production')
View(prod_data)

#plotting using ggplot2
require(ggplot2)
ggplot(prod_data, aes(year,production)) + geom_point() #1
ggplot(prod_data, aes(Place,production, color=year)) + geom_point() #2
ggplot(prod_data, aes(year,production, color=year)) + geom_point() #3

require(dplyr)
prod_data_NA <- prod_data %>% filter(Place=='North America')
head(prod_data_NA)
ggplot(prod_data_NA, aes(year,production, color=year)) +geom_point() #4
savePlot('Rplot03.png')

con_data <- read_excel(file, sheet=3)
con_data <- melt(con_data, is='Place', variable.name='year', value.name='consumption')
prod_con_data <- inner_join(prod_data, con_data, by=c('year','Place'), suffix=c('_prod','_con'))
prod_con_data <- prod_con_data %>% mutate(avg_con_prod=production/value)
ggplot(prod_con_data, aes(year, avg_con_prod, color=Place))+geom_point() #5

prod_con_data_NA <- filter(prod_con_data, Place=='North America')
ggplot(prod_con_data_NA, aes(year,avg_con_prod))+geom_point(aes(color=Place)) #6

#plotting a graph of both production and consumption overlapping each other
ggplot(prod_con_data)+geom_point(aes(year, production, color='Production')) +geom_point(aes(year, value, color='Consumption')) #7

ggplot(prod_con_data_NA)+geom_point(aes(year, production, color='Production')) +geom_point(aes(year, value, color='Consumption')) #8

ggplot(prod_con_data)+geom_point(aes(year, production, color='Production')) +
    geom_point(aes(year, value, color='Consumption')) +facet_wrap(vars(Place)) #9

ggplot(prod_con_data)+geom_point(aes(year, production, color='Production')) +
    geom_point(aes(year, value, color='Consumption')) +facet_grid(vars(Place)) #9
