require(dplyr)
require(readxl)
require(ggplot2)

file <- file.choose()
prod_data <- read_excel(file)
prod_data

#filter  production data where year is 2004
prod_data  %>% filter(prod_year==2004)

#Q1 Extract the dataset where Year is 2004 and state is AK----
Akdata <- prod_data %>% filter(prod_year==2004&state=='AK')
View(Akdata)

#Q2 Extract the dataset where Year is between 2004 and 2006 and state is FL----
FLdata <- prod_data %>% filter(between(prod_year, 2004,2006)& state == 'FL')
View(FLdata)

#Arrange data in an order (ascending or descending order)
Akdata %>% arrange(desc(oil_prod_BBL))
arrange(prod_data,oil_prod_BBL)


#Filter and arrange on the same line

Oil_2004_2006_AK <- prod_data %>% filter(state =='AK'&prod_year<=2006 & prod_year>=2004) %>% arrange(desc(oil_prod_BBL))

#Use mutate to create a nw variable
  
#Q3 Average oil produced by each well per state----
Prod_data <- prod_data %>% mutate(Average_oil_produced = oil_prod_BBL/num_oil_wells)
str(Prod_data)

#Q4 Average oil produced from each well in year 2004 and the position of texas----
q4 <- prod_data %>% mutate(Average_oil_produced=oil_prod_BBL/num_oil_wells)%>% filter(prod_year==2004) %>% arrange(desc(Average_oil_produced))
View(q4) 

#Q5 Average gas produced from each well in rate_class 20 and position of TX when arranged----
q5 <- prod_data %>% mutate(Average_oil_produced = oil_prod_BBL/num_oil_wells) %>% 
    filter(rate_class==20) %>% arrange(desc(Average_oil_produced))
View(q5)
q5[q5$state == 'TX',][1,]

#Quiz----
file1 <- file.choose()
consumption_data <- read_excel(file1, sheet='Consumption_Total')
production_data <- read_excel(file1, sheet='Production_Total')
View(consumption_data)
View(production_data)

#define a function
melt <- function(dataframe,...){
    result <- dataframe %>% gather(2:(ncol(dataframe)), key='year', value='volume')
    result$year <- as.numeric(result$year)
    return (result)
}

consumption_data <- melt(consumption_data)
production_data <- melt(production_data) 
#Join
Combined <- inner_join(consumption_data, production_data, by=c('year','Place'), suffix=c('_consumed','_produced'))
View(Combined)
head(Combined)
#mutating the list to create a new variable
Combined <- Combined %>% mutate(prod_cons_ratio = volume_produced/volume_consumed) %>% arrange(desc(prod_cons_ratio))

arrange(Combined,prod_cons_ratio)

#Prod_cons_ration for the year 2015 in Middle east
Combined %>% filter (Place=='Middle East'&year==2015)
#choose either to get your results
Combined[which(Combined$Place == 'Middle East'& Combined$year==2015),]


Combined %>% group_by(Place) %>% dplyr::summarize(mean_prod_consump =mean(prod_cons_ratio)) %>% arrange(mean_prod_consump)
                                                   