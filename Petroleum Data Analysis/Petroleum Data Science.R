require(tidyverse); require(ggplot2)

file <- file.choose()
prod_data <- read_excel(file)
xstate <- c('TX','KS','CA')

prod_data_xstate=prod_data %>% filter(state %in% xstate)
#creates a simple frequency table
table(prod_data_xstate$state)

sum(table(prod_data_xstate$state))

ggplot() +theme(axis.text.x = element_text(size = 10))

prod_tx_2004 <- prod_data %>% filter(state=='TX', prod_year==2004)
ggplot(prod_tx_2004, aes(oil_prod_BBL, rate_class, color=rate_class)) + geom_step() 
ggplot(prod_tx_2004, aes(oil_prod_BBL, rate_class, color=rate_class)) + geom_point()+geom_line(linetype = 'dashed', color='red') 
ggplot(prod_tx_2004, aes(oil_prod_BBL, rate_class, color=rate_class)) + geom_path()

#bar
g <- ggplot(prod_data, aes(color=state, fill=state))
g+geom_bar(aes(x=state))

g <- ggplot(prod_data, aes(color=prod_year, fill=prod_year))
g+geom_bar(aes(x=prod_year))

g <- ggplot(prod_data, aes(color=state, fill=state))
g+geom_bar(aes(x=prod_year))

#histogram and density plot
g <- ggplot(prod_data, aes(color=state, fill=state))
g+geom_histogram(aes(x=prod_year), bins =5)
g+geom_histogram(aes(x=prod_year), bins =20, color='green')

ggplot(prod_data, aes(x=prod_year))+geom_histogram(aes(y=..density..), bins =20, fill='cyan')+ geom_density(alpha=0.2, fill='black')+
    theme(legend.position = 'top')

#---
