'''
title: Imputation in R
author: Arnold
date: June-01-2020
output: html-document

Also bring in the profduction data from the shared repository 
'''
file <- file.choose()
require(readxl)
prod_data <- read_excel(file)
head(prod_data)
summary(prod_data)

#check for number of N.A's
colSums(is.na(prod_data))

dim(prod)[1]

or use

dplyr::summarise_all
rowSums(is.na(prod_data))

'mean_na. if there is upto 50% na values, therefore the column would be deleted. else 
    feature engineer on the column'

rowMeans()  

#imputation using R libraries

install.packages(c('Hmisc','mice','Amelia','imputeR','mi','missForest'))
require(mice)
md.patter n()