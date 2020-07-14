n()
data("mtcars")
head(mtcars)

mtcyl <- group_by(mtcars,cyl)
mtcyl <- mtcars %>% group_by(cyl)


#Groupby doesn't change the shape of the dataframe but actually affects the way
#the tibble/dataframe applied to it interacts with other dplyr functions

mtcyl %>% summarise(
    disp=mean(disp),
    hp=mean(hp)
    )
mtcyl %>% filter(disp==max(disp))
mtcars %>% summarise(disp=mean(disp), hp=mean(hp))
mtcars %>% filter(disp == max(disp))

#groupby two categories  -  groupby doesn't affect the shape
#The n() displays and error - n() to be used only in a data context
#To bypass thos problem, the name of the packages should be specified to each function

mtamvs <- mtcars %>% dplyr::group_by(vs,am)
mtamvs
mtamvs %>% filter(disp==max(disp))
mtamvs %>% summarise(meandisp=mean(disp), meandrat= mean(drat))

mt_vs <- mtamvs %>% dplyr::summarise(disp = mean(disp),n = dplyr::n())
mt_vs
sum(mt_vs$n)
mt_vs %>% summarise(n=sum(n))
#using group_by overides default grouping in the data
mtcyl %>% group_by(vs,am) %>% group_vars()

#To ungroup a tibble or dataframe
mt_vs %>% ungroup()