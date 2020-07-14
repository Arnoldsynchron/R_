#Boostrap method is used to determine uncertainty for when there is no analytic solution
require(plyr)
data("baseball")
head(baseball)
baseball <- baseball[baseball$year>1990,]
head(baseball)

bat.avg <- function(data, indices=1:NROW(data), hits='h', at.bats='ab'){
    sum(data[indices,hits], na.rm = TRUE)/(sum(data[indices,at.bats], na.rm=TRUE))}
bat.avg(baseball)

require(boot)
avgBoot <- boot(baseball, statistic = bat.avg, R=1200, stype='i')
avgBoot

#Confidence Interval
boot.ci(avgBoot, cong=0.95,type='norm')

ggplot()+geom_histogram(aes(x=avgBoot$t), fill='grey',color='grey')+
    geom_vline(xintercept = avgBoot$t0 + c(-1,1)*2*sqrt(var(avgBoot$t)), linetype=2)
