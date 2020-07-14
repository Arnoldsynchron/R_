#Generalized Additive Models (GAM) -ways of modelling data that are non linear
load('C:/Users/user/Downloads/credit.rdata')
head(credit)
require(useful)

ggplot(credit, aes(x=CreditAmount, y=Credit)) + geom_jitter(position=position_jitter(height=.2))+
    facet_grid(CreditHistory~Employment)+xlab('Credit Amount')+theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))+
    scale_x_continuous(labels = multiple)
require(mgcv)
creditGam <- gam(Credit~te(CreditAmount)+s(Age)+CreditHistory+Employment,credit,family=binomial(link='logit'))

summary(creditGam)
plot(creditGam, select=1,se=TRUE,shade=TRUE)
plot(creditGam, select=2,se=TRUE,shade=TRUE)

te()
s()