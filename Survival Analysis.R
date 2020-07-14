#Survival Analysis requires dealing with sensored data. Hospital data.

require(survival)
head(bladder)
?bladder

bladder[100:105,]

survObject <- with(bladder[100:105,], Surv(stop,event))
survObject
survObject[,1:2]

cox1 <- coxph(Surv(stop,event)~rx+number+size+enum,bladder)
summary(cox1)
plot(survfit(cox1), xlab='Days',ylab='Survival Rate', conf.int = TRUE)


cox2 <- coxph(Surv(stop,event)~strata(rx)+number+size+enum,bladder)
summary(cox2)
plot(survfit(cox2), xlab='Days',ylab='Survival Rate', conf.int = TRUE, col=1:2)
legend('bottomleft', legend = c(1,2), lty=1, col=1:2, text.col=1:2, title='rx')

cox.zph(cox1)
cox.zph(cox2)

head(bladder2)
