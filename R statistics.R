#Bell curve or Gaussian distribution or Normal distribution

set.seed(0)
rnorm(10)
rnorm(10, 100, 20)
randNorm10 <- c(rnorm(10,100,20))
randNorm10

#Finding the probability of each random number occuring (Inverse of norm distribution)
dnorm(randNorm10, 100,20)

# visualizing a normal distribution 
r30k <- rnorm(30000)
N30k <- dnorm(r30k)
require(ggplot2)
require(ggthemes)
head(r30k, 4)

#g <- ggplot(data.frame(r30k), aes(r30k, color = 'magenta')) +geom_histogram()
#g
#rlang::last_error(); rlang::last_trace()

g <- ggplot(data.frame(x=r30k, y=N30k))+ aes(x,y, color = 'magenta') +geom_point()+ labs()
g
