data(iris)
library(rstan)
"iris"
head(iris)
library(tidyverse)

versicolor = iris %>% filter(Species == "versicolor")
x = versicolor$Sepal.Length

y =  versicolor$Petal.Length

data = list(N = length(x), x = x, y = y)

fit = stan(file = "C:/Users/balae/Documents/model.stan", data = data)
summary(fit)
plot(x,y)

params = extract(fit)

alpha = mean(params$alpha) 
beta = mean(params$beta)

abline(alpha, beta)


xr = seq(4,7.5, 0.1) #xrange  
yCI = sapply(xr, function(k) quantile(params$beta * k +params$alpha, probs = c(0.05, 0.95) )) 
lines(xr, yCI[1,], col = "red")
lines(xr, yCI[2,], col = "red")


# posterior checks with simulated data given our posterior:

fit = stan(file = "C:/Users/balae/Documents/model.stan", data = data)
params = extract(fit)

plot(density(y))



for(i in 1:10){
  lines(density(params$y_sim[i, ]), col = "red")
  
}

#can we recover same parameters from simmulated data

y_new = params$y_sim[20,]


#hierarchical models----






 