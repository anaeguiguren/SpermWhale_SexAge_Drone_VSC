#bootstraping:
data(iris)


library(boot)

# estimate the median and CIS for sepal.length, width, and spearman's correlation between the two
# 1. make a function with the data and indices (row numbers)


foo <- function(data, indices, cor.type){
  dt <- data[indices,]#subset data
  
  c(
    cor(dt[,1], dt[,2], method = cor.type),
    median(dt[,1]),
    median(dt[,2])
  )
}


#apply boot function:

set.seed(1991)
myBoot <-boot(iris, foo, R = 1000, cor.type = 's')
myBoot$t0 # observed values for full dataset


head(myBoot$t) #values obtained in each bootstrap


colMeans(myBoot$t)

## [1]  0.002546391 -0.013350000  0.007900000

plot(myBoot, index = 2)
boot.ci(myBoot, index = 1)#basic ci. 
boot.ci(myBoot, index = 2)#basic ci. 



true.altitude <- function(true.length, pixel.length, image.width){
  alpha = ifelse(image.width == 3840, yes = 0.000328, no = 
                   ifelse(image.width == 1920, yes = 0.000656, no = NA))
  t.a = (true.length/(pixel.length* alpha))
  return(t.a)
}





foo <- function(data, indices){
  dt <- data[indices,]#subset data
  m<-gls(true.altitude ~ altitude.fix,, data = dt, method = "ML", verbose = F)
  
  c(
    unname(coef(m)[1]),#intercept
    unname(coef(m)[2]),# slope
    sd(dt$true.altitude - dt$altitude.fix) #error
  )
}


myBoot <-boot(dat, foo, R = 1000)
myBoot$t0
colMeans(myBoot$t)
