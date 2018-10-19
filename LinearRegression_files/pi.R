
library(plotrix)
library(mosaic)
library(pander)
library(DT)

points <- c(1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000, 9000, 10000, 11000, 12000, 13000, 14000, 15000, 16000, 17000, 18000, 19000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000)
comp.time <- c(.47999, .37663, .40267, .40622, .45690, .47228, .53520, .53379, .57000, .58171, .1488, .1417, .1784, .1611, .1871, .2145, .2431, .2663, .2907, .4133, .3504, .3721, .3974, .4311, .4636, .4947, .5037, .5335, .5620, .5838, .8088, 1.0570, 1.3399, 1.8432, 1.8150, 2.0642, 2.2960, 2.7075)
acc <- c(0, 0, 0, 0, 0, 1, 1, 0, 1, 0)
accu <- c("o", "o", "o", "o", "o", "u", "u", "o", "u", "o")
time <- data.frame(points, comp.time)




monte.carlo.pi<-function(n)
{
  start.time <- Sys.time()
  circle.points<-0
  square.points<-0
  x<-runif(n,0,1)
  y<-runif(n,0,1)
  z<-rep(0, n)
  for (i in 1:n)
  {
  if ((x[i]-.5)^2 + (y[i]-.5)^2 <=.5^2)
  {
  circle.points<-circle.points+1
  square.points<-square.points+1
  z[i]<-1
  } else
  {
  square.points<-square.points+1
  }
  }
  plot.new()
  frame()
  plot(x,y,asp=1,xlim=c(0,1),ylim=c(0,1), pch=16, col=(c("blue","green")[as.factor(z)]), cex = 4/log(n))
  draw.circle(0.5,0.5,1/2,nv=1000,border=NULL,col=NA,lty=1,lwd=1)
  rect(0,0,1,1)
  cat("Estimate of pi: ", 4.0 * circle.points / square.points,"\n")
  cat("Computed in:    ", Sys.time() - start.time, "sec\n")
  #return()
}



ggplot(data = time, aes(x = points, y = comp.time)) + 
geom_point(color='black') +
geom_smooth(method = "lm", se = FALSE, color="red") +
xlab("Number of Points used in estimation") +
ylab("Computation time (seconds)") +
ggtitle("Linear Relationship of points and Computation Time")


mylm <- lm(comp.time ~ points, data=time)
pander(summary(mylm))

boxplot(mylm$residuals ~ accu, data=time)

plot(comp.time ~ points, data=time, pch = 21, col=time$accu)
plot(mylm, which=1:2)
plot(mylm$residuals, main="Residuals vs Order", xlab="",
ylab="Residuals")
