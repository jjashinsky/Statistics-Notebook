library(car)

X <- runif(50,0,100)
Y <- rnorm(50,40,10)
plot(Y ~ X, pch=19, col="red",cex.axis=1.8, cex.lab=1.8, cex=1.5)
curve(40+0*x,add=TRUE, lwd=2)




Y <- 40 - 1.3*X + rnorm(50,0,60)
plot(Y ~ X, pch=19,cex.axis=1.8, cex.lab=1.8, cex=1.5, ylim=c(-250,200))
cor(X,Y)


X <- runif(50, -10, 10)
Y <- X^3 + 7*X^2 + rnorm(50,0,100)
plot(Y ~ X, pch=19,cex.axis=1.8, cex.lab=1.8, cex=1.3)
