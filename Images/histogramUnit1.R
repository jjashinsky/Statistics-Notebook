library(mosaic)
X<-rnorm(50, 30, 10)
X2 <- X + 60
Y<-rnorm(50, 30, 20)
Y2<-rnorm(25, 30, 25)

par(mfrow=c(2,2))
hist(X, main="Histogram 1", 
     col="skyblue", 
     xlab="", 
     cex.lab=2, cex.axis=2, cex.main=2,
     ylab="", xlim=c(-10, 100))

hist(X2, main="Histogram 2", 
     col="skyblue", 
     xlab="", ylab="", 
     cex.lab=2, cex.axis=2, cex.main=2, xlim=c(-10, 100))

hist(Y, main="Histogram 3", 
     col="skyblue", 
     xlab="", ylab="", 
     cex.lab=2, cex.axis=2, cex.main=2, xlim=c(-10, 100))

hist(Y2, main="Histogram 4", 
     col="skyblue", 
     xlab="", ylab="", 
     cex.lab=2, cex.axis=2, cex.main=2, xlim=c(-10, 100))




boxplots <- data.frame(Plot1 = X, Plot2 = Y2, Plot3 = Y, Plot4=X2)
boxplot(boxplots$Plot1,boxplots$Plot2,boxplots$Plot3,boxplots$Plot4, 
        names=c("Plot 1", "Plot 2", "Plot 3", "Plot 4"),
        col=c("steelblue4", "steelblue3", "steelblue2", "steelblue1"),
        cex.axis=1.5, cex=1.2, pch=19, lwd=1.5)







H1 <- rnorm(50, 0,1)
hist(H1,col="turquoise1", main="", xlab="", ylab="", cex.axis=1.5)

H2 <- -1*rexp(40,3)
hist(H2,col="turquoise2", main="", xlab="", ylab="", cex.axis=1.5)

H3 <- rgamma(20, 4,5) 
hist(H3,col="turquoise3", main="", xlab="", ylab="", cex.axis=1.5)

H4 <- runif(400,1,10)
hist(H4,col="turquoise4", main="", xlab="", ylab="", cex.axis=1.5)




