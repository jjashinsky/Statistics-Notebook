#-----------------------------------------------#
## Define TRUTH (this would be done in heaven)

beta0 <- 53
beta1 <- 2.5
beta2 <- 1.6
beta3 <- 1.4

N <- 80
X1 <- rnorm(N, 10, 2)
X2 <- sample(c("Bond","Free"), N, replace=TRUE)

E <- rnorm(N, 0, 5)

Y <- beta0 + beta1*X1 + beta2*(X2=="Free") + beta3*X1*(X2=="Free") + E

X3 <- rf(N, 3, 20)

#------------------------------------------------#
# This is where the real world would begin,
# with data. Our goal would be to uncover the
# truth that was "created in heaven."
  
myData <- data.frame(Y, X1, X2, X3)  

# Try a first "Simple Model"
myData.lm <- lm(Y ~ X1, data=myData)
plot(Y ~ X1, data=myData)
abline(myData.lm)
plot(myData.lm, which=1:2)

# Consider the added variable plot for X2
plot(myData.lm$residuals ~ X2, data=myData)  
  # We should add X2, it shows a nice pattern

# Consider the added variable plot for X3
plot(myData.lm$residuals ~ X2, data=myData)  
  # Leave X3 out, it shows no pattern

# Perform a second regression including X2
myData.lm2 <- lm(Y ~ X1 + X2, data=myData)
plot(Y ~ X1, data=myData, col=X2)
plot(myData.lm2, which=1:2)
b <- coef(myData.lm2)
abline(b[1], b[2]) #add the first line
abline(b[1]+b[3], b[2], col="red") #add the second





