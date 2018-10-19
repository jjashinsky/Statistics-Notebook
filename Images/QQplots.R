errors <- rnorm(50, 0, 5)
X <- unif(50, 0, 100)
Y <- 3*X + 4 + errors 

mylm <- lm(log(Y) ~ X^2)

my.qq <- qqnorm(mylm$residuals)
qqline(mylm$residuals)



stuff <- -1*rexp(50,5)
my.qq <- qqnorm(stuff)
qqline(stuff)
