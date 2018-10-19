View(Davis)
Davis2 <- Davis
Davis2[12,] <- Davis2[12, c(1,3,2,4,5)]
View(Davis2)

myglm <- glm(sex ~ height, data=Davis, family=binomial)

plot(sex=="M" ~ height, data=Davis, pch=16)
curve(exp(-64.95385+.37835*x)/(1+exp(-64.95385+.37835*x)), add=TRUE)
summary(myglm)


## There was an error in the twelth row. 
## This is the code to switch the columns of that row.

myglm2 <- glm(sex ~ height, data=Davis2, family=binomial)
b <-myglm2$coefficients

plot(sex=="M" ~ height, data=Davis2, pch=16)
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), add=TRUE)
summary(myglm2)

## if someone had a height of 170cm, 
## whats the probability that they are Male?

exp(b[1]+b[2]*170)/(1+exp(b[1]+b[2]*170))

## or you could predict this way 

predict(myglm2, data.frame(height=170), type="response")
predict(myglm2, data.frame(height=160), type="response")
predict(myglm2, data.frame(height=150), type="response")
predict(myglm2, data.frame(height=180), type="response")
predict(myglm2, data.frame(height=175.2), type="response")

## to find the odds
exp(.37982)
## The odds increase by this much for every one increase in x. 