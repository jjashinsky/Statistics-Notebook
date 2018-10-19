library(mosaic)
library(car)
library(ResourceSelection)
View(infert)
?infert
infert.glm <-glm((spontaneous > 0) ~ age, data=infert, family=binomial)
summary(infert.glm)
plot((spontaneous > 0) ~ age, data=infert)
b <- infert.glm$coefficients

curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), add=TRUE)

table(infert$age)

pchisq(334.01, 246, lower.tail=FALSE)

View(Galton)

galton.glm <-glm(sex=="M" ~ height, data=Galton, family=binomial)
summary(galton.glm)
plot(sex=="M" ~ height, data=Galton)
b <- galton.glm$coefficients
curve(exp(b[1]+b[2]*x)/(1+exp(b[1]+b[2]*x)), add=TRUE)
predict(galton.glm, data.frame(height=65), type="response")

(exp(b[1]+b[2]*90))/(1+exp(b[1]+b[2]*90))
                
pchisq(626.22, 896, lower.tail=FALSE)
hoslem.test(galton.glm$y, galton.glm$fitted)
