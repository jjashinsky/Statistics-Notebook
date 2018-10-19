states <- data.frame(state.x77)
View(states)
mylm <- lm(Illiteracy ~ Income + Life.Exp, data=states)
summary(mylm)
par(mfrow=c(1,2))
plot(mylm, which=1:2)


library(car)

?Highway1
View(Highway1)

mylm <- lm(rate ~ slim + shld + trks, data=Highway1)
mylm$coefficients
17.69097 + -.20484*55 + .02015*6 + -.28175*10 

?mpg
View(mpg)
plot(hwy ~ cty, data=mpg)
mylm <- lm(hwy ~ cty, data=mpg)
summary(mylm)
par(mfrow=c(1,2))
plot(mylm, which=1:2)

plot(mylm$residuals ~ cyl, data=mpg)
boxplot(mylm$residuals ~ drv, data=mpg)
plot(mylm$residuals ~ displ, data=mpg)

mylm <- lm(hwy ~ cty + drv, data=mpg)
mplot(hwy ~ cty, data=mpg, col=mpg$drv)
?mplot
