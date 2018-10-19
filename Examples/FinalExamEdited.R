library(mosaic)
library(car)

#2

View(CO2)
favstats(uptake ~ Type, data=CO2)

#4
?warpbreaks
warp.aov <- aov(breaks ~ wool + tension + wool:tension, data=warpbreaks)
summary(warp.aov)

#7
?mtcars
View(mtcars)
mylm <- lm(qsec ~ mpg +am, data=mtcars)
summary(mylm)

#8
chile.glm <- glm(vote=="Y" ~ age, data=Chile, family="binomial")
summary(chile.glm)
stripchart(Chile$age, method="stack", ylim=c(1, 2))
pchisq(3229.4, 2529, lower.tail=FALSE)

#9
View(mtcars)
mylm <- lm(qsec ~ mpg + am, data=mtcars)
summary(mylm)


#10
?USArrests
mylm <- lm(Assault ~ Murder, data=USArrests)
plot(mylm, which=1:2)


#12
View(Salaries)
Salaries2 <- subset(Salaries, rank == "AsstProf")
wilcox.test(salary ~ sex, data = Salaries2, mu = 0, alternative = "two.sided", conf.level = 0.95)

#13
View(Davis)
View(Davis2)
Davis2 <- subset(Davis, height != 57 )
t.test(height ~ sex, data = Davis2, mu = 0, alternative = "greater", conf.level = 0.95)

#15
boxplot(yrs.service ~ rank, data=Salaries)

#22
View(USArrests)
?USArrests
plot(Murder ~ UrbanPop, data=USArrests)

#25
USArrests$total <- USArrests$Murder + USArrests$Assault + USArrests$Rape
View(USArrests)

boxplot(exercise ~ group, data=Blackmore)
kruskal.test(exercise ~ group, data=Blackmore)
wilcox.test(exercise ~ group, data = Blackmore, mu = 0, alternative = "two.sided", conf.level = 0.95)
t.test(exercise ~ group, data = Blackmore, mu = 0, alternative = "two.sided", conf.level = 0.95)


?Wong
longwong <- with(Wong, table(sex, LongerThanThreeDays = duration > 3))
longwong
barplot(longwong, beside=TRUE, legend=TRUE, args.legend=list(x="topleft", bty='n'), xlab="Coma Lasted Longer than Three Days")
chisq.test(longwong)


chisq.test(apply(Titanic, c(2,4), sum))
chisq.test(apply(Titanic, c(2,4), sum))$residuals
