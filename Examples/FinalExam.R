#1
View(Highway1)
?Highway1
favstats(rate ~ htype, data=Highway1)

#2
plot(rate ~ slim, data=Highway1, pch=16, xlab="", ylab="", main="")

#3
?ToothGrowth
View(ToothGrowth)
t.test(len ~ supp, data = ToothGrowth, mu = 0, alternative = "two.sided", conf.level = 0.95)
boxplot(len~supp,data=ToothGrowth)

#5
Highway2 <- subset(Highway1, lane==2 | lane==4)
wilcox.test(rate ~ lane, data=Highway2, mu=0, alternative="two.sided", conf.level=0.95)
boxplot(rate ~ lane, data=Highway2)

#6
View(KidsFeet)

#7
View(starwars)
starwars2 <- subset(starwars, species=="Wookiee" | species=="Gungan" | species=="Kaminoan")
View(starwars2)
boxplot(height ~ species, data=starwars2)

#8
xyplot(length ~ sex, data=KidsFeet, group=domhand, type=c("p","a"), auto.key=TRUE)
myaov <- aov(length ~ sex+domhand+sex:domhand, data=KidsFeet)
summary(myaov)

#9
View(singer)
boxplot(height ~ voice.part, data=singer)

#11
?cars
View(cars)
mylm <- lm(speed ~ dist, data=cars)
summary(mylm)
8.28391+130*.16557

#12
plot(height ~ age, data=Loblolly)

#13
View(iris)
mylm <- lm(Sepal.Length ~ Sepal.Width + Species, data=iris)
summary(mylm)
2.251 + 1.4587

#16
exp(.251)

#17
View(cars)
plot(speed > 15 ~ dist, data=cars, ylab="Probability Speed > 15 mph", xlab="Stopping Distance (feet)")
myglm <- glm(speed > 15 ~ dist, data=cars, family=binomial)
summary(myglm)
predict(myglm, data.frame(dist=70), type="response")

#20
View(Galton)
myTest <- t.test(height ~ sex, data = Galton, mu = 0)
observedTestStat <- myTest$statistic

# Now we run the permutation for a distribution of test statistics
N <- 2000
permutedTestStats <- rep(NA, N)
for (i in 1:N){
  permutedData <- sample(Galton$sex)
  permutedTest <- t.test(height ~ permutedData, data = Galton, mu = 0)
  permutedTestStats[i] <- permutedTest$statistic
}

# Now we create a histogram of that distribution
hist(permutedTestStats, col = "skyblue")
abline(v = observedTestStat, col = "red", lwd = 3)

#Greater-Than p-value: Not the correct one in this case
sum(permutedTestStats >= observedTestStat)/N

# Less-Than p-value: This is the corret one
sum(permutedTestStats <= observedTestStat)/N

# Two-Sided p-value
2*sum(permutedTestStats <= observedTestStat)/N


#22
hist(islands, xlab="Area in Thousands of Square Miles", main="Areas of the World's Major Landmasses")
summary(islands)
mean(islands)
median(islands)
sd(islands)

#23
pf(15.36, 5, 65, lower.tail = FALSE)

#24
View(KidsFeet)
KidsFeet2 <- subset(KidsFeet, name=="David" | name=="Josh" | name=="Caitlin")
View(KidsFeet2)
mean(width ~ name, data=KidsFeet2)

#25
View(mtcars)
hist(am ~ cyl, data=mtcars)
mytable <- table(mtcars$cyl, mtcars$am)
barplot(mytable, beside=TRUE, legend.text = TRUE)
mytable
