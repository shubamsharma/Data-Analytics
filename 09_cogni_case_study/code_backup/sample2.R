#
model <- lm(dist ~ speed, data = cars)
plot(dist ~ speed, data = cars)
abline(model$coefficients)

#
model <- lm(speed ~ dist, data = cars)
plot(speed ~ dist, data = cars)
abline(model$coefficients)

#
model <- lm(dist ~ scale(speed) + I(scale(speed)^2), data = cars)
plot(dist ~ scale(speed), data = cars)
lines(fitted(model) ~ scale(speed), data = cars)

fitted(model)
scale(cars$speed)

#
m1 <- lm(dist ~ poly(speed, 2), data = cars)
plot(dist ~ speed, data = cars)
lines(fitted(m1) ~ poly(speed, 2), data = cars)
lines(fitted(lm(dist ~ poly(speed, 2), data = cars)))

scale(cars$dist)

#
m1 <- lm(dist ~ poly(speed, 2), data = cars)
summary(m1)
plot(speed ~ dist, data = cars)
lines(fitted(m1),scale(cars$dist))
lines(fitted(m1) ~ speed, data = cars)

library("data.table")

#creating a dummy data table
DT <- data.table( ID = 1:50,
                  Capacity = sample(100:1000, size = 50, replace = F),
                  Code = sample(LETTERS[1:4], 50, replace = T),
                  State = rep(c("Alabama","Indiana","Texas","Nevada"), 50))

head(DT)

#simple data.table command
DT[Code == "C", mean(Capacity), State]






#Problem Statement

summary(iris)

table(iris$Species)


hist(iris$Sepal.Length)

plot(density(iris$Sepal.Length))


cor(iris$Sepal.Length, iris$Petal.Length)

cor(iris[,1:4])


aggregate(Sepal.Length ~ Species, summary, data=iris)

install.packages("party")

set.seed(1234)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]


install.packages("lme4", dependencies = TRUE)
library(lme4)
methods(sigma)
install.packages("pbkrtest", dependencies = TRUE)
library(caret)


library(caret)
inTrain = createDataPartition(df$yourFactor, p = 2/3, list = FALSE)
dfTrain=df[inTrain,]
dfTest=df[-inTrain,]



library(plyr)
df <- ddply(df, "cl", transform, set = sample(c("train", "test"), length(cl),
                                              replace = TRUE, prob = c(2, 1)))



table(trainData$Species)

table(iris$Species)

library(party)
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)
# check the prediction
table(predict(iris_ctree), trainData$Species)


plot(iris_ctree)



df <- ddply(df, "cl", transform,
            set = sample(c(rep("train", round(2/3 * length(cl)),
                               rep("test",  round(1/3 * length(cl)))))

                         
                         