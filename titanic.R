library(tidyverse)
# Read in as a data frame
titanic <- read.csv('train.csv')
# Factor variables 'Sex' and 'Embarked'
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)
# Fit a general linear regression model. Name, Ticket, and Cabin cannot affect the regression, so I did not include them in my model
titanic.lg <- glm(Survived ~ .-PassengerId-Name-Ticket-Cabin, data = titanic, family = 'binomial')
summary(titanic.lg)
anova(titanic.lg,test="Chisq")
# We can see that all variables except for Pclass, Sex, Age, and SibSp are not significant in terms of Survived, So I only kept the variables that are significant
titanic <- titanic[,c(1, 2, 3, 5, 6, 7)]
titanic.lg <- glm(Survived ~ .-PassengerId, data = titanic, family = 'binomial')
summary(titanic.lg)

titanic.predict <- read.csv('test.csv')
# Keep only those columns that are used in the regression model
titanic.predict <- titanic.predict[,c(1, 2, 4, 5, 6)]
titanic.predict$Sex <- factor(titanic.predict$Sex)
probs <- as.vector(predict(titanic.lg, newdata = titanic.predict, type = 'response'))
preds <- rep(0,418)
preds[probs>0.5] <- 1
# Create a dataframe with the passenger ID and my predictions of survival
table <- data.frame(titanic.predict$PassengerId, preds)
write.table(table, file = 'titanic-predictions.csv', row.names = F, col.names = c('PassengerID', 'Survived'), sep = ',')