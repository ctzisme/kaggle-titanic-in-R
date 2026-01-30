# Reference: 
# Will Cukierski. Titanic - Machine Learning from Disaster. https://kaggle.com/competitions/titanic, 2012. Kaggle.

library(tidyverse)
library(ranger)

traindata <- read.csv("train.csv")
testdata <- read.csv("test.csv")

summary(traindata)
summary(testdata)
unique(traindata$Embarked)

anyNA(traindata)
colSums(is.na(traindata))

anyNA(testdata)
colSums(is.na(testdata))


traindata$Age[is.na(traindata$Age)] <- median(traindata$Age, na.rm = TRUE)

testdata$Age[is.na(testdata$Age)]   <- median(testdata$Age, na.rm = TRUE)
testdata$Fare[is.na(testdata$Fare)]   <- median(testdata$Fare, na.rm = TRUE)

traindata$Embarked[trimws(traindata$Embarked) == ""] <- NA
anyNA(traindata)
traindata$Embarked[is.na(traindata$Embarked)] <- "S"

anyNA(traindata)
anyNA(testdata)
unique(traindata$Embarked)

traindata <- traindata |> select(-Name, -Ticket, -Cabin)
testdata  <- testdata  |> select(-Name, -Ticket, -Cabin)

traindata <- traindata |> mutate(FamilySize = SibSp+Parch+1)
testdata <- testdata |> mutate(FamilySize = SibSp+Parch+1)

traindata$IsAlone <- ifelse(traindata$FamilySize == 1, 1, 0)
testdata$IsAlone  <- ifelse(testdata$FamilySize == 1, 1, 0)

traindata <- traindata |> select(-SibSp, -Parch)
testdata  <- testdata  |> select(-SibSp, -Parch)

traindata$AgeBand <- cut(
  traindata$Age,
  breaks = c(0, 12, 18, 35, 60, Inf),
  labels = c("Child","Teen","Young","Adult","Senior")
)

testdata$AgeBand <- cut(
  testdata$Age,
  breaks = c(0, 12, 18, 35, 60, Inf),
  labels = c("Child","Teen","Young","Adult","Senior")
)

traindata <- traindata |>
  mutate(
    Sex = as.factor(Sex),
    Embarked = as.factor(Embarked),
    Survived = as.factor(Survived)
  )

testdata <- testdata |>
  mutate(
    Sex = as.factor(Sex),
    Embarked = as.factor(Embarked)
  )

k <- 5
n <- nrow(traindata)
fold_id <- sample(rep(1:k, length.out = n))

run_one_fold <- function(fold) {
  
  train_fold <- traindata[fold_id != fold, ]
  valid_fold <- traindata[fold_id == fold, ]
  
  rf_fit <- ranger(
    Survived ~ . - PassengerId,
    data = train_fold,
    classification = TRUE,
    num.trees = 500,
    mtry = 3
  )
  
  pred <- predict(rf_fit, data = valid_fold)$predictions
  
  pred_num <- as.integer(as.character(pred))
  true_num <- as.integer(as.character(valid_fold$Survived))
  
  mean(pred_num == true_num)
}

cv_acc <- sapply(1:k, run_one_fold)

cv_acc
mean(cv_acc)
sd(cv_acc)

rf_final <- ranger(
  Survived ~ . - PassengerId,
  data = traindata,
  classification = TRUE,
  num.trees = 800,
  mtry = 3,
  importance = "permutation"
)


pred <- predict(rf_final, data = valid_df)$predictions
mean(pred == valid_df$Survived)

test_pred <- predict(rf_final, data = testdata)$predictions

sort(rf_final$variable.importance, decreasing = TRUE)

submission <- tibble(
  PassengerId = testdata$PassengerId,
  Survived = as.integer(as.character(test_pred))
)

write_csv(submission, "submission.csv")



