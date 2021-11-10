# Library �ҷ����� ----

setwd("C:/Users/jaege/Desktop/LBP")

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 6119��, train 4284��, test 1835��

# 1. ������ �ҷ�����
train <- read.csv("train_p.csv", header=TRUE)
train <- train[ , c(2:32)]
train$LBP <- factor(train$LBP, labels=c("no", "yes"))
head(train)

test <- read.csv("test_p.csv", header=TRUE)
test <- test[ , c(2:32)]
test$LBP <- factor(test$LBP, labels=c("no", "yes"))
head(test)

# Feature selection (resulted by RFE algorithm)

train <- select(train,OA,age,sex,depression,income,fbg,smoking,dm,activity,
                sitting,dyslipidemia,bmi,ihd,LBP)

test <- select(test,OA,age,sex,depression,income,fbg,smoking,dm,activity,
               sitting,dyslipidemia,bmi,ihd,LBP)

table(train$LBP)
table(test$LBP)

# SMOTE
train2 <- SMOTE(LBP~., train, perc.over = 100, perc.under=200)
table(train2$LBP)
train <- train2

# 2. k-nearest neighbors (KNN) �� ����

# The k-nearest neighbors (KNN) algorithm is a simple machine learning method used for both classification and regression.
# The kNN algorithm predicts the outcome of a new observation by comparing it to k similar cases in the training data set, where k is defined by the analyst.

# Fit the model on the training set
set.seed(42)

train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 3,
                              savePredictions = TRUE)

model <- caret::train(LBP ~ ., data = train,
                      method = "knn",
                      trControl = train.control,
                      tuneLength = 20)

model

# Plot model accuracy vs different values of k
plot(model)

# Print the best tuning parameter k that maximizes model accuracy
model$bestTune

# 3. �� ��

# 3-1. �Ʒ� ���� ���� Class ����

train_pred <- predict(model, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2. �׽�Ʈ ���� ���� Class ����

test_pred <- predict(model, test)
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes")
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$LBP)

# 3-3. ROC

require(Epi)
require(pROC)

predictedProbs <- predict(model, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = LBP ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(LBP ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(889, 496, 181, 269), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/knn_ROC1.csv")