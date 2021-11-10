# Library 불러오기 ----

setwd("C:/Users/jaege/Desktop/LBP")

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 6119명

# Import data

data <- read.csv("hn1415.csv", header=TRUE)
data <- data[ , c(2:32)]

# Outer CV (k=5)

k_fold_result <- createFolds(data$LBP, k=5, list=TRUE, returnTrain = FALSE)
k_fold_result$Fold1 #K 갯수만큼 리스트 원소가 있음.

# Fold 별 데이터 나누어 저장하기
data_1 <- data[k_fold_result$Fold1,]
data_2 <- data[k_fold_result$Fold2,]
data_3 <- data[k_fold_result$Fold3,]
data_4 <- data[k_fold_result$Fold4,]
data_5 <- data[k_fold_result$Fold5,]

# k = 5 inner CV

# Train-Test Split

set.seed(42)
training.samples <- createDataPartition(data_5$LBP, p = 0.7, list = FALSE)
train  <- data_5[training.samples, ]
test <- data_5[-training.samples, ]

table(train$LBP)
table(test$LBP)

# Training Data scaling ----

train_cat <- train %>% 
  select(sex, dyslipidemia, htn, dm, ihd, svd, depression, smoking, alcohol, marriage, occupation.1, occupation.2, occupation.3, occupation.4, occupation.5, occupation.6, occupation.7, income, education, activity, sleep, OA, RA, sitting, stress)
train_num <- train %>% 
  select(age, height, weight, bmi, fbg)
train_class <- train %>% 
  select(LBP)

# 표준화(평균 0, 표준편차 1) scaling

train_StandardScale <- preProcess(train_num, method=c("center", "scale"))
print(train_StandardScale)
train_standard <- predict(train_StandardScale, train_num)
head(train_standard)

# Test Data scaling ----

test_cat <- test %>% 
  select(sex, dyslipidemia, htn, dm, ihd, svd, depression, smoking, alcohol, marriage, occupation.1, occupation.2, occupation.3, occupation.4, occupation.5, occupation.6, occupation.7, income, education, activity, sleep, OA, RA, sitting, stress)
test_num <- test %>% 
  select(age, height, weight, bmi, fbg)
test_class <- test %>% 
  select(LBP)

# 표준화(평균 0, 표준편차 1) scaling

test_standard <- predict(train_StandardScale, test_num)
head(test_standard)

train = cbind(train_cat, train_standard, train_class)
test = cbind(test_cat, test_standard, test_class)

train$LBP <- factor(train$LBP, labels=c("no", "yes"))
head(train)

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

###########################################

# 3. Logistic regression 모델 적용

# 3-1. 기본 모델
train.control <- trainControl(method = "repeatedcv", 
                              number = 5, 
                              repeats = 3,
                              classProbs = TRUE)

LogitModel <- caret::train(LBP ~ .,
                           data = train,
                           method = "LogitBoost",
                           trControl = train.control)
LogitModel
varImp(LogitModel)


# 3. 모델 평가

# 3-1) 훈련 모델의 예측 Class 측정

train_pred <- predict(LogitModel, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2) 테스트 모델의 예측 Class 측정

test_pred <- predict(LogitModel, test)
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes")
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$LBP)


# 3-3) ROC

require(Epi)
require(pROC)

predictedProbs <- predict(LogitModel, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = LBP ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(LBP ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(973,412,219,231), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/LR_ROC1.csv")
