# Naive Bayes

#1. 데이터 불러오기

setwd("C:/Users/jaege/Desktop/LBP")

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 6119명, train 4725명, test 1394명

# 1. 데이터 불러오기

data <- read.csv("hn1415_p.csv", header=TRUE)
data <- data[ , c(2:32)]
data$LBP <- factor(data$LBP, labels=c("no", "yes"))
head(data)

data <- select(data,OA,age,sex,depression,income,fbg,smoking,dm,activity,
               sitting,dyslipidemia,bmi,ihd,LBP)

#2. traing-test 데이터 분할

set.seed(42)
training.samples <- createDataPartition(data$LBP, p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

table(train$LBP)
table(test$LBP)

# SMOTE
train2 <- SMOTE(LBP~., train, perc.over = 100, perc.under=200)
table(train2$LBP)
train <- train2

# 3. 학습

train.control <- trainControl(method="repeatedcv",
                              number = 10,
                              repeats = 10,
                              savePredictions = TRUE)

model <- train(LBP ~ ., data = train,
               method = "naive_bayes",
               trControl = train.control,
               metric="Accuracy")

model
# Plot model accuracy 
plot(model)

# 4. 모델 평가

# 4-1. 훈련 모델의 예측 Class 측정

train_pred <- predict(model, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 4-2. 테스트 모델의 예측 Class 측정

test_pred <- predict(model, test)
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes")
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$LBP)

# 4-3. ROC

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
table1 <- as.table(matrix(c(929, 488, 150, 268), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/nb_ROC.csv")

#5. 변수중요도

importance_nb <- varImp(model, scale=FALSE)
plot(importance_nb, xlim = c(0,1))

