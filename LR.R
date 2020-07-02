# Library �ҷ����� ----

setwd("C:/Users/jaege/Desktop/LBP")

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 6119��, train 4284��, test 1835��

# 1. ������ �ҷ�����
data <- read.csv("hn1415_p.csv", header=TRUE)
data <- data[ , c(2:32)]
data$LBP <- factor(data$LBP, labels=c("no", "yes"))
head(data)

data <- select(data,OA,age,sex,depression,income,fbg,smoking,dm,activity,
               sitting,dyslipidemia,bmi,ihd,LBP)

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

###########################################

# 3. Logistic regression �� ����

# 3-1. �⺻ ��
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              classProbs = TRUE)

LogitModel <- caret::train(LBP ~ .,
                           data = train,
                           method = "LogitBoost",
                           trControl = train.control)
LogitModel
varImp(LogitModel)


# 3. �� ��

# 3-1) �Ʒ� ���� ���� Class ����

train_pred <- predict(LogitModel, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2) �׽�Ʈ ���� ���� Class ����

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
table1 <- as.table(matrix(c(1152,265,239,179), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/LR_ROC.csv")