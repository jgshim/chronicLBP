# Library 불러오기 ----

setwd("C:/Users/jaege/Desktop/LBP")

library(tidyverse)
library(DMwR)
library(caret)

########## Binary classification ###########

# total 6119명, train 4284명, test 1835명

# 1. 데이터 불러오기

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

# 2. SVM Radial 모델 적용

# 2-1. 기본 모델

set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 10,
                              repeats = 5,
                              classProbs = TRUE)

svmModel1 <- caret::train(LBP ~ ., data = train,
                   method = "svmPoly",
                   trControl = train.control,
                   metric = "Accuracy")

ggplot(svmModel1)
svmModel1
varImp(svmModel1)

# 2-2. 그리드탐색

Grid <- expand.grid(sigma=c(0,0.01, 0.02, 0.025, 0.03, 0.04, 0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                    C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.5, 2,5))  

set.seed(42)
svmModel2 <- train(OST ~ ., data=data_train,
                   method="svmRadial",
                   trControl=train.control,
                   metric = "Accuracy",
                   tuneGrid = Grid)
svmModel2

trellis.par.set(caretTheme())
plot(svmModel2) 
plot(svmModel2, metric = "Kappa")
plot(svmModel2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(svmModel2) 

# 2-3. 랜덤 서치 기반 모델 튜닝

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 10,
                              classProbs = TRUE,
                              search = "random")
set.seed(42)
svmModel3 <- train(OST ~ ., data = data_train, 
                   method = "svmRadial",
                   metric = "ROC",
                   tuneLength = 30,
                   trControl = train.control)
svmModel3

plot(svmModel3) 
plot(svmModel3, metric = "Kappa")
plot(svmModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(svmModel3) 

# 3. 모델 평가

# 3-1) 훈련 모델의 예측 Class 측정

train_pred <- predict(svmModel1, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2) 테스트 모델의 예측 Class 측정

test_pred <- predict(svmModel1, test)
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes")
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$LBP)

# 3-3) ROC

require(Epi)
require(pROC)

predictedProbs <- predict(svmModel1, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = LBP ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(LBP ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(1029, 388, 178, 240), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/SVM_ROC.csv")
