# Library 불러오기 ----

setwd("C:/Users/jaege/Desktop/LBP")

library(tidyverse)
library(DMwR)
library(caret)

########## Binary classification ###########

# total 6119명, train 4284명, test 1835명

# 1. 데이터 불러오기
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

set.seed(42)

# 2. Decision Tree 모델 적용

# 2-1. 기본 모델

train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              savePredictions = TRUE)

TreeModel1 <- caret::train(LBP ~ ., data = train,
                    method="rpart",
                    metric = "Accuracy",
                    trControl=train.control)

TreeModel1
TreeModel1$finalModel

varImp(TreeModel1)
plot(TreeModel1$finalModel, uniform=TRUE, main="Classification Tree")
text(TreeModel1$finalModel, use.n.=TRUE, all=TRUE, cex=.5)

suppressMessages(library(rattle))
fancyRpartPlot(TreeModel1$finalModel)

library(rpart.plot)
rpart.plot(TreeModel1$finalModel)

# 2-2. 그리드탐색

Grid <- expand.grid(cp=0:1/10) 

set.seed(42)
TreeMode2 <- train(LBP ~ ., data=train,
                   method="rpart",
                   trControl=train.control,
                   metric = "Accuracy",
                   tuneGrid = Grid)

TreeMode2

trellis.par.set(caretTheme())
plot(TreeMode2) 
plot(TreeMode2, metric = "Kappa")
plot(TreeMode2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(TreeMode2)  

# 2-3. 랜덤 서치 기반 모델 튜닝

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 3,
                              classProbs = TRUE,
                              search = "random")

set.seed(42)
TreeModel3 <- train(LBP ~ ., data = train, 
                    method = "rpart",
                    metric = "Accuracy",
                    tuneLength = 30,
                    trControl = train.control)
TreeModel3

plot(TreeModel3) 
plot(TreeModel3, metric = "Kappa")
plot(TreeModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(TreeModel3)  

# 2-4. 최종모델의 선택

finalControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(42)
FinalModel <- caret::train(LBP ~ ., data = train, 
                    method = "rpart", 
                    trControl = finalControl, 
                    tuneGrid = data.frame(cp=0.0105932203),
                    metric = "ROC")
FinalModel


# 3. 모델 평가

# 3-1. 훈련 모델의 예측 Class 측정

train_pred <- predict(FinalModel, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2. 테스트 모델의 예측 Class 측정

test_pred <- predict(FinalModel, test)
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes")
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$LBP)

# 3-3. ROC

require(Epi)
require(pROC)

predictedProbs <- predict(FinalModel, test , type = "prob")
head(predictedProbs)

a1 = ROC(form = LBP ~ predictedProbs$yes, data = test, plot="ROC")
b1 = roc(LBP ~ predictedProbs$yes, test, ci=T, percent=T)

plot(b1)

b1
a1

library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(940, 445, 169, 281), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/DT_ROC1.csv")

