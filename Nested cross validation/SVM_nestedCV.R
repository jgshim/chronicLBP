# Library 불러오기 ----

setwd("C:/Users/jaege/Desktop/LBP")

library(tidyverse)
library(DMwR)
library(caret)

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
training.samples <- createDataPartition(data_4$LBP, p = 0.7, list = FALSE)
train  <- data_4[training.samples, ]
test <- data_4[-training.samples, ]

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

# 2. SVM Radial 모델 적용

# 2-1. 기본 모델

set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 5,
                              repeats = 5,
                              classProbs = TRUE)

svmModel1 <- caret::train(LBP ~ ., data = train,
                   method = "svmRadial",
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
table1 <- as.table(matrix(c(964, 421, 171, 279), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/SVM_ROC1.csv")
