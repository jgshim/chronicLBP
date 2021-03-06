# Library �ҷ����� ----

setwd("C:/Users/jaege/Desktop/LBP")

library(caret)
library(tidyverse)
library(DMwR)

########## Binary classification ###########

# total 6119��, train 4725��, test 1394��

# 1. ������ �ҷ�����
data <- read.csv("hn1415_p.csv", header=TRUE)
data <- data[ , c(2:32)]
data$LBP <- factor(data$LBP, labels=c("no", "yes"))
head(data)

data <- select(data,OA,age,sex,depression,income,fbg,smoking,dm,activity,
               sitting,dyslipidemia,bmi,ihd,LBP)

data <- select(data, -weight, -height)

set.seed(42)
training.samples <- createDataPartition(data$LBP, p = 0.7, list = FALSE)
train  <- data[training.samples, ]
test <- data[-training.samples, ]

# SMOTE
train2 <- SMOTE(LBP~., train, perc.over = 100, perc.under=200)
table(train2$LBP)
train <- train2

# 2. Random Forest �� ����

# 2-1. �⺻ ��
train.control <- trainControl(method = "repeatedcv", 
                              number = 10, 
                              repeats = 3,
                              savePredictions = TRUE,
                              classProbs =  TRUE)

RFModel1 <- caret::train(LBP ~ ., data=train,
                  method="rf",
                  trControl=train.control,
                  tuneLength = 10)

RFModel1
varImp(RFModel1)

# estimate variable importance
importance <- varImp(RFModel1)
# summarize importance
print(importance)
# plot importance
plot(importance)


# 2-2. �׸���Ž��

Grid <- expand.grid(mtry = c(1:14)) 

set.seed(42)
train.control <- trainControl(method = "repeatedcv", 
                              number = 5, 
                              repeats = 10,
                              savePredictions = TRUE)

RFModel2 <- train(LBP ~ ., data=train,
                  method="rf",
                  trControl=train.control,
                  metric = "Accuracy",
                  tuneGrid = Grid)
RFModel2

trellis.par.set(caretTheme())
plot(RFModel2) 
plot(RFModel2, metric = "Kappa")
plot(RFModel2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(RFModel2) 

# 2-3. ���� ��ġ ��� �� Ʃ��

train.control <- trainControl(method = "repeatedcv",
                              number = 5,
                              repeats = 10,
                              classProbs = TRUE,
                              search = "random")
set.seed(42)
RFModel3 <- train(LBP ~ ., data = train, 
                  method = "rf",
                  metric = "Accuracy",
                  tuneLength = 30,
                  trControl = train.control)
RFModel3

plot(RFModel3) 
plot(RFModel3, metric = "Kappa")
plot(RFModel3, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))
ggplot(RFModel3) 

# 2-4. �������� ����

finalControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(42)
FinalModel <- caret::train(LBP ~ ., data = train, 
                    method = "rf", 
                    trControl = finalControl, 
                    tuneGrid = data.frame(mtry=3),
                    metric = "ROC")

# 3. �� ��

# 3-1) �Ʒ� ���� ���� Class ����

train_pred <- predict(FinalModel, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2) �׽�Ʈ ���� ���� Class ����

test_pred <- predict(FinalModel, test)
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes")
confusionMatrix(data = test_pred, reference = test$LBP, positive = "yes", mode = "prec_recall")
postResample(pred = test_pred, obs = test$LBP)


# 3-3) ROC

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
table1 <- as.table(matrix(c(1070, 347, 195, 223), nrow = 2, byrow = TRUE))
epi.tests(table1)


write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/RF_ROC.csv")
