# Library �ҷ����� ----

setwd("C:/Users/jaege/Desktop/LBP")

library(tidyverse)
library(DMwR)
library(caret)

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

# 2. Decision Tree �� ����

# 2-1. �⺻ ��

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

# 2-2. �׸���Ž��

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

# 2-3. ���� ��ġ ��� �� Ʃ��

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

# 2-4. �������� ����

finalControl <- trainControl(method = "none", classProbs = TRUE)

set.seed(42)
FinalModel <- caret::train(LBP ~ ., data = train, 
                    method = "rpart", 
                    trControl = finalControl, 
                    tuneGrid = data.frame(cp=0.01229508),
                    metric = "ROC")
FinalModel


# 3. �� ��

# 3-1. �Ʒ� ���� ���� Class ����

train_pred <- predict(FinalModel, train)
confusionMatrix(data = train_pred, reference = train$LBP)
confusionMatrix(data = train_pred, reference = train$LBP, mode = "prec_recall")
postResample(pred = train_pred, obs = train$LBP)

# 3-2. �׽�Ʈ ���� ���� Class ����

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
table1 <- as.table(matrix(c(1029, 388, 166, 252), nrow = 2, byrow = TRUE))
epi.tests(table1)

write.csv(predictedProbs, file = "C:/Users/jaege/Desktop/LBP/ROC/DT_ROC.csv")

