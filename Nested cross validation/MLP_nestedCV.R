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
data_train <- train2
data_test <- test

###########################################

### 케라스 모델

library(tensorflow)
library(ggplot2)
library(caTools)

str(data_train$LBP)
class(data_train$LBP)
data_train$LBP <- as.numeric(data_train$LBP)
table(data_train$LBP)
data_train$LBP <- ifelse(data_train$LBP==1, 0, 1)
data_test$LBP <- as.numeric(data_test$LBP)
data_test$LBP <- ifelse(data_test$LBP==1, 0, 1)
str(data_train$LBP)
str(data_test$LBP)
table(data_train$LBP)
table(data_test$LBP)

input <- as.matrix(data_train[1:13], ncol = 13)
output <- as.matrix(data_train[14], ncol = 1)
input2 <- as.matrix(data_test[1:13], ncol = 13)
output2 <- as.matrix(data_test[14], ncol = 1)

#3. seed 고정

seed <- 42
set.seed(seed)

#4. train set / test set split

input_train <- input
input_test <- input2
output_train <- output
output_test <- output2
str(output_test)


library(keras)

# 모델 정의하기
# 동일한 모델을 여러 번 인스턴스화해야 하기 때문에 함수를 사용해 모델을 생성한다.
build_model <- function() {
  model <- keras_model_sequential() %>% 
    layer_dense(units = 20, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>% 
    layer_dense(units = 10, kernel_regularizer = regularizer_l2(0.001), activation = "relu") %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    optimizer = "rmsprop",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
}


# k겹 검증하기
k <- 5
indices <- sample(1:nrow(input_train))
folds <- cut(1:length(indices), breaks = k, labels = FALSE)

num_epochs <- 100
all_scores <- c()
for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- input_train[val_indices,]
  val_targets <- output_train[val_indices]
  
  partial_train_data <- input_train[-val_indices,]
  partial_train_targets <- output_train[-val_indices]
  
  model <- build_model()
  model %>% fit(partial_train_data, partial_train_targets,
                epochs = num_epochs, batch_size = 20, verbose = 0)
  
  results <- model %>% evaluate(val_data, val_targets, verbose = 0)
  all_scores <- c(all_scores, results$accuracy)
}

all_scores
mean(all_scores)

num_epochs <- 200
all_acc_histories <- NULL
all_loss_histories <- NULL

for (i in 1:k) {
  cat("processing fold #", i, "\n")
  
  val_indices <- which(folds == i, arr.ind = TRUE)
  val_data <- input_train[val_indices,]
  val_targets <- output_train[val_indices]
  
  partial_train_data <- input_train[-val_indices,]
  partial_train_targets <- output_train[-val_indices]
  
  model <- build_model()
  
  history <- model %>% fit(
    partial_train_data, partial_train_targets,
    validation_data = list(val_data, val_targets),
    epochs = num_epochs, batch_size = 20, verbose = 0
  )
  acc_history <- history$metrics$val_acc
  all_acc_histories <- rbind(all_acc_histories, acc_history)
  loss_history <- history$metrics$val_loss
  all_loss_histories <- rbind(all_loss_histories, loss_history)
}

# 연속 평균 k겹 검증 점수의 이력 구축하기
average_acc_history <- data.frame(
  epoch = seq(1:ncol(all_acc_histories)),
  validation_acc = apply(all_acc_histories, 2, mean)
)
average_loss_history <- data.frame(
  epoch = seq(1:ncol(all_loss_histories)),
  validation_loss = apply(all_loss_histories, 2, mean)
)

# 검증 점수 그리기
library(ggplot2)
ggplot(average_acc_history, aes(x = epoch, y = validation_acc)) + geom_line()
ggplot(average_loss_history, aes(x = epoch, y = validation_loss)) + geom_line()

# geom_smooth()로 검증 점수를 그리기
ggplot(average_acc_history, aes(x = epoch, y = validation_acc)) + geom_smooth()
ggplot(average_loss_history, aes(x = epoch, y = validation_loss)) + geom_smooth()

# 최종 모델 훈련하기
model <- build_model()
model %>% fit(input_train, output_train,
              epochs = 20, batch_size = 20, verbose = 0)
results <- model %>% evaluate(input_test, output_test)

model %>% predict(input_test) -> pred_test
class(pred_test)
class(output_test)
ANN_ROC <- data.frame(x = pred_test, y = output_test)
write.csv(ANN_ROC, file = "C:/Users/jaege/Desktop/LBP/ROC/ANN_ROC5.csv")

require(Epi)
require(pROC)

output_test_df <- as.data.frame(output_test)
a1 = ROC(form = LBP ~ pred_test, data = output_test_df, plot="ROC")
b1 = roc(LBP ~ pred_test, output_test_df, ci=T, percent=T)

plot(b1)

b1
a1

optimal_lr.eta <-  function(x) {
  no = which.max(x$res$sens+x$res$spec)[1]
  result = x$res$lr.eta[no]
  result
}
optimal_lr.eta(a1)

optimal_cutpoint <-  function(x) {
  y = optimal_lr.eta(x)
  b0 = unname(x$lr$coeff[1])
  b1 = unname(x$lr$coeff[2])
  result = (-log(1/y-1)-b0)/b1
  result
} 

optimal_cutpoint(a1)

pred_test <- ifelse(pred_test > 0.421, 1, 0)
pred_test <- as.data.frame(pred_test)
  
library(gmodels)
CrossTable(x=pred_test$V1, y=output_test_df$LBP, chisq = T)
confusionMatrix(table(as.vector(pred_test$V1), as.vector(output_test_df$LBP)), positive = "1")
confusionMatrix(table(as.vector(pred_test$V1), as.vector(output_test_df$LBP)), positive = "1", mode = "prec_recall")

# ROC

library(ROCR)
library(epiR)
table1 <- as.table(matrix(c(1071,314,205,245), nrow = 2, byrow = TRUE))
epi.tests(table1)


