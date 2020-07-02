setwd("C:/Users/jaege/Desktop/LBP/ROC")

library(precrec)
library(plotROC)
library(ggplot2)
library(ROCR)

# total 6119Έν, train 4284Έν, test 1835Έν

total_ROC <- read.csv("Total_ROC.csv", header=TRUE)

total_ROC <- melt_roc(total_ROC, "LBP", c("ANN", "DT", "LR", "RF", "SVM", "GBM", "KNN", "NB"))

head(total_ROC)

ggplot(total_ROC, aes(d = D, m = M, color = name)) + geom_roc() + style_roc()

ggplot(total_ROC, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = 0) + style_roc() +
  ggplot2::theme_classic()

