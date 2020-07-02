# Library 불러오기 ----

setwd("C:/Users/jaege/Desktop/LBP")

library(caret)
library(tidyverse)
library(MASS)
library(mlbench)

########## Binary classification ###########

# total 3150명, train 4284명, test 1835명

# 1. 데이터 불러오기
data <- read.csv("hn1415_p.csv", header=TRUE)
data <- data[ , c(2:32)]
data <- select(data,-height,-weight)

raw_data <- read.csv("hn1415.csv", header=TRUE)
raw_data <- raw_data[ , c(2:32)]
raw_data <- select(raw_data,-height,-weight)

data$LBP <- factor(data$LBP, labels=c("no", "yes"))
head(data)
class(data$LBP)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data[,1:28], data[,29], sizes=c(1:28), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

importance <- varImp(results, scale=FALSE)
print(importance)
plot(results)
ggplot(results)

# prepare training scheme

data_reduced <- select(data,OA,age,sex,depression,income,fbg,smoking,dm,activity,
                       sitting,dyslipidemia,bmi,ihd,LBP)

control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- caret::train(LBP~., data=data_reduced, method="lvq",trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)

# calculate correlation matrix
data$LBP <- as.numeric(data$LBP)
correlationMatrix <- cor(data[,1:29])

# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

raw_data <- rename(raw_data,
               age = age,                               # 나이
               BMI = bmi,                               # BMI
               fastingbloodglucose = fbg,             # Fasting blood glucose
               sex = sex,                # 성별
               hyperlipidemia = dyslipidemia,  # Dyslipidemia
               hypertension = htn,              # HTN
               diabetesmellitus = dm,               # DM
               ischemicheartdisease = ihd,             # IHD
               cerebrovasculardisease = svd,             # SVD
               depressivesymptom = depression,      # Depression
               smokingstatus = smoking,          # Smoking status
               alcoholintake = alcohol,            # Alcohol
               maritalstatus = marriage,       # Marriage
               managersexperts = occupation.1,        # Occupation
               officework = occupation.2,
               salesservices = occupation.3,
               agricultureforestryfishery = occupation.4,
               machinefitting = occupation.5,
               simplelabor = occupation.6,
               unemployed = occupation.7,
               householdincome = income,         # Household income
               educationlevel = education,          # Education
               physicalactivity = activity,        # Physical activity
               durationofsleep = sleep,              # Sleeping hours
               stress = stress,             # Stress
               osteoarthritis = OA,              # OA
               rheumatoidarthritis = RA,              # RA
               sittingtime = sitting,          # Sitting hours
               chronicLBP = LBP)              # Chronic low back pain

LBP_cor <- cor(raw_data)
round(LBP_cor, 2)

library(corrplot)
corrplot(LBP_cor)
col <- colorRampPalette(c("#EC592E", "#E8E800", "#289BF0", "#BA1EFF", "#1EA316"))

tiff("Correlation.tiff", units="in", width=10, height=10, res=300)

corrplot(LBP_cor,
         method = "color",
         type = "lower",
         order = "original",
         addCoef.col = "black",
         number.cex = 0.8,
         tl.col = "black",
         tl.cex = 0.7,
         tl.srt = 45,
         diag = F)

dev.off()
