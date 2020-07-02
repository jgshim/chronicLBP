#---- KNHANES data 분석 준비하기 ----

# 1. 데이터 준비하기

setwd("C:/Users/jaege/Desktop/LBP")

# 2. 패키지 설치 및 로드하기

library(foreign)      # SPSS 파일 불러오기
library(dplyr)        # 전처리
library(ggplot2)      # 시각화
library(readxl)       # 엑셀 파일 불러오기
library(caret)
library(DMwR)

# 3. 데이터 불러오기

raw_hn15 <- read.spss(file = "C:/Users/jaege/Desktop/LBP/2015/hn15_all.sav",
                      to.data.frame = T)

raw_hn14 <- read.spss(file = "C:/Users/jaege/Desktop/LBP/2014/hn14_all.sav",
                      to.data.frame = T)

# 복사본 만들기
hn15 <- raw_hn15
hn14 <- raw_hn14

# total 6119명, 2014년 2969명, 2015년 3150명

# 4. 데이터 검토하기

head(hn15)
tail(hn15)
View(hn15)
dim(hn15)
str(hn15)
summary(hn15)

hn15 <- rename(hn15,
               age = age,                # 나이
               height = HE_ht,           # 키
               weight = HE_wt,           # 체중
               bmi = HE_BMI,             # BMI
               fbg = HE_glu,             # Fasting blood glucose
               sex = sex,                # 성별
               dyslipidemia = HE_HCHOL,  # Dyslipidemia
               htn = HE_HP,              # HTN
               dm = HE_DM,               # DM
               ihd = DI4_dg,             # IHD
               svd = DI3_dg,             # SVD
               depression = DF2_dg,      # Depression
               smoking = BS1_1,          # Smoking status
               alcohol = BD1,            # Alcohol
               marriage = marri_1,       # Marriage
               occupation = occp,        # Occupation
               income = ho_incm,         # Household income
               education = edu,          # Education
               activity = BE3_81,        # Physical activity
               sleep = BP8,              # Sleeping hours
               stress = BP1,             # Stress
               OA = DM2_dg,              # OA
               RA = DM3_dg,              # RA
               sitting = BE8_1,          # Sitting hours
               LBP = D_8_4)              # Chronic low back pain

head(hn14)

hn14 <- rename(hn14,
               age = age,                # 나이
               height = HE_ht,           # 키
               weight = HE_wt,           # 체중
               bmi = HE_BMI,             # BMI
               fbg = HE_glu,             # Fasting blood glucose
               sex = sex,                # 성별
               dyslipidemia = HE_HCHOL,  # Dyslipidemia
               htn = HE_HP,              # HTN
               dm = HE_DM,               # DM
               ihd = DI4_dg,             # IHD
               svd = DI3_dg,             # SVD
               depression = DF2_dg,      # Depression
               smoking = BS1_1,          # Smoking status
               alcohol = BD1,            # Alcohol
               marriage = marri_1,       # Marriage
               occupation = occp,        # Occupation
               income = ho_incm,         # Household income
               education = edu,          # Education
               activity = BE3_81,        # Physical activity
               sleep = BP8,              # Sleeping hours
               stress = BP1,             # Stress
               OA = DM2_dg,              # OA
               RA = DM3_dg,              # RA
               sitting = BE8_1,          # Sitting hours
               LBP = D_8_4)              # Chronic low back pain


# 1단계. 변수 검토 및 전처리
# 변수의 특성을 파악하고 이상치를 정제한 다음 파생변수를 만든다.
# 2단계. 변수 간 관계 분석
# 데이터를 요약한 표를 만든 후 분석 결과를 쉽게 이해할 수 있는 그래프를 만든다.

hn15 <- hn15 %>% 
  select(sex, age, height, weight, bmi, fbg, dyslipidemia, htn, dm, ihd, svd, depression,
         smoking, alcohol, marriage, occupation, income, education, activity, sleep,
         OA, RA, sitting, stress, LBP)

hn14 <- hn14 %>% 
  select(sex, age, height, weight, bmi, fbg, dyslipidemia, htn, dm, ihd, svd, depression,
         smoking, alcohol, marriage, occupation, income, education, activity, sleep,
         OA, RA, sitting, stress, LBP)

#---- 2015년 데이터 전처리 ----

# 1. LBP, 7380명 중 50세 이상(LBP study)인 3150명만 추출

class(hn15$LBP)  # 1. 있음 2. 없음 8. 비해당(만50세미만) 9. 모름, 무응답
table(hn15$LBP)
hn15 <- hn15 %>% 
  filter(LBP != 8) # 2015년 3150명

# 이상치 결측 처리
hn15$LBP <- ifelse(hn15$LBP == 9, NA, hn15$LBP)
hn15$LBP <- ifelse(hn15$LBP == 2, 0, 1)              # 0. 없음 1. 있음
table(hn15$LBP)
table(!is.na(hn15$LBP))  # 275/2875 cases missing

# 2. sex
class(hn15$sex)
table(is.na(hn15$sex))
table(hn15$sex)  # 1. 남자 2. 여자
hn15$sex <- ifelse(hn15$sex == 2, 0, hn15$sex)
table(hn15$sex) # 0. 여자 1. 남자     missing value 없음

# 3. age
class(hn15$age)
summary(hn15$age)
qplot(hn15$age)
table(is.na(hn15$age))  # missing value 없음

# 4. height
class(hn15$height)
summary(hn15$height)
qplot(hn15$height)
table(is.na(hn15$height))  # 7/3150 cases missing

# 5. weight
class(hn15$weight)
summary(hn15$weight)
qplot(hn15$weight)
table(is.na(hn15$weight))  # 3/3150 cases missing

# 6. bmi
class(hn15$bmi)
summary(hn15$bmi)
qplot(hn15$bmi)
table(is.na(hn15$bmi))  # 7/3150 cases missing

# 7. fbg
class(hn15$fbg)
summary(hn15$fbg)
qplot(hn15$fbg)
table(is.na(hn15$fbg))  # 241/3150 cases missing

# 8. dyslipidemia
class(hn15$dyslipidemia)
table(hn15$dyslipidemia)  # 0. 없음 1. 있음
qplot(hn15$dyslipidemia)
table(is.na(hn15$dyslipidemia)) # 498/3150 cases missing

# 9. htn
class(hn15$htn)
table(hn15$htn)  # 1. 정상 2. 고혈압 전단계 3. 고혈압
hn15$htn <- ifelse(hn15$htn == 3, 1, 0)
table(hn15$htn)  # 0. 없음 1. 있음
qplot(hn15$htn)
table(is.na(hn15$htn)) # 214/3150 cases missing

# 10. dm
class(hn15$dm)
table(hn15$dm)  # 1. 정상 2. 공복혈당장애 3. 당뇨병
hn15$dm <- ifelse(hn15$dm == 3, 1, 0)
table(hn15$dm)  # 0. 없음 1. 있음
qplot(hn15$dm)
table(is.na(hn15$dm)) # 499/3150 cases missing

# 11. ihd
class(hn15$ihd)
table(hn15$ihd)  # 0. 없음 1. 있음
qplot(hn15$ihd)
table(is.na(hn15$ihd))  # 262/2888 cases missing

# 12. svd
class(hn15$svd)
table(hn15$svd)  # 0. 없음 1. 있음 9. 모름, 무응답
qplot(hn15$svd)
hn15$svd <- ifelse(hn15$svd == 9, NA, hn15$svd)
table(is.na(hn15$svd))  # 259/3150 cases missing

# 13. depression
class(hn15$depression)
table(hn15$depression)  # 0. 없음 1. 있음 9. 모름, 무응답
qplot(hn15$depression)
hn15$depression <- ifelse(hn15$depression == 9, NA, hn15$depression)
table(is.na(hn15$depression))  # 267/3150 cases missing

# 14. smoking
class(hn15$smoking)
table(hn15$smoking)  # 1. 5갑(100개비) 미만 2. 5갑(100개비) 이상 3. 피운 적 없음 9. 모름, 무응답
qplot(hn15$smoking)
hn15$smoking <- ifelse(hn15$smoking == 9, NA,
                       ifelse(hn15$smoking == 3, 0, 1))
table(hn15$smoking)   # 0. 담배 피운 적 없음 1. 담배 피운 적 있음
table(is.na(hn15$smoking))  # 122/3150 cases missing

# 15. alcohol
class(hn15$alcohol)
table(hn15$alcohol)  # 1. 술을 마셔 본 적 없음 2. 있음 9. 모름, 무응답
table(is.na(hn15$alcohol))
qplot(hn15$alcohol)
hn15$alcohol <- ifelse(hn15$alcohol == 9, NA, hn15$alcohol)  # 1. 없음 2. 음주
hn15$alcohol <- ifelse(hn15$alcohol == 2, 1, 0)              # 0. 없음 1. 음주
table(is.na(hn15$alcohol))  # 115/3150 cases missing

# 16. marriage
class(hn15$marriage)
table(hn15$marriage)  # 1. 기혼 2. 미혼
table(is.na(hn15$marriage))
hn15$marriage <- ifelse(hn15$marriage == 2, 0, 1)            # 0. 미혼 1. 기혼
qplot(hn15$marriage)
table(is.na(hn15$marriage)) # missing value 없음

# 17. occupation
class(hn15$occupation)
table(hn15$occupation) # 7가지로 분류
qplot(hn15$occupation)
table(is.na(hn15$occupation))  # 298/3150 cases missing

# 18. income
class(hn15$income)
table(hn15$income) # 4가지로 분류 1.하 2.중하 3.중상 4.상
qplot(hn15$income)
table(hn15$income) # 
hn15$income <- ifelse(hn15$income >=3, 1, 0)  # 0. 하/중하 1. 중상/상
table(is.na(hn15$income))  # 28/3150 cases missing 

# 19. education
class(hn15$education)
table(hn15$education) # 4가지로 분류
qplot(hn15$education)
hn15$education <- ifelse(hn15$education >3, 1, 0)  # 0. 고졸이하 1. 대졸이상
table(is.na(hn15$education))  # 300/3150 cases missing

# 20. activity
class(hn15$activity)
table(hn15$activity) # 최소 10분 이상 계속 숨이 약간 차거나 심장이 약간 빠르게 뛰는 정도
qplot(hn15$activity)
hn15$activity <- ifelse(hn15$activity == 9, NA,
                        ifelse(hn15$activity == 2, 0, hn15$activity))  # 0. 아니오 1. 예
table(is.na(hn15$activity))  # 292/3150 cases missing

# 21. sleep
class(hn15$sleep)
table(hn15$sleep)
summary(hn15$sleep)
qplot(hn15$sleep)
hn15$sleep <- ifelse(hn15$sleep == 99, NA, hn15$sleep)
hn15$sleep <- ifelse(hn15$sleep >= 7, 1, 0)  # 0. 7시간 미만 1. 7시간 이상 
table(is.na(hn15$sleep))  # 125/3150 cases missing

# 22. OA
class(hn15$OA)
table(hn15$OA)  # 0. 없음 1. 있음 9. 모름, 무응답
hn15$OA <- ifelse(hn15$OA == 9, NA, hn15$OA)
qplot(hn15$OA)
table(is.na(hn15$OA))  # 264/3150 cases missing

# 23. RA
class(hn15$RA)
table(hn15$RA)  # 0. 없음 1. 있음 9. 모름, 무응답
hn15$RA <- ifelse(hn15$RA == 9, NA, hn15$RA)
qplot(hn15$RA)
table(is.na(hn15$RA))  # 265/3150 cases missing

# 24. sitting
class(hn15$sitting)
table(hn15$sitting)
hn15$sitting <- ifelse(hn15$sitting == 99, NA, hn15$sitting)
summary(hn15$sitting)
hn15$sitting <- ifelse(hn15$sitting >= 8, 1, 0)  # 0. 8시간 미만 1. 8시간 이상
qplot(hn15$sitting)
table(is.na(hn15$sitting))  # 394/3150 cases missing

# # 25. stress
class(hn15$stress)
table(hn15$stress)
hn15$stress <- ifelse(hn15$stress == 9, NA, hn15$stress)
hn15$stress <- ifelse(hn15$stress > 3, 0, 1)  # 1. 대단히 많이 느낀다 2. 많이 느끼는 편이다 3. 조금 느끼는 편이다 4. 거의 느끼지 않는다
qplot(hn15$stress)
table(is.na(hn15$stress))  # 122/3150 cases missing

# occupation 7가지 분류로 one-hot encoding
hn15$occupation <- as.factor(hn15$occupation)
dummy <- dummyVars(" ~ .", data=hn15)
hn15 <- data.frame(predict(dummy, newdata = hn15)) 

knnOutput_15 <- knnImputation(hn15[, !names(hn15) %in% "medv"])  # perform knn imputation.
anyNA(knnOutput_15)

hn15 <- knnOutput_15

hn15$LBP <- ifelse(hn15$LBP >= 0.5, 1, 0)
hn15$dm <- ifelse(hn15$dm >= 0.5, 1, 0)
hn15$htn <- ifelse(hn15$htn >= 0.5, 1, 0)
hn15$ihd <- ifelse(hn15$ihd >= 0.5, 1, 0)
hn15$svd <- ifelse(hn15$svd >= 0.5, 1, 0)
hn15$smoking <- ifelse(hn15$smoking >= 0.5, 1, 0)
hn15$depression <- ifelse(hn15$depression >= 0.5, 1, 0)
hn15$alcohol <- ifelse(hn15$alcohol >= 0.5, 1, 0)
hn15$education <- ifelse(hn15$education >= 0.5, 1, 0)
hn15$activity <- ifelse(hn15$activity >= 0.5, 1, 0)
hn15$sleep <- ifelse(hn15$sleep >= 0.5, 1, 0)
hn15$OA <- ifelse(hn15$OA >= 0.5, 1, 0)
hn15$RA <- ifelse(hn15$RA >= 0.5, 1, 0)
hn15$sitting <- ifelse(hn15$sitting >= 0.5, 1, 0)
hn15$income <- ifelse(hn15$income >= 0.5, 1, 0)
hn15$dyslipidemia <- ifelse(hn15$dyslipidemia >= 0.5, 1, 0)
hn15$stress <- ifelse(hn15$stress >= 0.5, 1, 0)
hn15$occupation.1 <- ifelse(hn15$occupation.1 >= 0.5, 1, 0)
hn15$occupation.2 <- ifelse(hn15$occupation.2 >= 0.5, 1, 0)
hn15$occupation.3 <- ifelse(hn15$occupation.3 >= 0.5, 1, 0)
hn15$occupation.4 <- ifelse(hn15$occupation.4 >= 0.5, 1, 0)
hn15$occupation.5 <- ifelse(hn15$occupation.5 >= 0.5, 1, 0)
hn15$occupation.6 <- ifelse(hn15$occupation.6 >= 0.5, 1, 0)
hn15$occupation.7 <- ifelse(hn15$occupation.7 >= 0.5, 1, 0)
str(hn15)


#---- 2014년 데이터 전처리 ----

# 1. LBP, 7550명 중 50세 이상(LBP study)인 2969명만 추출
class(hn14$LBP)  # 1. 있음 2. 없음 9. 모름, 무응답
table(hn14$LBP)
hn14 <- hn14 %>% 
  filter(LBP != 8)

# 이상치 결측 처리
hn14$LBP <- ifelse(hn14$LBP == 9, NA, hn14$LBP)
hn14$LBP <- ifelse(hn14$LBP == 2, 0, 1)              # 0. 없음 1. 있음
table(hn14$LBP)
table(is.na(hn14$LBP))  # 237/2969 cases missing

# 2. sex
class(hn14$sex)
table(is.na(hn14$sex))
table(hn14$sex)  # 1. 남자 2. 여자
hn14$sex <- ifelse(hn14$sex == 2, 0, hn14$sex)
table(is.na(hn14$sex))  #  # 0. 여자 1. 남자     missing value 없음
qplot(hn14$sex)

# 3. age
class(hn14$age)
summary(hn14$age)
qplot(hn14$age)
table(is.na(hn14$age))  # missing value 없음

# 4. height
class(hn14$height)
summary(hn14$height)
qplot(hn14$height)
table(is.na(hn14$height))  # 5/2969 cases missing

# 5. weight
class(hn14$weight)
summary(hn14$weight)
qplot(hn14$weight)
table(is.na(hn14$weight))  # 5/2969 cases missing

# 6. bmi
class(hn14$bmi)
summary(hn14$bmi)
qplot(hn14$bmi)
table(is.na(hn14$bmi))  # 5/2969 cases missing

# 7. fbg
class(hn14$fbg)
summary(hn14$fbg)
qplot(hn14$fbg)
table(is.na(hn14$fbg))  # 405/405 cases missing

# 8. dyslipidemia
class(hn14$dyslipidemia)
table(hn14$dyslipidemia)  # 0. 없음 1. 있음
qplot(hn14$dyslipidemia)
table(is.na(hn14$dyslipidemia)) # 614/2969 cases missing

# 9. htn
class(hn14$htn)
table(hn14$htn)  # 1. 정상 2. 고혈압 전단계 3. 고혈압
hn14$htn <- ifelse(hn14$htn == 3, 1, 0)
table(hn14$htn)  # 0. 없음 1. 있음
qplot(hn14$htn)
table(is.na(hn14$htn)) # 228/2969 cases missing

# 10. dm
class(hn14$dm)
table(hn14$dm)  # 1. 정상 2. 공복혈당장애 3. 당뇨병
hn14$dm <- ifelse(hn14$dm == 3, 1, 0)
table(hn14$dm)  # 0. 없음 1. 있음
qplot(hn14$dm)
table(is.na(hn14$dm)) # 616/2969 cases missing

# 11. ihd
class(hn14$ihd)
table(hn14$ihd)  # 0. 없음 1. 있음
qplot(hn14$ihd)
table(is.na(hn14$ihd))  # 230/2969 cases missing

# 12. svd
class(hn14$svd)
table(hn14$svd)  # 0. 없음 1. 있음 9. 모름, 무응답
qplot(hn14$svd)
hn14$svd <- ifelse(hn14$svd == 9, NA, hn14$svd)
table(is.na(hn14$svd))  # 228/2969 cases missing

# 13. depression
class(hn14$depression)
table(hn14$depression)  # 0. 없음 1. 있음 9. 모름, 무응답
qplot(hn14$depression)
hn14$depression <- ifelse(hn14$depression == 9, NA, hn14$depression)
table(is.na(hn14$depression))  # 231/2969 cases missing

# 14. smoking
class(hn14$smoking)
table(hn14$smoking)  # 1. 5갑(100개비) 미만 2. 5갑(100개비) 이상 3. 피운 적 없음 9. 모름, 무응답
qplot(hn14$smoking)
hn14$smoking <- ifelse(hn14$smoking == 9, NA,
                       ifelse(hn14$smoking == 3, 0, 1))
table(hn14$smoking)   # 0. 담배 피운 적 없음 1. 담배 피운 적 있음
table(is.na(hn14$smoking))  # 176/2969 cases missing

# 15. alcohol
class(hn14$alcohol)
table(hn14$alcohol)  # 1. 술을 마셔 본 적 없음 2. 있음 9. 모름, 무응답
table(is.na(hn14$alcohol))
qplot(hn14$alcohol)
hn14$alcohol <- ifelse(hn14$alcohol == 9, NA, hn14$alcohol)  # 1. 없음 2. 음주
hn14$alcohol <- ifelse(hn14$alcohol == 2, 1, 0)              # 0. 없음 1. 음주
table(is.na(hn14$alcohol))  # 168/2969 cases missing

# 16. marriage
class(hn14$marriage)
table(hn14$marriage)  # 1. 기혼 2. 미혼
table(is.na(hn14$marriage))
hn14$marriage <- ifelse(hn14$marriage == 2, 0, 1)            # 0. 미혼 1. 기혼
qplot(hn14$marriage)
table(is.na(hn14$marriage))  # missing value 없음

# 17. occupation
class(hn14$occupation)
table(hn14$occupation) # 7가지로 분류
qplot(hn14$occupation)
table(is.na(hn14$occupation))  # 273/2969 cases missing

# 18. income
class(hn14$income)
table(hn14$income) # 4가지로 분류
qplot(hn14$income)
hn14$income <- ifelse(hn14$income >=3, 1, 0)  # 0. 하/중하 1. 중상/상
table(is.na(hn14$income))  # 15/2969 cases missing

# 19. education
class(hn14$education)
table(hn14$education) # 4가지로 분류
qplot(hn14$education)
hn14$education <- ifelse(hn14$education >3, 1, 0)  # 0. 고졸이하 1. 대졸이상
table(is.na(hn14$education))  # 273/2969 cases missing

# 20. activity
class(hn14$activity)
table(hn14$activity) 
qplot(hn14$activity)
hn14$activity <- ifelse(hn14$activity == 9, NA,
                        ifelse(hn14$activity == 2, 0, hn14$activity))  # 0. 아니오 1. 예
table(is.na(hn14$activity))  # 263/2969 cases missing

# 21. sleep
class(hn14$sleep)
table(hn14$sleep)
summary(hn14$sleep)
qplot(hn14$sleep)
hn14$sleep <- ifelse(hn14$sleep == 99, NA, hn14$sleep)
hn14$sleep <- ifelse(hn14$sleep >= 7, 1, 0)  # 0. 7시간 미만 1. 7시간 이상 
table(is.na(hn14$sleep))  # 179/2969 cases missing

# 22. OA
class(hn14$OA)
table(hn14$OA)  # 0. 없음 1. 있음 9. 모름, 무응답
hn14$OA <- ifelse(hn14$OA == 9, NA, hn14$OA)
qplot(hn14$OA)
table(is.na(hn14$OA))  # 230/2969 cases missing

# 23. RA
class(hn14$RA)
table(hn14$RA)  # 0. 없음 1. 있음 9. 모름, 무응답
hn14$RA <- ifelse(hn14$RA == 9, NA, hn14$RA)
qplot(hn14$RA)
table(is.na(hn14$RA))  # 231/2969 cases missing

# 24. sitting
class(hn14$sitting)
table(hn14$sitting)
hn14$sitting <- ifelse(hn14$sitting == 99, NA, hn14$sitting)
summary(hn14$sitting)
hn14$sitting <- ifelse(hn14$sitting >= 7, 1, 0)  # 0. 8시간 미만 1. 8시간 이상
qplot(hn14$sitting)
table(is.na(hn14$sitting))  # 329/2969 cases missing

# # 25. stress
class(hn14$stress)
table(hn14$stress)
hn14$stress <- ifelse(hn14$stress == 9, NA, hn14$stress)
hn14$stress <- ifelse(hn14$stress > 3, 0, 1)  # 1. 대단히 많이 느낀다 2. 많이 느끼는 편이다 3. 조금 느끼는 편이다 4. 거의 느끼지 않는다
qplot(hn14$stress)
table(is.na(hn14$stress))  # 179/2969 cases missing

# occupation 7가지 분류로 one-hot encoding
hn14$occupation <- as.factor(hn14$occupation)
dummy <- dummyVars(" ~ .", data=hn14)
hn14 <- data.frame(predict(dummy, newdata = hn14))

knnOutput_14 <- knnImputation(hn14[, !names(hn14) %in% "medv"])  # perform knn imputation.
anyNA(knnOutput_14)

hn14 <- knnOutput_14

hn14$LBP <- ifelse(hn14$LBP >= 0.5, 1, 0)
hn14$sex <- ifelse(hn14$sex >= 0.5, 1, 0)
hn14$dyslipidemia <- ifelse(hn14$dyslipidemia >= 0.5, 1, 0)
hn14$htn <- ifelse(hn14$htn >= 0.5, 1, 0)
hn14$dm <- ifelse(hn14$dm >= 0.5, 1, 0)
hn14$ihd <- ifelse(hn14$ihd >= 0.5, 1, 0)
hn14$svd <- ifelse(hn14$svd >= 0.5, 1, 0)
hn14$smoking <- ifelse(hn14$smoking >= 0.5, 1, 0)
hn14$depression <- ifelse(hn14$depression >= 0.5, 1, 0)
hn14$alcohol <- ifelse(hn14$alcohol >= 0.5, 1, 0)
hn14$marriage <- ifelse(hn14$marriage >= 0.5, 1, 0)
hn14$education <- ifelse(hn14$education >= 0.5, 1, 0)
hn14$activity <- ifelse(hn14$activity >= 0.5, 1, 0)
hn14$sleep <- ifelse(hn14$sleep >= 0.5, 1, 0)
hn14$OA <- ifelse(hn14$OA >= 0.5, 1, 0)
hn14$RA <- ifelse(hn14$RA >= 0.5, 1, 0)
hn14$sitting <- ifelse(hn14$sitting >= 0.5, 1, 0)
hn14$income <- ifelse(hn14$income >= 0.5, 1, 0)
hn14$stress <- ifelse(hn14$stress >= 0.5, 1, 0)
hn14$dyslipidemia <- ifelse(hn14$dyslipidemia >= 0.5, 1, 0)
hn14$occupation.1 <- ifelse(hn14$occupation.1 >= 0.5, 1, 0)
hn14$occupation.2 <- ifelse(hn14$occupation.2 >= 0.5, 1, 0)
hn14$occupation.3 <- ifelse(hn14$occupation.3 >= 0.5, 1, 0)
hn14$occupation.4 <- ifelse(hn14$occupation.4 >= 0.5, 1, 0)
hn14$occupation.5 <- ifelse(hn14$occupation.5 >= 0.5, 1, 0)
hn14$occupation.6 <- ifelse(hn14$occupation.6 >= 0.5, 1, 0)
hn14$occupation.7 <- ifelse(hn14$occupation.7 >= 0.5, 1, 0)


# 2014년과 2015년 data 통합 ----

hn1415 <- bind_rows(hn14, hn15)

str(hn1415)

table(hn1415$sex)
table(hn1415$dyslipidemia)
table(hn1415$htn)
table(hn1415$dm)
table(hn1415$ihd)
table(hn1415$svd)
table(hn1415$depression)
table(hn1415$smoking)
table(hn1415$alcohol)
table(hn1415$marriage)
table(hn1415$occupation.1)
table(hn1415$occupation.2)
table(hn1415$occupation.3)
table(hn1415$occupation.4)
table(hn1415$occupation.5)
table(hn1415$occupation.6)
table(hn1415$occupation.7)
table(hn1415$income)
table(hn1415$education)
table(hn1415$activity)
table(hn1415$sleep)
table(hn1415$OA)
table(hn1415$RA)
table(hn1415$sitting)
table(hn1415$stress)
table(hn1415$LBP)

write.csv(hn1415, file="hn1415.csv", row.names = TRUE)


###########################################
## !!! Data scaling & 범주특성의 변환 ## ----
###########################################

# 1. 2014년, 2015년 데이터 범주(categorical data)/연속(continuous data)/label 분류

data_cat <- hn1415 %>% 
  select(sex, dyslipidemia, htn, dm, ihd, svd, depression, smoking, alcohol, marriage, occupation.1, occupation.2, occupation.3, occupation.4, occupation.5, occupation.6, occupation.7, income, education, activity, sleep, OA, RA, sitting, stress)
data_num <- hn1415 %>% 
  select(age, height, weight, bmi, fbg)
data_class <- hn1415 %>% 
  select(LBP)

# 2. 연속형 특성의 Scaling

# 2-1. 표준화(평균 0, 표준편차 1) scaling

StandardScale <- preProcess(data_num, method=c("center", "scale"))
print(StandardScale)
data_standard <- predict(StandardScale, data_num)
head(data_standard)

# 2-2. min-max scaling

MinMaxScale <- preProcess(data_num, method=c("range"))
print(MinMaxScale)
data_minmax <- predict(MinMaxScale, data_num)
head(data_minmax)


# 3. 데이터 통합 및 저장

# cbind로 column 데이터를 추가해준다. cbind 외에도 여러가지 방법으로 같은 작업이 가능하다.
hn1415_data = cbind(data_cat, data_standard, data_class)

write.csv(hn1415_data, file="hn1415_p.csv", row.names = TRUE)

# Demographic 통계 분석
# ---- Data Set Population Characteristics and Characteristics of Patients Who Developed and Did Not Developed Chronic LBP ----

# Features: age, bmi, fbg, sex, dyslipidemia, htn, dm, ihd, svd, depression, Depression,
# smoking, alcohol, marriage, occupation, income, education, activity, sleep
# stress, OA, RA, sitting, LBP

# 1. LBP
# 0. 없음 1. 있음

hn1415_noLBP <- hn1415 %>% filter(LBP==0)
hn1415_LBP <- hn1415 %>% filter(LBP==1)

# 2. sex
# 0. 여자 1. 남자

table(hn1415$sex)
qplot(hn1415$sex, bins = 20)
df_sex <- hn1415 %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

table(hn1415_noLBP$sex)
qplot(hn1415_noLBP$sex, bins = 20)
df_sex<- hn1415_noLBP %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

table(hn1415_LBP$sex)
qplot(hn1415_LBP$sex, bins = 20)
df_sex<- hn1415_LBP %>% group_by(sex) %>% summarise(total = n())
df_sex.1 <- df_sex %>% mutate(percent = total / sum(total))
df_sex.1

sex_LBP_cross <- xtabs(~ sex + LBP, data = hn1415)
sex_LBP_cross
chisq.test(sex_LBP_cross)

# 3. age

table(hn1415$age)
table(is.na(hn1415$age))
hist(hn1415$age)
summary(hn1415$age)

table(hn1415_noLBP$age)
table(is.na(hn1415_noLBP$age))
hist(hn1415_noLBP$age)
summary(hn1415_noLBP$age)

table(hn1415_LBP$age)
table(is.na(hn1415_LBP$age))
hist(hn1415_LBP$age)
summary(hn1415_LBP$age)

boxplot(age ~ LBP, data = hn1415, col = c("brown2", "deepskyblue"))
var.test(age ~ LBP, data = hn1415)
t.test(age ~ LBP, data = hn1415, var.equal=TRUE)

# 6. bmi

hist(hn1415$bmi)
summary(hn1415$bmi)

table(hn1415_noLBP$bmi)
table(is.na(hn1415_noLBP$bmi))
hist(hn1415_noLBP$bmi)
summary(hn1415_noLBP$bmi)

table(hn1415_LBP$bmi)
table(is.na(hn1415_LBP$bmi))
hist(hn1415_LBP$bmi)
summary(hn1415_LBP$bmi)

boxplot(bmi ~ LBP, data = hn1415, col = c("brown2", "deepskyblue"))
var.test(bmi ~ LBP, data = hn1415)
t.test(bmi ~ LBP, data = hn1415, var.equal=TRUE)

# 7. fbg

hist(hn1415$fbg)
summary(hn1415$fbg)

table(hn1415_noLBP$fbg)
table(is.na(hn1415_noLBP$fbg))
hist(hn1415_noLBP$fbg)
summary(hn1415_noLBP$fbg)

table(hn1415_LBP$fbg)
table(is.na(hn1415_LBP$fbg))
hist(hn1415_LBP$fbg)
summary(hn1415_LBP$fbg)

boxplot(fbg ~ LBP, data = hn1415, col = c("brown2", "deepskyblue"))
var.test(fbg ~ LBP, data = hn1415)
t.test(fbg ~ LBP, data = hn1415, var.equal=TRUE)

# 8. dyslipidemia

table(hn1415$dyslipidemia)
qplot(hn1415$dyslipidemia, bins = 20)
df_dyslipidemia <- hn1415 %>% group_by(dyslipidemia) %>% summarise(total = n())
df_dyslipidemia.1 <- df_dyslipidemia %>% mutate(percent = total / sum(total))
df_dyslipidemia.1

table(hn1415_noLBP$dyslipidemia)
qplot(hn1415_noLBP$dyslipidemia, bins = 20)
df_dyslipidemia <- hn1415_noLBP %>% group_by(dyslipidemia) %>% summarise(total = n())
df_dyslipidemia.1 <- df_dyslipidemia %>% mutate(percent = total / sum(total))
df_dyslipidemia.1

table(hn1415_LBP$dyslipidemia)
qplot(hn1415_LBP$dyslipidemia, bins = 20)
df_dyslipidemia <- hn1415_LBP %>% group_by(dyslipidemia) %>% summarise(total = n())
df_dyslipidemia.1 <- df_dyslipidemia %>% mutate(percent = total / sum(total))
df_dyslipidemia.1

dyslipidemia_LBP_cross <- xtabs(~ dyslipidemia + LBP, data = hn1415)
dyslipidemia_LBP_cross
chisq.test(dyslipidemia_LBP_cross)

# 9. htn

table(hn1415$htn)
qplot(hn1415$htn, bins = 20)
df_htn <- hn1415 %>% group_by(htn) %>% summarise(total = n())
df_htn.1 <- df_htn %>% mutate(percent = total / sum(total))
df_htn.1

table(hn1415_noLBP$htn)
qplot(hn1415_noLBP$htn, bins = 20)
df_htn <- hn1415_noLBP %>% group_by(htn) %>% summarise(total = n())
df_htn.1 <- df_htn %>% mutate(percent = total / sum(total))
df_htn.1

table(hn1415_LBP$htn)
qplot(hn1415_LBP$htn, bins = 20)
df_htn <- hn1415_LBP %>% group_by(htn) %>% summarise(total = n())
df_htn.1 <- df_htn %>% mutate(percent = total / sum(total))
df_htn.1

htn_LBP_cross <- xtabs(~ htn + LBP, data = hn1415)
htn_LBP_cross
chisq.test(htn_LBP_cross)

# 10. dm

table(hn1415$dm)
qplot(hn1415$dm, bins = 20)
df_dm <- hn1415 %>% group_by(dm) %>% summarise(total = n())
df_dm.1 <- df_dm %>% mutate(percent = total / sum(total))
df_dm.1

table(hn1415_noLBP$dm)
qplot(hn1415_noLBP$dm, bins = 20)
df_dm <- hn1415_noLBP %>% group_by(dm) %>% summarise(total = n())
df_dm.1 <- df_dm %>% mutate(percent = total / sum(total))
df_dm.1

table(hn1415_LBP$dm)
qplot(hn1415_LBP$dm, bins = 20)
df_dm <- hn1415_LBP %>% group_by(dm) %>% summarise(total = n())
df_dm.1 <- df_dm %>% mutate(percent = total / sum(total))
df_dm.1

dm_LBP_cross <- xtabs(~ dm + LBP, data = hn1415)
dm_LBP_cross
chisq.test(dm_LBP_cross)

# 11. Ischemic heart disease

table(hn1415$ihd)
qplot(hn1415$ihd, bins = 20)
df_ihd <- hn1415 %>% group_by(ihd) %>% summarise(total = n())
df_ihd.1 <- df_ihd %>% mutate(percent = total / sum(total))
df_ihd.1

table(hn1415_noLBP$ihd)
qplot(hn1415_noLBP$ihd, bins = 20)
df_ihd <- hn1415_noLBP %>% group_by(ihd) %>% summarise(total = n())
df_ihd.1 <- df_ihd %>% mutate(percent = total / sum(total))
df_ihd.1

table(hn1415_LBP$ihd)
qplot(hn1415_LBP$ihd, bins = 20)
df_ihd <- hn1415_LBP %>% group_by(ihd) %>% summarise(total = n())
df_ihd.1 <- df_ihd %>% mutate(percent = total / sum(total))
df_ihd.1

ihd_LBP_cross <- xtabs(~ ihd + LBP, data = hn1415)
ihd_LBP_cross
chisq.test(ihd_LBP_cross)

# 12. Cerebrovascular accident

table(hn1415$svd)
qplot(hn1415$svd, bins = 20)
df_svd <- hn1415 %>% group_by(svd) %>% summarise(total = n())
df_svd.1 <- df_svd %>% mutate(percent = total / sum(total))
df_svd.1

table(hn1415_noLBP$svd)
qplot(hn1415_noLBP$svd, bins = 20)
df_svd <- hn1415_noLBP %>% group_by(svd) %>% summarise(total = n())
df_svd.1 <- df_svd %>% mutate(percent = total / sum(total))
df_svd.1

table(hn1415_LBP$svd)
qplot(hn1415_LBP$svd, bins = 20)
df_svd <- hn1415_LBP %>% group_by(svd) %>% summarise(total = n())
df_svd.1 <- df_svd %>% mutate(percent = total / sum(total))
df_svd.1

svd_LBP_cross <- xtabs(~ svd + LBP, data = hn1415)
svd_LBP_cross
chisq.test(svd_LBP_cross)

# 13. Depressive symptom

table(hn1415$depression)
qplot(hn1415$depression, bins = 20)
df_depression <- hn1415 %>% group_by(depression) %>% summarise(total = n())
df_depression.1 <- df_depression %>% mutate(percent = total / sum(total))
df_depression.1

table(hn1415_noLBP$depression)
qplot(hn1415_noLBP$depression, bins = 20)
df_depression <- hn1415_noLBP %>% group_by(depression) %>% summarise(total = n())
df_depression.1 <- df_depression %>% mutate(percent = total / sum(total))
df_depression.1

table(hn1415_LBP$depression)
qplot(hn1415_LBP$depression, bins = 20)
df_depression <- hn1415_LBP %>% group_by(depression) %>% summarise(total = n())
df_depression.1 <- df_depression %>% mutate(percent = total / sum(total))
df_depression.1

depression_LBP_cross <- xtabs(~ depression + LBP, data = hn1415)
depression_LBP_cross
chisq.test(depression_LBP_cross)

# 14. smoking

table(hn1415$smoking)
qplot(hn1415$smoking, bins = 20)
df_smoking <- hn1415 %>% group_by(smoking) %>% summarise(total = n())
df_smoking.1 <- df_smoking %>% mutate(percent = total / sum(total))
df_smoking.1

table(hn1415_noLBP$smoking)
qplot(hn1415_noLBP$smoking, bins = 20)
df_smoking <- hn1415_noLBP %>% group_by(smoking) %>% summarise(total = n())
df_smoking.1 <- df_smoking %>% mutate(percent = total / sum(total))
df_smoking.1

table(hn1415_LBP$smoking)
qplot(hn1415_LBP$smoking, bins = 20)
df_smoking <- hn1415_LBP %>% group_by(smoking) %>% summarise(total = n())
df_smoking.1 <- df_smoking %>% mutate(percent = total / sum(total))
df_smoking.1

smoking_LBP_cross <- xtabs(~ smoking + LBP, data = hn1415)
smoking_LBP_cross
chisq.test(smoking_LBP_cross)

# 15. Alcohol consumption

table(hn1415$alcohol)
qplot(hn1415$alcohol, bins = 20)
df_alcohol <- hn1415 %>% group_by(alcohol) %>% summarise(total = n())
df_alcohol.1 <- df_alcohol %>% mutate(percent = total / sum(total))
df_alcohol.1

table(hn1415_noLBP$alcohol)
qplot(hn1415_noLBP$alcohol, bins = 20)
df_alcohol <- hn1415_noLBP %>% group_by(alcohol) %>% summarise(total = n())
df_alcohol.1 <- df_alcohol %>% mutate(percent = total / sum(total))
df_alcohol.1

table(hn1415_LBP$alcohol)
qplot(hn1415_LBP$alcohol, bins = 20)
df_alcohol <- hn1415_LBP %>% group_by(alcohol) %>% summarise(total = n())
df_alcohol.1 <- df_alcohol %>% mutate(percent = total / sum(total))
df_alcohol.1

alcohol_LBP_cross <- xtabs(~ alcohol + LBP, data = hn1415)
alcohol_LBP_cross
chisq.test(alcohol_LBP_cross)

# 16. Marital status

table(hn1415$marriage)
qplot(hn1415$marriage, bins = 20)
df_marriage <- hn1415 %>% group_by(marriage) %>% summarise(total = n())
df_marriage.1 <- df_marriage %>% mutate(percent = total / sum(total))
df_marriage.1

table(hn1415_noLBP$marriage)
qplot(hn1415_noLBP$marriage, bins = 20)
df_marriage <- hn1415_noLBP %>% group_by(marriage) %>% summarise(total = n())
df_marriage.1 <- df_marriage %>% mutate(percent = total / sum(total))
df_marriage.1

table(hn1415_LBP$marriage)
qplot(hn1415_LBP$marriage, bins = 20)
df_marriage <- hn1415_LBP %>% group_by(marriage) %>% summarise(total = n())
df_marriage.1 <- df_marriage %>% mutate(percent = total / sum(total))
df_marriage.1

marriage_LBP_cross <- xtabs(~ marriage + LBP, data = hn1415)
marriage_LBP_cross
chisq.test(marriage_LBP_cross)

# 17. Occupation

hn1415 <- mutate(hn1415, Occupation = ifelse(occupation.1==1, 1,
                                    ifelse(occupation.2==1, 2,
                                           ifelse(occupation.3==1, 3,
                                                  ifelse(occupation.4==1, 4,
                                                         ifelse(occupation.5==1, 5,
                                                                ifelse(occupation.6==1, 6,
                                                                       ifelse(occupation.7==1,7,7))))))))

hn1415_noLBP <- hn1415 %>% filter(LBP==0)
hn1415_LBP <- hn1415 %>% filter(LBP==1)

table(hn1415$Occupation)
table(is.na(hn1415$Occupation))
qplot(hn1415$Occupation, bins = 20)
df_Occupation <- hn1415 %>% group_by(Occupation) %>% summarise(total = n())
df_Occupation.1 <- df_Occupation %>% mutate(percent = total / sum(total))
df_Occupation.1

table(hn1415_noLBP$Occupation)
qplot(hn1415_noLBP$Occupation, bins = 20)
df_Occupation <- hn1415_noLBP %>% group_by(Occupation) %>% summarise(total = n())
df_Occupation.1 <- df_Occupation %>% mutate(percent = total / sum(total))
df_Occupation.1

table(hn1415_LBP$Occupation)
qplot(hn1415_LBP$Occupation, bins = 20)
df_Occupation <- hn1415_LBP %>% group_by(Occupation) %>% summarise(total = n())
df_Occupation.1 <- df_Occupation %>% mutate(percent = total / sum(total))
df_Occupation.1

Occupation_LBP_cross <- xtabs(~ Occupation + LBP, data = hn1415)
Occupation_LBP_cross
chisq.test(Occupation_LBP_cross)

# 18. Household income

table(hn1415$income)
qplot(hn1415$income, bins = 20)
df_income <- hn1415 %>% group_by(income) %>% summarise(total = n())
df_income.1 <- df_income %>% mutate(percent = total / sum(total))
df_income.1

table(hn1415_noLBP$income)
qplot(hn1415_noLBP$income, bins = 20)
df_income <- hn1415_noLBP %>% group_by(income) %>% summarise(total = n())
df_income.1 <- df_income %>% mutate(percent = total / sum(total))
df_income.1

table(hn1415_LBP$income)
qplot(hn1415_LBP$income, bins = 20)
df_income <- hn1415_LBP %>% group_by(income) %>% summarise(total = n())
df_income.1 <- df_income %>% mutate(percent = total / sum(total))
df_income.1

income_LBP_cross <- xtabs(~ income + LBP, data = hn1415)
income_LBP_cross
chisq.test(income_LBP_cross)

# 19. Education level

table(hn1415$education)
qplot(hn1415$education, bins = 20)
df_education <- hn1415 %>% group_by(education) %>% summarise(total = n())
df_education.1 <- df_education %>% mutate(percent = total / sum(total))
df_education.1

table(hn1415_noLBP$education)
qplot(hn1415_noLBP$education, bins = 20)
df_education <- hn1415_noLBP %>% group_by(education) %>% summarise(total = n())
df_education.1 <- df_education %>% mutate(percent = total / sum(total))
df_education.1

table(hn1415_LBP$education)
qplot(hn1415_LBP$education, bins = 20)
df_education <- hn1415_LBP %>% group_by(education) %>% summarise(total = n())
df_education.1 <- df_education %>% mutate(percent = total / sum(total))
df_education.1

education_LBP_cross <- xtabs(~ education + LBP, data = hn1415)
education_LBP_cross
chisq.test(education_LBP_cross)

# 20. Physical activity

table(hn1415$activity)
qplot(hn1415$activity, bins = 20)
df_activity <- hn1415 %>% group_by(activity) %>% summarise(total = n())
df_activity.1 <- df_activity %>% mutate(percent = total / sum(total))
df_activity.1

table(hn1415_noLBP$activity)
qplot(hn1415_noLBP$activity, bins = 20)
df_activity <- hn1415_noLBP %>% group_by(activity) %>% summarise(total = n())
df_activity.1 <- df_activity %>% mutate(percent = total / sum(total))
df_activity.1

table(hn1415_LBP$activity)
qplot(hn1415_LBP$activity, bins = 20)
df_activity <- hn1415_LBP %>% group_by(activity) %>% summarise(total = n())
df_activity.1 <- df_activity %>% mutate(percent = total / sum(total))
df_activity.1

activity_LBP_cross <- xtabs(~ activity + LBP, data = hn1415)
activity_LBP_cross
chisq.test(activity_LBP_cross)

# 21. Duration of sleep

table(hn1415$sleep)
qplot(hn1415$sleep, bins = 20)
df_sleep <- hn1415 %>% group_by(sleep) %>% summarise(total = n())
df_sleep.1 <- df_sleep %>% mutate(percent = total / sum(total))
df_sleep.1

table(hn1415_noLBP$sleep)
qplot(hn1415_noLBP$sleep, bins = 20)
df_sleep <- hn1415_noLBP %>% group_by(sleep) %>% summarise(total = n())
df_sleep.1 <- df_sleep %>% mutate(percent = total / sum(total))
df_sleep.1

table(hn1415_LBP$sleep)
qplot(hn1415_LBP$sleep, bins = 20)
df_sleep <- hn1415_LBP %>% group_by(sleep) %>% summarise(total = n())
df_sleep.1 <- df_sleep %>% mutate(percent = total / sum(total))
df_sleep.1

sleep_LBP_cross <- xtabs(~ sleep + LBP, data = hn1415)
sleep_LBP_cross
chisq.test(sleep_LBP_cross)

# 22. Osteoarthritis

table(hn1415$OA)
qplot(hn1415$OA, bins = 20)
df_OA <- hn1415 %>% group_by(OA) %>% summarise(total = n())
df_OA.1 <- df_OA %>% mutate(percent = total / sum(total))
df_OA.1

table(hn1415_noLBP$OA)
qplot(hn1415_noLBP$OA, bins = 20)
df_OA <- hn1415_noLBP %>% group_by(OA) %>% summarise(total = n())
df_OA.1 <- df_OA %>% mutate(percent = total / sum(total))
df_OA.1

table(hn1415_LBP$OA)
qplot(hn1415_LBP$OA, bins = 20)
df_OA <- hn1415_LBP %>% group_by(OA) %>% summarise(total = n())
df_OA.1 <- df_OA %>% mutate(percent = total / sum(total))
df_OA.1

OA_LBP_cross <- xtabs(~ OA + LBP, data = hn1415)
OA_LBP_cross
chisq.test(OA_LBP_cross)

# 23. Rheumatoid arthritis

table(hn1415$RA)
qplot(hn1415$RA, bins = 20)
df_RA <- hn1415 %>% group_by(RA) %>% summarise(total = n())
df_RA.1 <- df_RA %>% mutate(percent = total / sum(total))
df_RA.1

table(hn1415_noLBP$RA)
qplot(hn1415_noLBP$RA, bins = 20)
df_RA <- hn1415_noLBP %>% group_by(RA) %>% summarise(total = n())
df_RA.1 <- df_RA %>% mutate(percent = total / sum(total))
df_RA.1

table(hn1415_LBP$RA)
qplot(hn1415_LBP$RA, bins = 20)
df_RA <- hn1415_LBP %>% group_by(RA) %>% summarise(total = n())
df_RA.1 <- df_RA %>% mutate(percent = total / sum(total))
df_RA.1

RA_LBP_cross <- xtabs(~ RA + LBP, data = hn1415)
RA_LBP_cross
chisq.test(RA_LBP_cross)

# 24. Sitting time

table(hn1415$sitting)
qplot(hn1415$sitting, bins = 20)
df_sitting <- hn1415 %>% group_by(sitting) %>% summarise(total = n())
df_sitting.1 <- df_sitting %>% mutate(percent = total / sum(total))
df_sitting.1

table(hn1415_noLBP$sitting)
qplot(hn1415_noLBP$sitting, bins = 20)
df_sitting <- hn1415_noLBP %>% group_by(sitting) %>% summarise(total = n())
df_sitting.1 <- df_sitting %>% mutate(percent = total / sum(total))
df_sitting.1

table(hn1415_LBP$sitting)
qplot(hn1415_LBP$sitting, bins = 20)
df_sitting <- hn1415_LBP %>% group_by(sitting) %>% summarise(total = n())
df_sitting.1 <- df_sitting %>% mutate(percent = total / sum(total))
df_sitting.1

sitting_LBP_cross <- xtabs(~ sitting + LBP, data = hn1415)
sitting_LBP_cross
chisq.test(sitting_LBP_cross)

# 25. Stress

table(hn1415$stress)
qplot(hn1415$stress, bins = 20)
df_stress <- hn1415 %>% group_by(stress) %>% summarise(total = n())
df_stress.1 <- df_stress %>% mutate(percent = total / sum(total))
df_stress.1

table(hn1415_noLBP$stress)
qplot(hn1415_noLBP$stress, bins = 20)
df_stress<- hn1415_noLBP %>% group_by(stress) %>% summarise(total = n())
df_stress.1 <- df_stress %>% mutate(percent = total / sum(total))
df_stress.1

table(hn1415_LBP$stress)
qplot(hn1415_LBP$stress, bins = 20)
df_stress <- hn1415_LBP %>% group_by(stress) %>% summarise(total = n())
df_stress.1 <- df_stress %>% mutate(percent = total / sum(total))
df_stress.1

stress_LBP_cross <- xtabs(~ stress + LBP, data = hn1415)
stress_LBP_cross
chisq.test(stress_LBP_cross)

# The end ----