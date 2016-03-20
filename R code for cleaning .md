# Give-Me-Some-Credit
Banks play a crucial role in market economies. They decide who can get finance and on what terms and can make or 
break investment decisions. For markets and society to function, individuals and companies need access to credit.  
Credit scoring algorithms, which make a guess at the probability of default, are the method banks use to determine 
whether or not a loan should be granted. This competition requires participants to improve on the state of the art 
in credit scoring, by predicting the probability that somebody will experience financial distress in the next two years.  
The goal of this competition is to build a model that borrowers can use to help make the best financial decisions


#phase 2
# read the training set to R
train = read.csv("cs-training (1).csv")
str(train)
summary(train)
#imputation required (for monthly income and Number of dependents)
#loading mice package
library(mice)
#create a copy of training set
impute = train
imputedtrain = complete(mice(train))

summary(imputedtrain)

write.csv(immputedtrain, "imputedtrain.csv")

#phase 3
#reading the training and testing sets into R

train = read.csv("imputedtrain.csv")

test = read.csv("cs-test.csv")

str(train)

summary(train)

str(test)

summary(test)

#noticed outliers in several columns! hence we have to do data preprocessing 

#removing the outliers from the data to make sure that the data is consistent

boxplot(train$RevolvingUtilizationOfUnsecuredLines)

# the outliers are 

head(sort(train$RevolvingUtilizationOfUnsecuredLines, decreasing = TRUE))

#[1] 50708 29110 22198 22000 20514 18300

boxplot(train$DebtRatio)

boxplot(train$MonthlyIncome)

boxplot(train)

#major outliers are present in monthlyincome and debt ratio

#removing them as a part of data preprocessing


#test data has missing values

library(mice)

#create a copy of training set

impute = test

imputedtest = complete(mice(test))

summary(imputedtest)

write.csv(immputedtest, "imputedtest.csv")

train = read.csv("imputedtrainwithlog.csv")

test = read.csv("imputedtestwithlog.csv")

library(caret)

names(train)[5] = "NumberOfTime30to59DaysPastDueNotWorse"

names(train)[11] = "NumberOfTime60to89DaysPastDueNotWorse"

names(train)[2] = "Delinquin"

fitControl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

tr = train(as.factor(Delinquin)~., train, method="rf", nodesize=4, ntree=1000, metric="ROC", trControl=fitControl)

rf= randomForest(as.factor(Delinquin) ~. , train, mtry = 10,ntree = 1000)

library(caTools)

split = sample.split(train, SplitRatio = 0.55)

trainset = subset(train , split == TRUE)

nrow(trainset)

test = subset(test , split == FALSE)

pr = predict(gl,trainset$Delinquin)

library(pROC)

g <- roc(Delinquin ~ pr, data = train)

plot(g)  

#Call:

#roc.formula(formula = Delinquin ~ pr, data = train)

#Data: pr in 139974 controls (Delinquin 0) < 10026 cases (Delinquin 1).

#Area under the curve: 0.7018

gl1 = glm(Delinquin ~ age+ NumberOfTime30to59DaysPastDueNotWorse + DebtRatio + MonthlyIncome + NumberOfOpenCreditLinesAndLoans + 
NumberRealEstateLoansOrLines+ NumberOfDependents, trainset,family = "binomial")

gl1 = glm(Delinquin ~ age+ NumberOfTime30to59DaysPastDueNotWorse + DebtRatio + MonthlyIncome  + NumberRealEstateLoansOrLines+ 
NumberOfDependents, trainset,family = "binomial")

pr = predict(gl1,trainset$Delinquin)

library(pROC)

g1 <- roc(Delinquin ~ pr, data = trainset)

plot(g1) 

gl2 = glm(Delinquin ~ age+ NumberOfTimes90DaysLate + NumberOfTime30to59DaysPastDueNotWorse + DebtRatio + logmonthlyincome + NumberOfOpenCreditLinesAndLoans + NumberRealEstateLoansOrLines+ NumberOfDependents, trainset,family = "binomial")

gl3 = glm(SeriousDlqin2yrs ~ ., train,family = "binomial")
pr = predict(gl3,type = "response")

library(pROC)

g3 <- roc(SeriousDlqin2yrs ~ pr, data = train)


table(train$SeriousDlqin2yrs, pr>0.3)

FALSE   TRUE
0 139077    897
1   9040    986


plot(g3)  
