#=======================================================================#
#Data Analysis  -   Car dataset - Mode of Transport to Commute  
#Developer      -   Tahmid Bari
#Date           -   July 04, 2020
#=======================================================================#


#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("csv")
#install.packages("xlsx")
#install.packages("dplyr")
#install.packages("devtools")
#install.packages("NbClust")
#install.packages("naivebayes")
#install.packages("e1071")
#install.packages("pscl")
#install.packages("lmtest")
#install.packages("purrr")
#install.packages("tidyr")
#install.packages("car")
#install.packages("DMwR")
#install.packages("ipred")
#install.packages("rpart")
#install.packages("gbm")
#install.packages("vip")
#install.packages("VIM")
#install.packages("GGally")
#install.packages("caret, repos = http://cran.us.r-project.org")
#install.packages("rpart, repos = http://cran.us.r-project.org")
#install.packages("rpart.plot, repos = http://cran.us.r-project.org")

## Loading Library ##
library(readxl) ##Read excel
library(readr) ##Read csv
library(corrplot) ##library for correlation
library(lattice) ## for plots
library(caret) ##for confusionMatrix
library(ROCR) ##for auc,KS
library(ineq) ##gini
library(caTools) ##to Split data
library(naivebayes) ##Naive Bayes model for Numeric Predictors
library(e1071) ##For Naise Bayes
library(class) ## For KNN Classifier
library(pscl) ##Maximum likelihood estimation
library(lmtest) ##diagnostic checking in linear regression models
library(purrr) ## for Visualization
library(tidyr)## for Visualization
library(ggplot2) ## Data Visualization
library(car) ## vif
library(DMwR) ##to treat missing values by filling in NA values with the values of the nearest neighbours
library(ipred)##For Bagging
library(rpart)##For Bagging
library(gbm) ##For Boosting
library(vip) ## To find importance of variables
library(VIM) ## To load VIM

## Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/Great Learning/Course Work/Predictive Modeling/Project-4")

## Check working directory
getwd()

## EDA
## This is an R Markdown document created to Predict whether or not an employee will use Car as a mode of transport.
## Perform exploratory data analysis on the dataset. Showcase some charts, graphs. Check for outliers and missing values and also check the performance of various predictive model
```{r Mode of Transport}

carData <- read_csv("Cars-dataset.csv")
file.exists("C:\\Users\\Tahmid Bari\\Desktop\\Great Learning\\Course Work\\Predictive Modeling\\Project-4\\Cars-dataset.csv")

## EDA
dim(carData)
str(carData) 
head(carData)
tail(carData)

## Get Summary
summary(carData)

#set seed variable
seed=1234
```
carData=read.csv('Cars-dataset.csv')

#Convert Data type
carData$Engineer=as.factor(carData$Engineer)
carData$MBA=as.factor(carData$MBA)
carData$license=as.factor(carData$license)

#Check missing values data
sapply(carData,function (x) sum(is.na(x)))

#Treat Missing values
carData = knnImputation(carData, 5)
summary(carData)
##No missing values now.

#Adding a new variable Car Usage as we need to predict whether or not an employee will use Car as a mode of transport
carData$CarUsage<-ifelse(carData$Transport =='Car',1,0)
carData$CarUsage=as.factor(carData$CarUsage)

#we will drop the Transport column for further analysis
carDataNew=carData[,-9]
summary(carDataNew)


#check for outliers
boxplot(carDataNew$Age,col='green',main='Boxplot for Age')
boxplot(carDataNew$Work.Exp,col='yellow',main='Boxplot for Work Experience')
boxplot(carDataNew$Salary,col='red',main='Boxplot for Salary')
boxplot(carDataNew$Distance,col='pink',main='Boxplot for Distance')
## There are outliers in the numerical variables but they seem to be possible values. Hence, we are not treating them.

#Histogram
carDataEDA=carDataNew[,-c(2,3,4,8,9)]
carDataEDA=as.data.frame(carDataEDA)
for (i in (1:4)) {
  hist (carDataEDA[,i],  
        main = 'Histogram',xlab=colnames(carDataEDA[i]),ylab=NULL,col=c('blue','green')
  )
}

#Scatter Plot
for (i in (1:4)) {
  plot (carDataEDA[,i],  
        main = 'Scatter Plot',ylab=colnames(carDataEDA[i]),col=c('yellow','pink')
  )
}
## Library(tidyr)
## Library(purrr)

## Denisty Plot
carDataNew %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_density()                           # as density

attach(carDataNew)
#Bivariate Analysis
histogram(~CarUsage|factor(MBA),data = carDataNew,main="carUsage wrt MBA")
histogram(~CarUsage|factor(Engineer),data = carDataNew,main="carUsage wrt Engineer")
histogram(~CarUsage|factor(Gender),data = carDataNew,main="carUsage wrt Gender")
histogram(~CarUsage|factor(license),data = carDataNew,main="carUsage wrt license")
boxplot(carDataNew$Salary~carDataNew$Engineer, main = "Salary vs Eng.")
boxplot(carDataNew$Salary~carDataNew$MBA, main = "Salary vs MBA")
boxplot(carDataNew$Salary~carDataNew$CarUsage, main = "Salary vs CarUsage")
boxplot(carDataNew$Distance~carDataNew$CarUsage, main = "Distance vs CarUsage")
boxplot(carDataNew$Age~carDataNew$CarUsage, main = "Age vs CarUsage")

## Check Collinearity
## Correlation Matrix. We can ignore categorical variables.
carNum=carDataNew[,-c(2,3,4,8,9)]
str(carDataNew)
str(carNum)
carCor=cor(carNum)
carCor=round(carCor,2)
corrplot(carCor,type = "upper")
corrplot(carCor,type = "upper",method='number')

#data1 <- VIM::KNN(data=data, variable = c("MBA"),k=7)
#sapply(data1, function(x) sum(is.na(x)))

## High correlation between Age and Work experienece as well as age and salary. 
## Work experience is also highly correlated to salary
GGally::ggpairs(carNum, mapping = aes(color = CarUsage))

#Check Multicollinearity
#logistic : Model 1
LRModel_1=glm(CarUsage~., data = carDataNew, family = binomial(link="logit"))

## Check multicollienearity
vif(LRModel_1)
## Vif for Age and Work.Exp is high, we can drop work.exp and check

## logistic : Model 2
LRModel_2=glm(CarUsage~Age+Gender+Engineer+MBA+Salary+Distance+license, data = carDataNew, family = binomial(link="logit"))

## Check multicollienearity
vif(LRModel_2)

## Drop Work Exp
carDataFinal=carDataNew[,-5]
summary(carDataFinal)
```

## Smote
## We will check for imbalance data and treat it

```{r SMOTE}
#set seed
set.seed(seed)

## 70:30 ratio data splitting
## Get 70% of the sample size
splitLR = sample.split(carDataFinal$CarUsage, SplitRatio = 0.7)

trainDataLR<-subset(carDataFinal, splitLR == TRUE)
testDataLR<- subset(carDataFinal, splitLR == FALSE)
nrow(trainDataLR)
nrow(testDataLR)
prop.table(table(trainDataLR$CarUsage))

#Train Data for Bagging
trainDataBag=trainDataLR

#Test Data for Bagging
testDataBag=testDataLR

#Train Data for Boosting
trainDataBoost=trainDataLR

#Test Data for Boosting
testDataBoost=testDataLR

#Test Data for Naive Bayes
testDataNB=testDataLR

#Percentage positive in Train
table(trainDataLR$CarUsage)
sum(trainDataLR$CarUsage == 1)/nrow(trainDataLR)

#Percentage positive in test
table(testDataLR$CarUsage)
sum(testDataLR$CarUsage == 1)/nrow(testDataLR)

#Data is imbalanced so we SMOTE
carsTrainSMOTE<-SMOTE(CarUsage~., trainDataLR,perc.over = 550,perc.under = 160)
prop.table(table(carsTrainSMOTE$CarUsage))

dim(trainDataLR)
dim(carsTrainSMOTE)
```
## Logistic Regression
```{r Logistic Regression}
set.seed(seed)

#Logistic: Model2
LRModel=glm(CarUsage~., data = carsTrainSMOTE, family = binomial)

#Check AIC and statistical significant variables
summary(LRModel)

#Check Loglikelihood and p value
lrtest(LRModel)

#Check multicollienearity
vif(LRModel)
dim(carsTrainSMOTE)

# Predicting test set
testDataLR$response=predict(LRModel,newdata = testDataLR[,-8],type="response")
testDataLR$Usage_Predict=ifelse(testDataLR$response<.5,"0","1")

#Convert to factor
testDataLR$Usage_Predict=as.factor(testDataLR$Usage_Predict)
tabTestLR=table(testDataLR$Usage_Predict,testDataLR$CarUsage)
tabTestLR

#Confusion Matrix
confusionMatrix(testDataLR$Usage_Predict,testDataLR$CarUsage, positive="1")

#AUC KS GINI
ROCRTestLR=prediction(testDataLR$response,testDataLR$CarUsage)
aucTestLR=as.numeric(performance(ROCRTestLR,"auc")@y.values)
print(paste('Area Under the Curve for test Dataset:',aucTestLR))
perfTestLR=performance(ROCRTestLR,"tpr","fpr")
plot(perfTestLR,main="AUC ROC Curve for test dataset")
KSTestLR <- max(attr(perfTestLR, 'y.values')[[1]]-attr(perfTestLR, 'x.values')[[1]])
print(paste('K-S Value for test Dataset',KSTestLR))
giniTestLR = ineq(testDataLR$response, type="Gini")
print(paste('Gini Coefficient for test dataset:',giniTestLR))

```

## KNN
```{r KNN}
set.seed(seed)
set.seed(seed)
carDataknn=carDataFinal

#For KNN we convert Gender in 0 and 1
carDataknn$Gender=ifelse(carDataknn$Gender=="Male",1,0)
carDataknn$Gender=as.factor(carDataknn$Gender)

str(carDataknn)

#Normalizing the data
norm = function(x) { (x- min(x))/(max(x) - min(x)) }
normKNN = as.data.frame(lapply(carDataknn[,-c(2,3,4,7,8)], norm))

head(normKNN)
finalDataKNN = cbind(carDataknn[,c(2,3,4,7,8)], normKNN)
head(finalDataKNN)

# Data partitioning
splitKNN = sample.split(finalDataKNN$CarUsage, SplitRatio = 0.7)
trainDataKnn = subset(finalDataKNN, splitKNN == T)
testDataKnn = subset(finalDataKNN, splitKNN == F)
head(trainDataKnn)
head(testDataKnn)
knnTrainSMOTE<-SMOTE(CarUsage~., trainDataKnn,perc.over = 550,perc.under = 160)
prop.table(table(knnTrainSMOTE$CarUsage))
str(knnTrainSMOTE)

predKnn9 = knn(knnTrainSMOTE[-5], testDataKnn[-5], knnTrainSMOTE[,5], k = 9) 
tabKnn9 = table(testDataKnn[,5], predKnn9)
tabKnn9
accKnn9=sum(diag(tabKnn9)/sum(tabKnn9)) 
print(paste('Accuracy for KNN with k=9:',accKnn9))

#Confusion Matrix
confusionMatrix(predKnn9,testDataKnn$CarUsage, positive="1")
```
##Naive Bayes
```{r Naive Bayes}
set.seed(seed)

#Model building on train data
ModelNB = naiveBayes( carsTrainSMOTE$CarUsage~., data = carsTrainSMOTE)
ModelNB

#Model on test data
testDataNB$CarUsage_Predict=predict(ModelNB, newdata = testDataNB, type = "class")
testTabNB=table(testDataNB$CarUsage_Predict,testDataNB$CarUsage)
testTabNB

#Confusion Matrix
confusionMatrix(testDataNB$CarUsage_Predict,testDataNB$CarUsage, positive="1")
```
##Bagging
```{r Bagging}
set.seed(seed)
ModelBagging<- bagging(trainDataBag$CarUsage ~.,
                       data=trainDataBag,
                       control=rpart.control(maxdepth=5, minsplit=15))

## Training
trainDataBag$pred.class <- predict(ModelBagging, trainDataBag)

table(trainDataBag$CarUsage,trainDataBag$pred.class)
confusionMatrix(trainDataBag$pred.class,trainDataBag$CarUsage, positive="1")

#Testing
testDataBag$pred.class <- predict(ModelBagging, testDataBag)
table(testDataBag$CarUsage,testDataBag$pred.class)
confusionMatrix(testDataBag$pred.class,testDataBag$CarUsage, positive="1")
```
## Boosting
```{r Boosting}
set.seed(seed)
gbm.fit <- gbm(
  formula = CarUsage ~ .,
  distribution = "multinomial", #We are using bernoulli because we are doing a logistic and want probabilities
  data = trainDataBoost,
  n.trees = 10000, #These are the number of stumps
  interaction.depth = 1, #Number of splits it has to perform on a tree (starting from a single node)
  shrinkage = 0.001, #Shrinkage is used for reducing, or shrinking the impact of each additional fitted base-learner(tree)
  cv.folds = 5, #Cross validation folds
  n.cores = NULL, #Will use all cores by default
  verbose = FALSE #After every tree/stump it is going to show the error and how it is changing
)
gbm.fit

#Training
trainDataBoost$response= predict(gbm.fit, trainDataBoost[-8], type = "response")
trainDataBoost$pred.CarUsage <- apply(trainDataBoost$response, 1, which.max)

#Get predicted values in 0 and 1
trainDataBoost$CarUsage_Predict<-ifelse(trainDataBoost$pred.CarUsage==1,0,1)

#Convert to fator
trainDataBoost$CarUsage_Predict=as.factor((trainDataBoost$CarUsage_Predict))
table(trainDataBoost$CarUsage,trainDataBoost$CarUsage_Predict)
confusionMatrix(trainDataBoost$CarUsage_Predict,trainDataBoost$CarUsage, positive="1")

#Testing
testDataBoost$response= predict(gbm.fit, testDataBoost[-8], type = "response")
testDataBoost$pred.CarUsage <- apply(testDataBoost$response, 1, which.max)

#Get predicted values in 0 and 1
testDataBoost$CarUsage_Predict<-ifelse(testDataBoost$pred.CarUsage==1,0,1)

#Convert to fator
testDataBoost$CarUsage_Predict=as.factor((testDataBoost$CarUsage_Predict))
table(testDataBoost$CarUsage,testDataBoost$CarUsage_Predict)
confusionMatrix(testDataBoost$CarUsage_Predict,testDataBoost$CarUsage, positive="1")
vip::vip(gbm.fit, num_features = 8, bar = FALSE)
```