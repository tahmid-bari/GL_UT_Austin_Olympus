#=======================================================================#
#Data Analysis  -   Thera Bank - Loan Purchase Modeling 
#Developer      -   Tahmid Bari
#Date           -   May 23, 2020
#=======================================================================#

#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("xlsx")
#install.packages("dplyr")
#install.packages("devtools")
#install.packages("NbClust")
#install.packages("caret, repos = http://cran.us.r-project.org")
#install.packages("rpart, repos = http://cran.us.r-project.org")
#install.packages("rpart.plot, repos = http://cran.us.r-project.org")
#install.packages("randomForest, repos = http://cran.us.r-project.org")


## Loading Library ##
library(xlsx)
library(readxl)
library("readr")
library(dplyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(lattice)
library(DataExplorer)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(ineq)
library(InformationValue)
library(data.table)
library(scales)
library(Metrics)
library(grDevices)
library(factoextra)
library(ROCit)
library(kableExtra)
 
## Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/Great Learning/Course Work/Machine Learning/Project-3")

## Check working directory
getwd()

TBdata <- read_excel("Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx")
file.exists("C:\\Users\\Tahmid Bari\\Desktop\\Great Learning\\Course Work\\Machine Learning\\Project-3\\Thera Bank_Personal_Loan_Modelling-dataset-1.xlsx")

### Exploratory Data Analysis
head(TBdata,10)
tail(TBdata,10)

## Lets see the variable list in the data
names(TBdata)

## Reordering the position of target variable/dependent variable Personal Loan to last Data
TBdata[c(1,2,3,4,5,6,7,8,9,11,12,13,14,10)]

## Removing column Id and zip code as they will not help much in analysis since they are basically
## addon - para information
TBdata = TBdata[,-c(1,5)]

## Check Missing values 
colSums(is.na(TBdata)) 
## Family members has missing values

## Lets see the datatype of the data set 
str(TBdata) 
summary(TBdata) 

## Experience (in years ) has negative values which is incorrect value 
## Check the dimension or shape of the data 
dim(TBdata) 

## Fixing the Missing Values in Family members and negative values in Experience by algorithms 
TBdata$`Experience (in years)`[TBdata$`Experience (in years)` < 0] <- 0 
TBdata$`Family members`[is.na(TBdata$`Family members`)] <- median(TBdata$`Family members`, na.rm=TRUE) 

## Converting target variable into factor
TBdata$`Personal Loan` = as.factor(TBdata$`Personal Loan`)

## Converting categorical variables into factor
TBdata$`Family members` = as.factor(TBdata $`Family members`)
TBdata$Education = as.factor(TBdata $Education)
TBdata$`Securities Account`= as.factor(TBdata $`Securities Account`)
TBdata$`CD Account` = as.factor(TBdata $`CD Account`)
TBdata$Online = as.factor(TBdata $Online)
TBdata$CreditCard = as.factor(TBdata$CreditCard)

## Introducing the Dataset
plot_intro(TBdata)

## Plotting the histogram for all numerical variables
plot_histogram(TBdata)

##Frequency distribution for categorical variable (Univariate Analysis)
table(TBdata[,c(4)])
table(TBdata[,c(6)])
table(TBdata[,c(8)])
table(TBdata[,c(9)])
table(TBdata[,c(10)])
table(TBdata[,c(11)])
table(TBdata[,c(12)])

## Plotting bar plot for all factor variables
plot_bar(TBdata[c(4,6,8,9,10,11,12)])

## Box plot for all variables
boxplot(TBdata[-c(4,6,8,9,10,11,12)],las=2)


## Bivariate analyisis
## Plotting boxplot for Personal Loan (Response variable) for all variables
p1 = ggplot(TBdata, aes(`Age (in years)`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p2 = ggplot(TBdata, aes(`Experience (in years)`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p3 = ggplot(TBdata, aes(`Income (in K/month)`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p4 = ggplot(TBdata, aes(`Mortgage`, fill= `Personal Loan`)) + geom_density(alpha=0.5)
p5 = ggplot(TBdata, aes(`Education`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p6 = ggplot(TBdata, aes(`Family members`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p7= ggplot(TBdata, aes(`CCAvg`, fill= `Personal Loan`)) + geom_histogram(alpha=0.5, bins=50)
p8 = ggplot(TBdata, aes(`Securities Account`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p9 =ggplot(TBdata, aes(`CreditCard`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p10 = ggplot(TBdata, aes(`CD Account`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
p11 = ggplot(TBdata, aes(`Online`, fill= `Personal Loan`)) + geom_bar(alpha=0.5)
grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,p9,p10,p11, ncol = 2, nrow = 6)
p12 = ggplot(TBdata, aes(`Income (in K/month)`,y=`Mortgage`, color= `Personal Loan`)) + geom_point(size=1)
p12
p14 = ggplot(TBdata, aes(`Income (in K/month)`, y=`CCAvg`, color= `Personal Loan`)) + geom_point(size=1)
p14

## Correlation between numeric continuous independant variables
matrix= cor(TBdata[,-c(4,6,8,9,10,11,12)] ,method = "pearson")
matrix
corrplot(matrix,type="upper",method = "number")
corrplot(matrix, method = "circle")

## Distance Calculation
Distchebyshev = dist(x=TBdata, method = "maximum")
Disteuc = dist(x=TBdata, method = "euclidean")
HCclusteuc = hclust(Disteuc, method = "complete")
HCclustch = hclust(Distchebyshev, method = "complete")

## Cluster Height, Sorting and Plotting
clusterheight = HCclusteuc$height
clusterheight = sort(clusterheight, decreasing = TRUE)
plot(clusterheight, pch =20, col="red", main="Cluster Height", ylab="Cluster Height")
lines(clusterheight, lty=2, lwd=2, col="blue")

## Cluster Plotting and Comparison
par(mfrow=c(2,1))
plot(HCclusteuc, labels = as.character(TBdata[,2]), main = "HClust using Euclidian method", xlab = "Euclidian distance", ylab = "Height")rect.hclust(HCclusteuc, k=3, border = "red")
plot(HCclusteuc, labels = as.character(TBdata[,2]), main = "HClust using Chebychev method", xlab = "Chebychev distance", ylab = "Height")rect.hclust(HCclusteuc, k=3, border = "red")


###### Clustering Using K Means
TBdata.clus = TBdata%>% select_if(is.numeric)
TBdata.scaled = scale(TBdata.clus, center = TRUE)
TBdata.dist = dist(TBdata.scaled, method = "euclidean")

## checking optimal number of clusters to categorize dataset
P20 = fviz_nbclust(TBdata.scaled, kmeans, method = "silhouette", k.max = 5)
p21 = fviz_nbclust(TBdata.scaled, kmeans, method = "wss", k.max = 5)
grid.arrange(p12, p21, ncol=2)
set.seed(1234)
TBdata.clusters = kmeans(TBdata.scaled, 3, nstart = 10)
fviz_cluster(TBdata.clusters, TBdata.scaled, geom = "point",
             ellipse = TRUE, pointsize = 0.2, ) + theme_minimal()

## Splitting the dataset into train and test for development and out of sample testing respectively
set.seed(100)
P_Loan_TRAIN_INDEX <- sample(1:nrow(TBdata),0.70*nrow(TBdata))
CARTtrain <- TBdata[P_Loan_TRAIN_INDEX,]
CARTtest <- TBdata[-P_Loan_TRAIN_INDEX,]

## Calculate the response rate
sum(TBdata$`Personal Loan` == "1")/nrow(TBdata)
sum(CARTtrain$`Personal Loan` == "1")/nrow(CARTtrain)
sum(CARTtest$`Personal Loan` == "1")/nrow(CARTtest)
table(CARTtrain$`Personal Loan`)
prop.table(table(CARTtrain$`Personal Loan`))
table(CARTtest$`Personal Loan`)
prop.table(table(CARTtest$`Personal Loan`))


## Check top 6 observation of train dataset
head(CARTtrain)

## CART Model
## Import rpart and rpart.plot library for creating CART model
#library(rpart)
#library(rpart.plot)
## Build first CART model
## Setting the control parameters for rpart
#minsplit: if the number of records in a given node falls below a threshold, the node will not be split further.
#minbucket: minimum records in a terminal node. if the records are less, that bucket will not be created.
#minsplit = 3(minbucket)
#cp = cost complexity parameter
#We begin by building a very complex classification tree, by setting the "cost complexity" threshold
#to "0" and the minimum bucket size to be 10. Then we plot the tree using rpart.
tree = rpart(formula = `Personal Loan` ~ .,data=CARTtrain,method="class",minbucket = 10,cp=0)

## Plot tree
tree
rpart.plot(tree)

## The cost complexity table can be obtained using the printcp or plotcp functions
## Print cp value
printcp(tree)

## Plot cp value
plotcp(tree)

#Pruning the tree
#The unncessarily complex tree above can be pruned using a cost complexity threshold.
#Using a complexity threshold of 0.003 gives us a much simpler tree.
ptree = prune(tree,cp=0.003,"CP")

## Print cp value
printcp(ptree)

## Check the updated tree
## Plot tree
ptree
rpart.plot(ptree)

## Visualize the CART tree
boxcols <- c("orange", "palegreen3")[ptree$frame$yval]

# Ptree$frame$yval
par(xpd=TRUE)
prp(ptree, faclen=0, cex=0.6, extra=1, box.col=boxcols, nn=TRUE, uniform=TRUE)

## Variable Importance
library(caret)
summary(ptree)
ptree$variable.importance
df_cart = data.frame(ptree$variable.importance)
df_cart

## Use this tree to do the prediction on train as well as test data set
CARTtrain$CART.Pred = predict(ptree,data=CARTtrain,type="class")
CARTtrain$CART.Score = predict(ptree,data=CARTtrain,type="prob")[,"1"]
CARTtest$CART.Pred = predict(ptree,CARTtest,type="class")
CARTtest$CART.Score = predict(ptree,CARTtest,type="prob")[,"1"]

##Confusion Metrix
#train dataset
CART_CM_train = confusionMatrix(CARTtrain$`Personal Loan`, CARTtrain$CART.Score,threshold = 0.7)
CART_CM_train

## Test dataset
CART_CM_test = confusionMatrix(CARTtest$`Personal Loan`, CARTtest$CART.Score, threshold = 0.7)
CART_CM_test

## Error Rate
#train
(CART_CM_train[1,2]+CART_CM_train[2,1])/nrow(CARTtrain)
#test
(CART_CM_test[1,2]+CART_CM_test[2,1])/nrow(CARTtest)

## Accuracy
#train
(CART_CM_train[1,1]+CART_CM_train[2,2])/nrow(CARTtrain)
#test
(CART_CM_test[1,1]+CART_CM_test[2,2])/nrow(CARTtest)

##Specificity
#train
(CART_CM_train[1,1])/(CART_CM_train[1,1]+CART_CM_train[2,1])
#test
(CART_CM_test[1,1])/(CART_CM_test[1,1]+CART_CM_test[2,1])

##Sensitivity
#train
(CART_CM_train[2,2])/(CART_CM_train[2,2]+CART_CM_train[1,2])
#test
(CART_CM_test[2,2])/(CART_CM_test[2,2]+CART_CM_test[1,2])

### ROCR and ineq packages to compute AUC, KS and gini
predobjtrain = prediction(CARTtrain$CART.Score,CARTtrain$`Personal Loan`)
## View(predobjtrain)
preftrain = performance(predobjtrain,"tpr","fpr")
plot(preftrain)

## View(preftrain)
predobjtest = prediction(CARTtest$CART.Score,CARTtest$`Personal Loan`)
preftest = performance(predobjtest,"tpr","fpr")
plot(preftest)

## KS
max(preftrain@y.values[[1]]-preftrain@x.values[[1]])
# preftrain@y.values[[1]]
# preftrain@x.values[[1]]
max(preftest@y.values[[1]]-preftest@x.values[[1]])

## AUC
auctrain=performance(predobjtrain,"auc")
as.numeric(auctrain@y.values)

auctest=performance(predobjtest,"auc")
as.numeric(auctest@y.values)

## Gini
ineq(CARTtrain$CART.Score,"gini")
ineq(CARTtest$CART.Score,"gini")

## Concordance
Concordance(actuals=CARTtrain$`Personal Loan`,predictedScores = CARTtrain$CART.Score)
Concordance(actuals=CARTtest$`Personal Loan`,predictedScores = CARTtest$CART.Score)

## Define Decile function for rank ordering
decile <- function(x){
  deciles <- vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] <- quantile(x, i, na.rm=T)
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

## Rank order table for train dataset
CARTtrain$deciles <- decile(CARTtrain$CART.Score)

## Ranking code
inter_datatable_train_cart = data.table(CARTtrain)
rank <- inter_datatable_train_cart[, list(
  cnt = length(as.integer(as.character(`Personal Loan`))),
  cnt_P_Loan = sum(as.integer(as.character(`Personal Loan`))==1),
  cnt_no_P_Loan = sum(as.integer(as.character(`Personal Loan`)) == 0)) ,
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_P_Loan / rank$cnt,4);
rank$cum_P_Loan <- cumsum(rank$cnt_P_Loan)
rank$cum_no_P_Loan <- cumsum(rank$cnt_no_P_Loan)
rank$cum_rel_P_Loan <- round(rank$cum_P_Loan / sum(rank$cnt_P_Loan),4);
rank$cum_rel_no_P_Loan <- round(rank$cum_no_P_Loan / sum(rank$cnt_no_P_Loan),4);
rank$rrate_perc <- percent(rank$rrate)
rank$cum_rel_P_Loan_perc <- percent(rank$cum_rel_P_Loan)
rank$cum_rel_no_P_Loan_perc <- percent(rank$cum_rel_no_P_Loan)
rank$cum_cnt<-cumsum(rank$cnt)
rank$cum_P_Loan_rate<-round(rank$cum_P_Loan / rank$cum_cnt,4)
overall_P_Loan_rate<-sum(as.integer(as.character(CARTtrain$`Personal Loan`)))/nrow(CARTtrain)
rank$ks = abs(rank$cum_rel_P_Loan - rank$cum_rel_no_P_Loan);

## Derive Lift
rank$lift<-round(rank$cum_P_Loan_rate/overall_P_Loan_rate,2)
View(rank)
write.csv(rank, "RankOrderingCM1.csv")

### Rank Order table for test dataset

CARTtest$deciles <- decile(CARTtest$CART.Score)

## Ranking code
inter_datatable_test_cart = data.table(CARTtest)
rank_test_cart <- inter_datatable_test_cart[, list(
  cnt = length(as.integer(as.character(`Personal Loan`))),
  cnt_P_Loan = sum(as.integer(as.character(`Personal Loan`))==1),
  cnt_no_P_Loan = sum(as.integer(as.character(`Personal Loan`)) == 0)) ,
  by=deciles][order(-deciles)]
rank_test_cart$rrate <- round(rank_test_cart$cnt_P_Loan / rank_test_cart$cnt,4);
rank_test_cart$cum_P_Loan <- cumsum(rank_test_cart$cnt_P_Loan)
rank_test_cart$cum_no_P_Loan <- cumsum(rank_test_cart$cnt_no_P_Loan)
rank_test_cart$cum_rel_P_Loan <- round(rank_test_cart$cum_P_Loan / sum(rank_test_cart$cnt_P_Loan),4);
rank_test_cart$cum_rel_no_P_Loan <- round(rank_test_cart$cum_no_P_Loan / sum(rank_test_cart$cnt_no_P_Loan),4);
rank_test_cart$rrate_perc <- percent(rank_test_cart$rrate)
rank_test_cart$cum_rel_P_Loan_perc <- percent(rank_test_cart$cum_rel_P_Loan)
rank_test_cart$cum_rel_no_P_Loan_perc <- percent(rank_test_cart$cum_rel_no_P_Loan)
rank_test_cart$cum_cnt<-cumsum(rank_test_cart$cnt)
rank_test_cart$cum_P_Loan_rate<-round(rank_test_cart$cum_P_Loan / rank_test_cart$cum_cnt,4)
overall_P_Loan_rate_test<-sum(as.integer(as.character(CARTtest$`Personal Loan`)))/nrow(CARTtest)
rank_test_cart$ks = abs(rank_test_cart$cum_rel_P_Loan - rank_test_cart$cum_rel_no_P_Loan);

## Get Lift
rank_test_cart$lift<-round(rank_test_cart$cum_P_Loan_rate/overall_P_Loan_rate_test,2)
View(rank_test_cart)
write.csv(rank_test_cart, "RankOrderingCM2.csv")

## Lift graph for CART model
library(lift)
plotLift(CARTtrain$CART.Score,CARTtrain$`Personal Loan`,cumulative = FALSE,n.buckets = 10)
plotLift(CARTtest$CART.Score,CARTtest$`Personal Loan`,cumulative = FALSE,n.buckets = 10)


### KS Graph for CART Model
sample1<-rnorm(CARTtrain$CART.Pred)
sample2<-rnorm(CARTtest$CART.Pred)
group <- c(rep("sample1", length(sample1)), rep("sample2", length(sample2)))
dat <- TBdata.frame(KSD = c(sample1,sample2), group = group)
cdf1 <- ecdf(sample1)
cdf2 <- ecdf(sample2)
minMax <- seq(min(sample1, sample2), max(sample1, sample2), length.out=length(sample1))
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )]
y0 <- cdf1(x0)
y1 <- cdf2(x0)
ggplot(dat, aes(x = KSD, group = group, colour = group, linetype=group))+
  stat_ecdf(size=1) +
  xlab("mm") +
  ylab("Cumulitive Distibution") +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
  ggtitle("K-S Test: Sample 1 / Sample 2")

## Buildig Random Forest model
names(TBdata)=c("Age","Experience","Income","FamilyMembers"
              ,"CCAvg","Education","Mortgage","SecuritiesAccount"
              ,"CDAccount","Online","CreditCard","PersonalLoan")

## Spliting the dataset into train and test for development and out of sample testing respectively
#dim(HRData)
set.seed(100)
P_Loan_TRAIN_INDEX <- sample(1:nrow(TBdata),0.70*nrow(TBdata))
RFtrain <- TBdata[P_Loan_TRAIN_INDEX,]
RFtest <- TBdata[-P_Loan_TRAIN_INDEX,]
RFtrain

## Import randomForest library for building random forest model
library(randomForest)

## Set a seed for the randomness
seed = 1000
set.seed(seed)

##Build the first RF model
#ntree: number of trees to grow
#mtry: number of variables to be considered for split
#nodesize: minimum size (number of records) of terminal nodes
#should importance of predictors be assessed
mtry = floor(sqrt(ncol(RFtrain)))
mtry
Rforest <- randomForest(PersonalLoan ~., data = RFtrain, ntree = 401, mtry = 3,
                        nodesize = 10, importance = TRUE)

## Print the model to see the OOB and error rate
print(Rforest)

## Plot the RF to know the optimum number of trees
Rforest$err.rate
plot(Rforest, main="Error Rates Random Forest")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)

#The error rate plot w.r.t number of trees reveals that anything more than, say 181, trees are
#really not that valuable.
##Identify the importance of the variables
importance(Rforest)

#Larger the MeanDecrease values, the more important the variable.Look at the help files to get
#a better sense of how these are computed.

## Tune up the RF model to find out the best mtry
# Now we will "tune" the Random Forest by trying different m values. We will stick with
# 201 trees (odd number of trees are preferable). The returned forest, "tRforest" is the
# one corresponding to the best m
# Starting with the default value of mtry, search for the optimal value (with respect to Out-of-Bag error
# estimate) of mtry for randomForest.
# Parameter Explanation
# x - Predictor variables
# y - Target Variable (factor for classification, numeric for regression)
# mtryStart - starting value of mtry; default is the same as in randomForest
# ntreeTry - number of trees used at the tuning step
# stepFactor - at each iteration, mtry is inflated (or deflated) by this value
# improve - the (relative) improvement in OOB error must be by this much for the search to continue
# trace - whether to print the progress of the search
# plot - whether to plot the OOB error as function of mtry
# doBest - whether to run a forest using the optimal mtry found
# If doBest=FALSE (default), it returns a matrix whose first column contains the mtry values searched, and the second column the corresponding OOB error.
# If doBest=TRUE, it returns the randomForest object produced with the optimal mtry.
# nodesize - min terminal node size
# importance - compute variable importance or not
set.seed(seed)
tRforest = tuneRF(x=RFtrain[,-c(12)], y=RFtrain$PersonalLoan, ntreeTry = 181, mtryStart = 3,
                  stepFactor = 1.5,
                  improve = 0.0001, nodesize = 10, trace = TRUE, plot = TRUE,
                  doBest = TRUE, importance = TRUE)
#names(RFtrain)
## Build the refined RF model
Rforest1 <- randomForest(PersonalLoan~., data = RFtrain, ntree = 181, mtry = 6, nodesize = 10,
                         importance = TRUE)

## Use this tree to do the prediction on train as well as test data set
RFtrain$RF.Pred = predict(Rforest1,data=RFtrain,type="class")
RFtrain$RF.Score = predict(Rforest1,data=RFtrain,type="prob")[,"1"]
RFtest$RF.Pred = predict(Rforest1,RFtest,type="class")
RFtest$RF.Score = predict(Rforest1,RFtest,type="prob")[,"1"]

## Performance Measure Parameters
RF_CM_train = confusionMatrix(RFtrain$PersonalLoan, RFtrain$RF.Score,threshold = 0.7)
RF_CM_train
#test dataset
RF_CM_test = confusionMatrix(RFtest$PersonalLoan, RFtest$RF.Score, threshold = 0.7)
RF_CM_test

## Error Rate
(RF_CM_train[1,2]+RF_CM_train[2,1])/nrow(RFtrain)
(RF_CM_test[1,2]+RF_CM_test[2,1])/nrow(RFtest)

## Accuracy
(RF_CM_train[1,1]+RF_CM_train[2,2])/nrow(RFtrain)
(RF_CM_test[1,1]+RF_CM_test[2,2])/nrow(RFtest)

## Specificity ##
#train
(RF_CM_train[1,1])/(RF_CM_train[1,1]+RF_CM_train[2,1])
#test
(RF_CM_test[1,1])/(RF_CM_test[1,1]+RF_CM_test[2,1])

## Sensitivity ##
#train
(RF_CM_train[2,2])/(RF_CM_train[2,2]+RF_CM_train[1,2])
#test
(RF_CM_test[2,2])/(RF_CM_test[2,2]+RF_CM_test[1,2])

## Probablity Related Parameters like KS, ROC, AUC, Concordance, Discordance and Gini
predobjtrain = prediction(RFtrain$RF.Score,RFtrain$PersonalLoan)
preftrain = performance(predobjtrain,"tpr","fpr")
plot(preftrain)
predobjtest = prediction(RFtest$RF.Score,RFtest$PersonalLoan)
preftest = performance(predobjtest,"tpr","fpr")
plot(preftest)

## KS
max(preftrain@y.values[[1]]-preftrain@x.values[[1]])
max(preftest@y.values[[1]]-preftest@x.values[[1]])

## AUC
auctrain=performance(predobjtrain,"auc")
as.numeric(auctrain@y.values)
auctest=performance(predobjtest,"auc")
as.numeric(auctest@y.values)

## Gini
ineq(RFtrain$RF.Score,"gini")
ineq(RFtest$RF.Score,"gini")

## Concordance
Concordance(actuals=RFtrain$PersonalLoan,predictedScores = RFtrain$RF.Score)
Concordance(actuals=RFtest$PersonalLoan,predictedScores = RFtest$RF.Score)

## Rank Order Table
RFtrain$deciles <- decile(RFtrain$RF.Score)

## Ranking Code
inter_datatable_train_RF = data.table(RFtrain)
rank_FM <- inter_datatable_train_RF[, list(
  cnt = length(as.integer(as.character(PersonalLoan))),
  cnt_P_L = sum(as.integer(as.character(PersonalLoan))==1),
  cnt_no_P_L = sum(as.integer(as.character(PersonalLoan)) == 0)) ,
  by=deciles][order(-deciles)]
rank_FM$rrate <- round(rank_FM$cnt_P_L / rank_FM$cnt,4);
rank_FM$cum_P_L <- cumsum(rank_FM$cnt_P_L)
rank_FM$cum_no_P_L <- cumsum(rank_FM$cnt_no_P_L)
rank_FM$cum_rel_P_L <- round(rank_FM$cum_P_L / sum(rank_FM$cnt_P_L),4);
rank_FM$cum_rel_no_P_L <- round(rank_FM$cum_no_P_L / sum(rank_FM$cnt_no_P_L),4);
rank_FM$rrate_perc <- percent(rank_FM$rrate)
rank_FM$cum_rel_P_L_perc <- percent(rank_FM$cum_rel_P_L)
rank_FM$cum_rel_no_P_L_perc <- percent(rank_FM$cum_rel_no_P_L)
rank_FM$cum_cnt<-cumsum(rank_FM$cnt)
rank_FM$cum_P_L_rate<-round(rank_FM$cum_P_L / rank_FM$cum_cnt,4)
overall_P_L_rate<-sum(as.integer(as.character(RFtrain$PersonalLoan)))/nrow(RFtrain)
rank_FM$ks = abs(rank_FM$cum_rel_P_L - rank_FM$cum_rel_no_P_L);

# Derive Lift
rank_FM$lift<-round(rank_FM$cum_P_L_rate/overall_P_L_rate,2)
View(rank_FM)
write.csv(rank_FM, "RankOrdering_RF.1.csv")
RFtest$deciles <- decile(RFtest$RF.Score)

## Ranking Code
inter_datatable_test_RF = data.table(RFtest)
rank_test_RF <- inter_datatable_test_RF[, list(
  cnt = length(as.integer(as.character(PersonalLoan))),
  cnt_P_L = sum(as.integer(as.character(PersonalLoan))),
  cnt_no_P_L = sum(as.integer(as.character(PersonalLoan)) == 0)) ,
  by=deciles][order(-deciles)]
rank_test_RF$rrate <- round(rank_test_RF$cnt_P_L / rank_test_RF$cnt,4);
rank_test_RF$cum_P_L <- cumsum(rank_test_RF$cnt_P_L)
rank_test_RF$cum_no_P_L <- cumsum(rank_test_RF$cnt_no_P_L)
rank_test_RF$cum_rel_P_L <- round(rank_test_RF$cum_P_L / sum(rank_test_RF$cnt_P_L),4);
rank_test_RF$cum_rel_no_P_L <- round(rank_test_RF$cum_no_P_L / sum(rank_test_RF$cnt_no_P_L),4);
rank_test_RF$rrate_perc <- percent(rank_test_RF$rrate)
rank_test_RF$cum_rel_P_L_perc <- percent(rank_test_RF$cum_rel_P_L)
rank_test_RF$cum_rel_no_P_L_perc <- percent(rank_test_RF$cum_rel_no_P_L)
rank_test_RF$cum_cnt<-cumsum(rank_test_RF$cnt)
rank_test_RF$cum_P_L_rate<-round(rank_test_RF$cum_P_L / rank_test_RF$cum_cnt,4)
overall_P_L_rate_test<-sum(as.integer(as.character(RFtest$PersonalLoan)))/nrow(RFtest)
rank_test_RF$ks = abs(rank_test_RF$cum_rel_P_L - rank_test_RF$cum_rel_no_P_L);

# Get Lift
rank_test_RF$lift<-round(rank_test_RF$cum_P_L_rate/overall_P_L_rate_test,2)
View(rank_test_RF)
write.csv(rank_test_RF, "RankOrdering_RF2.csv")

## Lift Graph for Rf Model
library(lift)
plotLift(RFtrain$RF.Score,RFtrain$PersonalLoan,cumulative = FALSE,n.buckets = 10)
plotLift(RFtest$RF.Score,RFtest$PersonalLoan,cumulative = FALSE,n.buckets = 10)

### KS Graph for RF Model
sample3<-rnorm(RFtrain$RF.Pred)
sample4<-rnorm(RFtest$RF.Pred)
group <- c(rep("sample3", length(sample3)), rep("sample4", length(sample4)))
dat <- data.frame(KSD = c(sample3,sample4), group = group)
cdf1 <- ecdf(sample3)
cdf2 <- ecdf(sample4)
minMax <- seq(min(sample3, sample4), max(sample3, sample4), length.out=length(sample3))
x0 <- minMax[which( abs(cdf1(minMax) - cdf2(minMax)) == max(abs(cdf1(minMax) - cdf2(minMax))) )]
y0 <- cdf1(x0)
y1 <- cdf2(x0)
ggplot(dat, aes(x = KSD, group = group, colour = group, linetype=group))+
  stat_ecdf(size=1) +
  xlab("mm") +
  ylab("Cumulitive Distibution") +
  geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
               linetype = "dashed", color = "red") +
  geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=1) +
  geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=1) +
  ggtitle("K-S Test: Sample 3/ Sample 4")
