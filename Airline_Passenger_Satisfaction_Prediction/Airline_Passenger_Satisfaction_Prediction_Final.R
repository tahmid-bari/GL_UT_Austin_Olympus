#=======================================================================#
#Capstone Project   -   Airplane Passenger Satisfaction Prediction
#Developer          -   Tahmid Bari
#Date               -   September 28, 2020
#=======================================================================#

library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(forcats)
library(readxl)
library(haven)
library(broom)
library(psych)
library(gvlma)
library(jtools)
library(caret) 
library(rpart) 
library(rpart.plot) 
library(e1071) 
library(MASS) 
library(corrplot)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(tidyr) 
library(sparklyr)
library(mice)
library(missForest)
library(VIM)
#install.packages("mice")
#install.packages("missForest")
#install.packages("randomForest")
#install.packages("sparklyr")
#install.packages("lattice")
#install.packages("mnormt")
#install.packages("psych")
#install.packages("purr")
#install.packages("VIM")


## Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/Great Learning/Course Work/Capstone Project/Capstone_Project_Tahmid_Bari/Final Report")

## Check working directory
getwd()
Virgin <- read.csv("Virgin_Atlantic_Satisfaction.csv")
glimpse(Virgin)
head(Virgin)
str(Virgin)
summary(Virgin)
#The raw data contains 90917 rows (passengers) and 24 columns (features). The first step is to check the number of missing values in each column. 
#There are 393 missing values in "Arrival.Delay_in_Mins" columns. Prior to main data analysis, data cleaning should be performed first. 
#The next step was to remove all rows with missing values.

## Remove missing values
VS <- Virgin %>% 
  na.omit()     
dim(VS)
glimpse(VS)

sapply(Virgin, function(x) sum(is.na(x)))
Virgin <- Virgin[complete.cases(Virgin), ]
#In the original dataset, the zero entries in the columns Service1 to Service14 are "Not Applicable" responses by the passengers.
#Since such responses are treated as missing values, the next task is to remove the rows with such entries.
#From 90917 observations, the data contains 90637 (99.69%) observations with no missing values. 

## Missing value immputaion with mode

# For CustomerType
ct <- unique(Virgin$CustomerType[!is.na(Virgin$CustomerType)]) 
CustomerType <- ct[which.max(tabulate(match(Virgin$CustomerType, ct)))]
CustomerType
Virgin$CustomerType[is.na(Virgin$CustomerType)]<- CustomerType

# For TypeTravel
tt <- unique(Virgin$TypeTravel[!is.na(Virgin$TypeTravel)]) 
TypeTravel <- tt[which.max(tabulate(match(Virgin$TypeTravel, tt)))] 
TypeTravel
Virgin$TypeTravel[is.na(Virgin$TypeTravel)]<- TypeTravel

# For Departure.Arrival.time_convenient
datc <-unique(Virgin$Departure.Arrival.time_convenient[!is.na(Virgin$Departure.Arrival.time_convenient)])
Departure.Arrival.time_convenient <- datc[which.max(tabulate(match(Virgin$Departure.Arrival.time_convenient, datc)))]
Departure.Arrival.time_convenient
Virgin$Departure.Arrival.time_convenient[is.na(Virgin$Departure.Arrival.time_convenient)]<- Departure.Arrival.time_convenient

# For Food_drink
fd <- unique(Virgin$Food_drink[!is.na(Virgin$Food_drink)]) 
Food_drink <- fd[which.max(tabulate(match(Virgin$Food_drink, fd)))]
Food_drink
Virgin$Food_drink[is.na(Virgin$Food_drink)]<- Food_drink

# For Onboard_service
os <- unique(Virgin$Onboard_service[!is.na(Virgin$Onboard_service)]) 
Onboard_service <- os[which.max(tabulate(match(Virgin$Onboard_service, os)))]
Onboard_service
Virgin$Onboard_service[is.na(Virgin$Onboard_service)]<- Onboard_service


# Converting Cate into int

Virgin$Seat_comfort<-factor(Virgin$Seat_comfort,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Seat_comfort <- as.integer(factor(Virgin$Seat_comfort))

Virgin$Departure.Arrival.time_convenient<- factor(Virgin$Departure.Arrival.time_convenient,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Departure.Arrival.time_convenient <- as.integer(factor(Virgin$Departure.Arrival.time_convenient))

Virgin$Food_drink<-factor(Virgin$Food_drink,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Food_drink <- as.integer(factor(Virgin$Food_drink))

Virgin$Gate_location<-factor(Virgin$Gate_location,levels=c("very inconvinient","Inconvinient","need improvement","manageable","Convinient","very convinient"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Gate_location <- as.integer(factor(Virgin$Gate_location))

Virgin$Inflightwifi_service<-factor(Virgin$Inflightwifi_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Inflightwifi_service <- as.integer(factor(Virgin$Inflightwifi_service))

Virgin$Inflight_entertainment<-factor(Virgin$Inflight_entertainment,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Inflight_entertainment <- as.integer(factor(Virgin$Inflight_entertainment))

Virgin$Online_support<-factor(Virgin$Online_support,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Online_support <- as.integer(factor(Virgin$Online_support))

Virgin$Ease_of_Onlinebooking<-factor(Virgin$Ease_of_Onlinebooking,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Ease_of_Onlinebooking <- as.integer(factor(Virgin$Ease_of_Onlinebooking))

Virgin$Onboard_service<-factor(Virgin$Onboard_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Onboard_service <- as.integer(factor(Virgin$Onboard_service))

Virgin$Leg_room_service<-factor(Virgin$Leg_room_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Leg_room_service <- as.integer(factor(Virgin$Leg_room_service))

Virgin$Baggage_handling<-factor(Virgin$Baggage_handling,levels=c("poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4),ordered = T)
Virgin$Baggage_handling <- as.integer(factor(Virgin$Baggage_handling))

Virgin$Checkin_service<-factor(Virgin$Checkin_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Checkin_service <- as.integer(factor(Virgin$Checkin_service))

Virgin$Cleanliness<-factor(Virgin$Cleanliness,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Cleanliness <- as.integer(factor(Virgin$Cleanliness))

Virgin$Online_boarding<-factor(Virgin$Online_boarding,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Online_boarding <- as.integer(factor(Virgin$Online_boarding)) str(Virgin)

## Drop ID attribute
VS1 <- Virgin[-1]
summary(VS1)

# Finding correlation
VS1 <- VS1 %>% filter(!is.na(VS1$ArrivalDelayin_Mins))
for(i in 1:ncol(VS1.non)){
  if(class(VS1[,i])!="integer"){
    VS1.non[,i] <- as.integer(VS1.non[,i])
  }
}
VS1.cor <- round(cor(VS1.non[,1:7], VS1.non[,8]),3)
colnames(VS1.cor) <- "ArrivalDelayin_Mins"
VS1.cor

# Removing High Correlated atribute "Arrival.Delay.in.Minutes" 
VS1 <- VS1[-8]


#check for outliers
boxplot(VS1$Age,col='green',main='Boxplot for Age')
boxplot(VS1$Flight_Distance,col='yellow',main='Boxplot for Flight Distance')
boxplot(VS1$DepartureDelayin_Mins,col='red',main='Boxplot for Departure Delay in Minutes')
boxplot(VS1$ArrivalDelayin_Mins,col='pink',main='Boxplot for Arrival Delay in Minutes')
## There are outliers in the numerical variables but they seem to be possible values. Hence, we are not treating them.


## Exploratory Data Analysis

ggplot(VS1, aes(Satisfaction, group = Gender)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop..), stat= "count", vjust = -.5, cex=3) + labs(x="Satisfaction Level",y = "Percentage",title="Gender vs Satisfaction", fill="Satisfaction") +
  scale_y_continuous(labels=scales::percent) + facet_grid(~Gender)+ theme(axis.text.x = element_text(size=8,angle = 90), plot.title = element_text(hjust = 0.5))

ggplot(VS1, aes(Satisfaction, group = Class)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop..), stat= "count", vjust = -.5, cex=3) + labs(x="Class of Travel",y = "Percentage",title="Travel Class vs Satisfaction", fill="Satisfaction") + scale_y_continuous(labels=scales::percent) +
  facet_grid(~Class)+ theme(axis.text.x = element_text(size=8,angle = 90), plot.title = element_text(hjust = 0.5))

ggplot(VS1, aes(Satisfaction, group = Seat_comfort)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop..), stat= "count", vjust = -.5, cex=3) +
  labs(x="Seat Comfort Level",y = "Percentage",title="Seat Comfort vs Satisfaction", fill="Satisfaction") +
  scale_y_continuous(labels=scales::percent) + facet_grid(~Seat_comfort)+ theme(axis.text.x = element_text(size=8,angle = 90), plot.title = element_text(hjust = 0.5))

ggplot(VS1, aes(Satisfaction, group = Inflight_entertainment)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop..), stat= "count", vjust = -.5, cex=3) +
  labs(x="Inflight entertainment Level",y = "Percentage",title= "Inflight Entertainment vs Satisfaction", fill="Satisfaction") +
  scale_y_continuous(labels=scales::percent) + facet_grid(~Inflight_entertainment)+ theme(axis.text.x = element_text(size=8,angle = 90), plot.title = element_text(hjust = 0.5))

ggplot(VS1, aes(Satisfaction, group = Online_support)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop..), stat= "count", vjust = -.5, cex=3) +
  labs(x="Online support Level",y = "Percentage",title="Online Support vs Satisfaction", fill="Satisfaction") +
  scale_y_continuous(labels=scales::percent) + facet_grid(~Online_support)+ theme(axis.text.x = element_text(size=8,angle = 90), plot.title = element_text(hjust = 0.5))

ggplot(VS1, aes(Satisfaction, group = Baggage_handling)) + geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..prop..), stat= "count", vjust = -.5, cex=3) +
  labs(x="Baggage Handling Level",y = "Percentage",title="Baggage Handling vs Satisfaction", fill="Satisfaction") +
  scale_y_continuous(labels=scales::percent) + facet_grid(~Baggage_handling)+ theme(axis.text.x = element_text(size=8,angle = 90), plot.title = element_text(hjust = 0.5))



# Generate 10% missing values at Random
Virgin.mis <- prodNA(Virgin, noNA = 0.1)

# Check missing values introduced in the data
summary(Virgin.mis)

# I've removed categorical variable.
# Let's here focus on continuous values.
# To treat categorical variable, simply encode the levels and follow the procedure below.

# Remove categorical variables
Virgin.mis <- subset(Virgin.mis, select = -c(Species))
summary(Virgin.mis)

# mice package has a function known as md.pattern().
# It returns a tabular form of missing value present in each variable in a data set.

md.pattern(Virgin.mis)
mice_plot <- aggr(Virgin.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Now, let's impute the missing values.
imputed_Data <- mice(Virgin.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# Multiply imputed data set
# Call:
mice(data = Virgin.mis, m = 5, method = "pmm", maxit = 50, seed = 500)

# Check imputed values
imputed_Data$imp$Sepal.Width

# Get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data,2)

# Build predictive model
fit <- with(data = Virgin.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))

# Combine results of all 5 models
combine <- pool(fit)
summary(combine)
library(VIM)
aggr_plot <- aggr(Virgin.mis, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(Virgin.mis[c(1,2)])
xyplot(Virgin.mis,Ozone ~ Wind+Temp+Solar.R,pch=18,cex=1)
stripplot(Virgin.mis, pch = 20, cex = 1.2)

## Number of observations per patterns for all pairs of variables
Virgin.mis <- md.pairs(Virgin.mis)

## Margin plot of y1 and y4
marginplot(Virgin.mis[c(5, 8)], col = c("blue", "red", "orange"))

## distributions of missing variable by another specified variable
pbox(Virgin.mis, pos = 2)

## By default it does 5 imputations for all missing values
imp1 <- mice(Virgin.mis, m = 5)

imp_het_sri <- mice(Virgin.mis, method = "norm.nob", m = 1)
data_het_sri <- complete(Virgin.mis)
imp_het_pmm <- mice(Virgin.mis, method = "pmm", m = 1)
data_het_pmm <- complete(Virgin.mis)

## Remove missing values
VS <- Virgin %>% 
  na.omit()     
dim(VS)
glimpse(VS)

sapply(Virgin, function(x) sum(is.na(x)))
Virgin <- Virgin[complete.cases(Virgin), ]
#In the original dataset, the zero entries in the columns Service1 to Service14 are "Not Applicable" responses by the passengers.
#Since such responses are treated as missing values, the next task is to remove the rows with such entries.
#From 90917 observations, the data contains 90637 (99.69%) observations with no missing values. 

## Missing value immputaion with mode

# For CustomerType
ct <- unique(Virgin$CustomerType[!is.na(Virgin$CustomerType)]) 
CustomerType <- ct[which.max(tabulate(match(Virgin$CustomerType, ct)))]
CustomerType
Virgin$CustomerType[is.na(Virgin$CustomerType)]<- CustomerType

# For TypeTravel
tt <- unique(Virgin$TypeTravel[!is.na(Virgin$TypeTravel)]) 
TypeTravel <- tt[which.max(tabulate(match(Virgin$TypeTravel, tt)))] 
TypeTravel
Virgin$TypeTravel[is.na(Virgin$TypeTravel)]<- TypeTravel

# For Departure.Arrival.time_convenient
datc <-unique(Virgin$Departure.Arrival.time_convenient[!is.na(Virgin$Departure.Arrival.time_convenient)])
Departure.Arrival.time_convenient <- datc[which.max(tabulate(match(Virgin$Departure.Arrival.time_convenient, datc)))]
Departure.Arrival.time_convenient
Virgin$Departure.Arrival.time_convenient[is.na(Virgin$Departure.Arrival.time_convenient)]<- Departure.Arrival.time_convenient

# For Food_drink
fd <- unique(Virgin$Food_drink[!is.na(Virgin$Food_drink)]) 
Food_drink <- fd[which.max(tabulate(match(Virgin$Food_drink, fd)))]
Food_drink
Virgin$Food_drink[is.na(Virgin$Food_drink)]<- Food_drink

# For Onboard_service
os <- unique(Virgin$Onboard_service[!is.na(Virgin$Onboard_service)]) 
Onboard_service <- os[which.max(tabulate(match(Virgin$Onboard_service, os)))]
Onboard_service
Virgin$Onboard_service[is.na(Virgin$Onboard_service)]<- Onboard_service


# Converting into int

Virgin$Seat_comfort<-factor(Virgin$Seat_comfort,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Seat_comfort <- as.integer(factor(Virgin$Seat_comfort))

Virgin$Departure.Arrival.time_convenient<- factor(Virgin$Departure.Arrival.time_convenient,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Departure.Arrival.time_convenient <- as.integer(factor(Virgin$Departure.Arrival.time_convenient))

Virgin$Food_drink<-factor(Virgin$Food_drink,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Food_drink <- as.integer(factor(Virgin$Food_drink))

Virgin$Gate_location<-factor(Virgin$Gate_location,levels=c("very inconvinient","Inconvinient","need improvement","manageable","Convinient","very convinient"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Gate_location <- as.integer(factor(Virgin$Gate_location))

Virgin$Inflightwifi_service<-factor(Virgin$Inflightwifi_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Inflightwifi_service <- as.integer(factor(Virgin$Inflightwifi_service))

Virgin$Inflight_entertainment<-factor(Virgin$Inflight_entertainment,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Inflight_entertainment <- as.integer(factor(Virgin$Inflight_entertainment))

Virgin$Online_support<-factor(Virgin$Online_support,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Online_support <- as.integer(factor(Virgin$Online_support))

Virgin$Ease_of_Onlinebooking<-factor(Virgin$Ease_of_Onlinebooking,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Ease_of_Onlinebooking <- as.integer(factor(Virgin$Ease_of_Onlinebooking))

Virgin$Onboard_service<-factor(Virgin$Onboard_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Onboard_service <- as.integer(factor(Virgin$Onboard_service))

Virgin$Leg_room_service<-factor(Virgin$Leg_room_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Leg_room_service <- as.integer(factor(Virgin$Leg_room_service))

Virgin$Baggage_handling<-factor(Virgin$Baggage_handling,levels=c("poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4),ordered = T)
Virgin$Baggage_handling <- as.integer(factor(Virgin$Baggage_handling))

Virgin$Checkin_service<-factor(Virgin$Checkin_service,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Checkin_service <- as.integer(factor(Virgin$Checkin_service))

Virgin$Cleanliness<-factor(Virgin$Cleanliness,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Cleanliness <- as.integer(factor(Virgin$Cleanliness))

Virgin$Online_boarding<-factor(Virgin$Online_boarding,levels=c("extremely poor","poor","need improvement","acceptable","good","excellent"),labels=c(0,1,2,3,4,5),ordered = T)
Virgin$Online_boarding <- as.integer(factor(Virgin$Online_boarding)) str(Virgin)

## Drop ID attribute
VS1 <- Virgin[-1]
summary(VS1)

# Finding correlation
VS1 <- VS1 %>% filter(!is.na(VS1$ArrivalDelayin_Mins))
for(i in 1:ncol(VS1.non)){
  if(class(VS1[,i])!="integer"){
    VS1.non[,i] <- as.integer(VS1.non[,i])
  }
}
VS1.cor <- round(cor(VS1.non[,1:7], VS1.non[,8]),3)
colnames(VS1.cor) <- "ArrivalDelayin_Mins"
VS1.cor

# Removing High Correlated atribute "Arrival.Delay.in.Minutes" 
VS1 <- VS1[-8]


# Check for outliers
boxplot(VS1$Age,col='green',main='Boxplot for Age')
boxplot(VS1$DepartureDelayin_Mins,col='red',main='Boxplot for Departure Delay in Minutes')
boxplot(VS1$ArrivalDelayin_Mins,col='pink',main='Boxplot for Arrival Delay in Minutes')

## There are outliers in the numerical variables but they seem to be possible values. Hence, we are not treating them.
## Now, let's impute the missing values.

imputed_Data <- mice(Virgin.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# Multiply imputed data set
# Call:
mice(data = Virgin.mis, m = 5, method = "pmm", maxit = 50, seed = 500)

# Check imputed values
imputed_Data$imp$Sepal.Width

# Get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data,2)

# Build predictive model
fit <- with(data = Virgin.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))

## Data Modelling
round(prop.table(table(data$Satisfaction))*100,digit=1) for(i in 2:ncol(data)){
  if(class(data[,i])=="factor"){
    data[,i] <- as.integer(data[,i])
  }
}
normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
  
}

# Write.csv(data, file = "Data.csv")
data <- read.csv("Data.csv",header = T) data <- data[,-1]
str(data)

# Converting categorical to int
# Gender levels(data$Gender)
data$Gender<-factor(data$Gender,levels=c("Female","Male"),labels=c(0,1)) data$Gender <- as.integer(factor(data$Gender))

# CustomerType levels(data$CustomerType)
data$CustomerType<-factor(data$CustomerType,levels=c("disloyal Customer","Loyal Customer"),labels=c(0,1),ordered = T)
data$CustomerType <- as.integer(factor(data$CustomerType))


#TypeTravel levels(data$TypeTravel)
data$TypeTravel<-factor(data$TypeTravel,levels=c("Business travel","Personal Travel"),labels=c(0,1)) data$TypeTravel <- as.integer(factor(data$TypeTravel))

#Class levels(data$Class)
data$Class<-factor(data$Class,levels=c("Eco","Eco Plus","Business"),labels=c(0,1,2),ordered = T)

data$Class <- as.integer(factor(data$Class))
data_n <- as.data.frame(lapply(data[2:22], normalize)) data_n <- cbind(data_n, Satisfaction=data$Satisfaction)

set.seed(5)
partition <- createDataPartition(y = data_n$Satisfaction, p=0.7, list= F) data_train <- data_n[partition,]
data_test <- data_n[-partition,]


# Logistic regression

log_model <- glm(Satisfaction~., data=data_train, family = "binomial") summary(log_model)
log_preds <- predict(log_model, data_test[,1:22], type = "response") head(log_preds)
log_class <- array(c(99))
for (i in 1:length(log_preds)){ if(log_preds[i]>0.5){ log_class[i]<-"satisfied"
}else{
  log_class[i]<-"neutral or dissatisfied"
}
}


#Creating a new dataframe containing the actual and predicted values. 
log_result <- data.frame(Actual = data_test$Satisfaction, Prediction = log_class)
mr1 <- confusionMatrix(as.factor(log_class),data_test$Satisfaction, positive = "satisfied") mr1
# Linear Discriminant Analysis


pairs(data_train[,1:5], main="Predict ",  pch=22, bg=c("red", "blue")[unclass(data_train$Satisfaction)]) lda_model <- lda(Satisfaction ~ ., data_train)
lda_model


# Predict the model
lda_preds <- predict(lda_model, data_test)
mr2 <- confusionMatrix(data_test$Satisfaction, lda_preds$class, positive = "satisfied") mr2


# Decision tree - CART algorithm
data_train_new <- data_train
for(i in c(1,2,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20,21)){
  data_train_new[,i] <- as.factor(data_train_new[,i])
}

data_test_new <- data_test
for(i in c(1,2,4,5,8,9,10,11,12,13,14,15,16,17,18,19,20,21)){
  data_test_new[,i] <- as.factor(data_test_new[,i])
}


str(data_train_new) str(data_test_new)
cart_model <- rpart(Satisfaction~ ., data_train_new, method="class") cart_model
rpart.plot(cart_model,digits = 2, type=4, extra=106)

cm1 <- predict(cart_model, data_test_new, type = "class")
mr3 <- confusionMatrix(cm1, data_test$Satisfaction, positive = "satisfied") mr3


# Model Comparision
comp_cfm <- function(x1,x2,x3,n1,n2,n3){ a <- data.frame()
b <- as.matrix(c(x1[[3]][1:2],x1[[4]][1:2]))
c <- as.matrix(c(x2[[3]][1:2],x2[[4]][1:2]))
d <- as.matrix(c(x3[[3]][1:2],x3[[4]][1:2]))
colnames(b) <- n1 colnames(c) <- n2 colnames(d) <- n3
a <- as.data.frame(cbind(b,c,d)) return(a)
}

r <- comp_cfm(mr1,mr2,mr3, "LogisticRegression", "LinearDiscriminantAna lysis", "DecisionTree") r

## Capstone Project Data And Naive Bayes Model
setwd("C:/Users/Tahmid Bari/Desktop/Great Learning/Course Work/Capstone Project/Capstone_Project_Tahmid_Bari/Final Report")

survey<-read.csv("Surveydata.csv")
flight<- read.csv("Virgin_Atlantic_Satisfaction.csv")

## Combine Data set
FSdata<-cbind(flight[-1],survey[-1]) str(FSdata)
colnames(FSdata) library(dplyr)

## filling "Blanks" with "NA" empty_as_na <- function(x)
{
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors ifelse(as.character(x)!="", x, NA)
}

## Transform all columns
FSdata<-FSdata %>% mutate_each(list(empty_as_na)) FSdata

## Missing value imputation for CustomerType
val1 <- unique(FSdata$CustomerType[!is.na(FSdata$CustomerType)]) mode1 <- val1[which.max(tabulate(match(FSdata$CustomerType, val1)))] mode1

FSdata$CustomerType[is.na(FSdata$CustomerType)]<-     mode1

## Missing value imputation for Typetravel
val2 <- unique(FSdata$TypeTravel[!is.na(FSdata$TypeTravel)])
mode2 <- val2[which.max(tabulate(match(FSdata$TypeTravel, val2)))] mode2

FSdata$TypeTravel[is.na(FSdata$TypeTravel)]<-     mode2


## Missing value imputation for Departure.Arrival.time_convenient
val3  <-unique(FSdata$Departure.Arrival.time_convenient[!is.na(FSdata$Departure.Arrival.time_convenient)]) 
mode3 <- val3[which.max(tabulate(match(FSdata$Departure.Arrival.time_convenient, val3)))]
mode3

FSdata$Departure.Arrival.time_convenient[is.na(FSdata$Departure.Arrival.time_convenient)]<-mode3


## Missing value imputation for Food_drink
val4 <- unique(FSdata$Food_drink[!is.na(FSdata$Food_drink)])
mode4 <- val4[which.max(tabulate(match(FSdata$Food_drink, val4)))] mode4

FSdata$Food_drink[is.na(FSdata$Food_drink)]<-     mode4


## Missing value imputation for Onboard_service
val5 <- unique(FSdata$Onboard_service[!is.na(FSdata$Onboard_service)]) mode5 <- val5[which.max(tabulate(match(FSdata$Onboard_service, val5)))] mode5
FSdata$Onboard_service[is.na(FSdata$Onboard_service)]<-      mode5

## Deteling the variable "Arrival Delay in Mins" due to collinearity FSdata<-FSdata[-8]

colnames(FSdata) write.csv(FSdata,file = "FSdata.csv")
sum(is.na(FSdata))

## Binning library(rbin)
FSdata$Age <- cut(FSdata$Age,
                  c(0,20,35,55,90))
table(FSdata$Age) prop.table(table(FSdata$Age)) levels(FSdata$Age)

## Binning Flight_Distance
FSdata$Flight_Distance <- cut(FSdata$Flight_Distance,
                              
                              
                              c(0,200,400,600,800,1000,1200,1400,1600,1800,2000,2200,2400,2600,2800,3000,3200,3400,3600,3800,4000,4200,44 00,4600,4800,5000,5200,5400,5600,5800,6000))
table(FSdata$Flight_Distance) prop.table(table(FSdata$Flight_Distance)) levels(FSdata$Flight_Distance)

##Binning DepartureDelayin_Mins (First 2 hrs every 10 mins, next 3 hrs every 15 mins,
#next 5 hrs every 30 mins, next 6 hrs every 60 mins, and beyond 6 hrs in the last bucket) FSdata$DepartureDelayin_Mins <- cut(FSdata$DepartureDelayin_Mins,
c(0,10,20,30,40,50,60,70,80,90,100,110,120,135,150, 165,180,195,210,225,240,255,270,285,300,330,360,390,
  420,450,480,510,540,570,600,660,720,780,840,900,960,1620))
table(FSdata$DepartureDelayin_Mins) prop.table(table(FSdata$DepartureDelayin_Mins))

## Converting 0 as string
levels<-levels(FSdata$DepartureDelayin_Mins)

levels[length(levels)+1]<-"0"
FSdata$DepartureDelayin_Mins<-factor(FSdata$DepartureDelayin_Mins,levels       =levels) FSdata$DepartureDelayin_Mins[is.na(FSdata$DepartureDelayin_Mins)]<-      "0"

str(FSdata)

## Converting Character variables to factor
FSdata$Gender<- factor(FSdata$Gender,c("Female","Male"),labels = c(0,1)) FSdata$CustomerType <- factor(FSdata$CustomerType,
                                                                                                       c("Loyal Customer","disloyal Customer"),labels = c(1,0)) FSdata$TypeTravel<-    factor(FSdata$TypeTravel,
                                                                                                                                                                                              c("Personal Travel", "Business travel"),labels = c(0,1))   FSdata$Class <- factor(FSdata$Class,c("Eco","Eco Plus","Business"),labels = c(1:3)) FSdata$Food_drink <- factor(FSdata$Food_drink,
                                                                                                                                                                                                                                                                                                                                                                         c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Seat_comfort <- factor(FSdata$Seat_comfort,
                              c( "extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Departure.Arrival.time_convenient    <-    factor(FSdata$Departure.Arrival.time_convenient, c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Gate_location <- factor(FSdata$Gate_location,
                               c("very inconvinient","Inconvinient","need improvement","manageable", "Convinient" , "very convinient"),labels = c(0:5))
FSdata$Inflightwifi_service<-     factor(FSdata$Inflightwifi_service,
                                         c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Inflight_entertainment <- factor(FSdata$Inflight_entertainment,
                                        c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Online_support <- factor(FSdata$Online_support,
                                
                                c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Ease_of_Onlinebooking   <-   factor(FSdata$Ease_of_Onlinebooking, c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Onboard_service <- factor(FSdata$Onboard_service,
                                 c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Leg_room_service<-     factor(FSdata$Leg_room_service,
                                     c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Baggage_handling<-     factor(FSdata$Baggage_handling,
                                     c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Checkin_service <- factor(FSdata$Checkin_service,
                                 c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))

FSdata$Cleanliness<-    factor(FSdata$Cleanliness,
                               c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Online_boarding <- factor(FSdata$Online_boarding,
                                 c("extremely poor","poor","need improvement","acceptable", "good" , "excellent"),labels = c(0:5))
FSdata$Satisfaction<-factor(FSdata$Satisfaction,
                            c("satisfied","neutral or dissatisfied"),labels=c(1,0))


levels(FSdata$Cleanliness)
str(FSdata)

## Split the data in train & test sample (70:30) library(caret)
set.seed(123)
id<-sample(2,nrow(FSdata),prob = c(.7,.3),replace = T) training<-FSdata[id==1,]
testing<-FSdata[id==2,]

c(nrow(training), nrow(testing)) str(testing)

## Rows and Columns dim(training) dim(testing)
## Columns name colnames(training) colnames(testing)

## Naive Baise Model
## library(e1071)
## library(caret)
## library(AUC)
model.bayes<- naiveBayes(Satisfaction~.,data = training) model.bayes


## Test pc<- NULL
pc<- predict(model.bayes,testing,type = "class") summary(pc)
confusionMatrix(table(pc,testing$Satisfaction))


## Lift Chart pb<- NULL
pb<- predict(model.bayes,testing,type = "raw") pb<-as.data.frame(pb)
pred.bayes<-data.frame(testing$Satisfaction,pb$`1`) colnames(pred.bayes)<-c("target","score")
lift.bayes<- lift(target~score,data=pred.bayes,cuts = 10,class = "1") xyplot(lift.bayes,main="Bayesian Classifier -Lift Chart",
                                                                             type=c("1","g"),lwd=2, x.scales=list(x=list(alternative=FALSE,tick.number=10),
                                                                                                                  y=list(alternating=FALSE,tick.number=10)))

