#=======================================================================#
#Data Analysis  -   Cold Storage Problem 
#Developer      -   Tahmid Bari
#Date           -   April 11, 2020
#=======================================================================#


## Loading library ##
library(readr)
library(readxl)
library(dplyr)
library(rmarkdown)
# install.packages("readr")
# install.packages("readxl")
# Install package "dplyr" for data manipulation
# install.packages("dplyr")
# install.packages("rmarkdown")

# Set working directory
setwd("C:/Users/Tahmid Bari/Desktop/Great_Learning/R_Project/Cold_Storage_Problem")
getwd()

# Import the CSV into R
cold_storage_data_temp<-read.csv("Cold_Storage_Temp_Data.csv", header=TRUE)
View(cold_storage_data_temp)

# Create a new data sub-set with the required columns i.e. Season and temperature
seasons_temp<-select(cold_storage_data_temp,Season,Temperature)
View(seasons_temp)

# Filter and view seasons_temp dataset w.r.t winter, summer and rainy
winter_temp<-filter(seasons_temp, Season == "Winter")
summer_temp<-filter(seasons_temp, Season == "Summer")
rainy_temp<-filter(seasons_temp, Season == "Rainy")
View(winter_temp)
View(summer_temp)
View(rainy_temp)

# Get summary of winter, summary and rainy temperatures
summary(winter_temp)
summary(summer_temp)
summary(rainy_temp)
boxplot(seasons_temp$Temperature~seasons_temp$Season, horizontal = TRUE, col=c("Red", "Blue", "Orange"))

# Overall mean for the full year
mean(cold_storage_data_temp$Temperature)

# Standard deviation for the full year
sd(cold_storage_data_temp$Temperature)

# Probability of temperature having fallen below 2 deg C
mean_temp<-mean(cold_storage_data_temp$Temperature)
sd_temp<-sd(cold_storage_data_temp$Temperature)
X<-2
pnorm(X,mean_temp,sd_temp)

# Probability of temperature having gone above 4 deg C
Y<-4
prob<-1-pnorm(Y,mean_temp,sd_temp)
prob

# Penalty for AMC company
Xl<-2
Xu<-4
P_Xl<-pnorm(Xl,mean_temp,sd_temp)
P_Xu<-1-pnorm(Xu,mean_temp,sd_temp)
P_total<-P_Xl+P_Xu
P_total


# Perform a one-way ANOVA test to determine if there is a significant difference in Cold Storage
# temperature between rainy, summer and winter seasons and comment on the findings.
seasons_tempaov = aov(seasons_temp$Temperature~seasons_temp$Season, data = seasons_temp)
summary(seasons_temp)
TukeyHSD(seasons_tempaov)

## Problem 2: Use only the dataset: Cold_Storage_Mar2018.csv

# Which Hypothesis test shall be performed to check if corrective action is needed at the cold storage plant
# z-test
cold_storage_data_prob2 = read.csv("Cold_Storage_Mar2018.csv")
summary(cold_storage_data_prob2)

m2 = mean(cold_storage_data_prob2$Temperature)
m2

s2 = sd(cold_storage_data_prob2$Temperature)
s2

z_cal = (m2 - 3.9)/(s2/sqrt(35))
z_cal
pnorm(-abs(z_cal))

# State the Hypothesis, perform hypothesis test and determine p-value
# t-test
t.test(cold_storage_data_prob2$Temperature, mu = 3.9, alternative = "greater", conf.level = 0.9)
