## Setting up Working Directory
setwd("C:/Users/Tahmid Bari/Desktop/Great_Learning/R_Project/Cardio_Good_Fitness")
getwd()

## The package (dplyr) provides convenient tools for the most common data manipulation tasks
require(pacman)
p_load(dplyr)

## Reading a file
Cardio <- read.csv("CardioGoodFitness.csv")

?dim
## Variabe Identifcation
dim(Cardio)
?head

head(Cardio)
tail(Cardio)

## Carrying basic descriptive stats
?str
str(Cardio)
summary(Cardio)
View(Cardio)

library(data.table)

# Resource Link: https://cengel.github.io/R-data-wrangling/data-visualization-with-ggplot2.html
# Univariate Analysis: Using ggplot - box plot

# 3D Scatterplot
library(rgl)
library(scatterplot3d)
attach(Cardio)
scatterplot3d(Age, Education, Usage, main="3D Scatterplot", color="steelblue")

library(ggplot2)
ggplot(Cardio)

ggplot(Cardio, aes(x = Education, y = Age)) +        # This is the plot function
  geom_point(fill="steelblue")         # This is the geom for box plot in ggplot.

ggplot(Cardio, aes(x = Age, y = Education)) +  # This is the plot function
  geom_point(fill="purple")         # This is the geom for box plot in ggplot.

ggplot(Cardio, aes(y = Usage)) +      # This is the plot function
  geom_boxplot(fill="purple")         # This is the geom for box plot in ggplot.

ggplot(Cardio, aes(y = Fitness)) +    # This is the plot function
  geom_boxplot(fill="purple")         # This is the geom for box plot in ggplot.

ggplot(Cardio, aes(y = Income)) +     # This is the plot function
  geom_boxplot(fill="purple")         # This is the geom for box plot in ggplot.

ggplot(Cardio, aes(y = Miles)) +      # This is the plot function
  geom_boxplot(fill="purple")         # This is the geom for box plot in ggplot.


# Univariate Analysis: Using ggplot - histogram
ggplot(data = Cardio)

ggplot(Cardio, aes(Age)) +        # This is the plot function
  geom_histogram(fill="purple")   # This is the geom for histogram plot in ggplot.

ggplot(Cardio, aes(Education)) +  # This is the plot function
  geom_histogram(fill="purple")   # This is the geom for histogram plot in ggplot.

ggplot(Cardio, aes(Usage)) +      # This is the plot function
  geom_histogram(fill="purple")   # This is the geom for histogram plot in ggplot.

ggplot(Cardio, aes(Fitness)) +    # This is the plot function
  geom_histogram(fill="purple")   # This is the geom for histogram plot in ggplot.

ggplot(Cardio, aes(Income)) +     # This is the plot function
  geom_histogram(fill="purple")   # This is the geom for histogram plot in ggplot.

ggplot(Cardio, aes(Miles)) +        # This is the plot function
  geom_histogram(fill="purple")   # This is the geom for histogram plot in ggplot.


# Univariate Analysis
?boxplot

qplot(Cardio$Age, geom="histogram")
qplot(Cardio$Age, geom="boxplot")
boxplot(Cardio$Age,main="Age")
hist(Cardio$Age)


qplot(Cardio$Education, geom="histogram")
qplot(Cardio$Education, geom="boxplot")
boxplot(Cardio$Education,main="Education")
hist(Cardio$Education)


qplot(Cardio$Usage, geom="histogram")
qplot(Cardio$Usage, geom="boxplot")
boxplot(Cardio$Usage,main="Usage")
hist(Cardio$Usage)


qplot(Cardio$Fitness, geom="histogram")
qplot(Cardio$Fitness, geom="boxplot")
boxplot(Cardio$Fitness,main="Fitness")
hist(Cardio$Fitness)


qplot(Cardio$Income, geom="histogram")
qplot(Cardio$Income, geom="boxplot")
boxplot(Cardio$Income,main="Income")
hist(Cardio$Income)


qplot(Cardio$Miles, geom="histogram")
qplot(Cardio$Miles, geom="boxplot")
boxplot(Cardio$Miles,main="Miles")
hist(Cardio$Miles)

library(ggplot2)
ggplot(Cardio, aes(x = Age)) +
  geom_histogram(binwidth = 200)
# Histogram is heavily skewed

# ggplot
ggplot(Cardio,aes(x=Age, y=Income, fill=Education)) +
  geom_boxplot(fill = "purple")


boxplot(Cardio[,c(2,4,6,7,8,9)])


### bi variate

# Product vs Age
plot(Cardio$Product,Cardio$Age, xlab= "Product", ylab="Age", main ="Product vs Age")

# Product vs Gender
plot(Cardio$Product,Cardio$Gender, xlab= "Product", ylab="Gender", main ="Product vs Gender")

# Product vs Education
plot(Cardio$Product,Cardio$Education, xlab= "Product", ylab="Education", main ="Product vs Education")

# Product vs Matialstatus
plot(Cardio$Product,Cardio$MaritalStatus, xlab= "Product", ylab="MartialStatus", main ="Product vs MartialStatus")

# Product vs Usage
plot(Cardio$Product,Cardio$Usage, xlab= "Product", ylab="usage", main ="Product vs Usage")

# Product vs Fitness
plot(Cardio$Product,Cardio$Fitness, xlab= "Product", ylab="Fitness", main ="Product vs Fitness")

# Product vs Income
plot(Cardio$Product,Cardio$Income, xlab= "Product", ylab="Income", main ="Product vs Income")

# Product vs Mile
plot(Cardio$Product,Cardio$Miles, xlab= "Product", ylab="Mile", main ="Product vs Mile")

##################################################

plot(Cardio$Age, Cardio$Education, xlab= "Age", ylab="Education", main ="Age vs Education")
abline(lm(Cardio$Education~Cardio$Age),col=c("Red"))

plot(Cardio$Age, Cardio$Usage, xlab= "Age", ylab="Usage", main ="Age vs Usage")
abline(lm(Cardio$Usage~Cardio$Age),col=c("Red"))

plot(Cardio$Gender, Cardio$Usage, xlab= "Gender", ylab="Usage", main ="Gender vs Usage")

plot(Cardio$Fitness, Cardio$Usage, xlab= "Fitness", ylab="Usage", main ="Fitness vs Usage")
abline(lm(Cardio$Usage~Cardio$Fitness),col=c("Red"))

plot(Cardio$Gender, Cardio$Income, xlab= "Gender", ylab="Income", main ="Gender vs Income")


plot(Cardio$Gender, Cardio$Miles, xlab= "Gender", ylab="Miles", main ="Gender vs Miles")

####################################################