#Loading essential libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(scales)
library(dplyr)
library(reshape2)
library(readxl)
library(corrplot)
library(Hmisc)
library(ggalt)
 
#Importing dataset (taken from kaggle)
data <- read.csv("C:/Users/heena/Downloads/diabetes1.csv")

#EDA
#Returns first 6 rows
head(data)
#Returns last 6 rows
tail(data)

#Returns dimensions of the dataset
dim(data)
#Returns number of rows and columns respectively
nrow(data)
ncol(data)

str(data)
#statistics of dataset
summary(data)

#apply(data, function)
#datatype of each column as vector
sapply(data, typeof)
#datatype of each column as list
lapply(data, typeof)


colnames(data)

#Returns NA values in each column
colSums(is.na(data))

#Replacing those values with median; na.rm for removing NA values
data$Glucose[is.na(data$Glucose)]=median(data$Glucose, na.rm=TRUE)
colSums(is.na(data))
head(data)
#We see the NA values replaced with median of column(here, 117).

#converting Outcome variable from int into factor for classification
data$Outcome <- as.factor(data$Outcome)
class(data$Outcome)

#frequency table for outcomes
table(data$Outcome)


##Data Visualization


#bar chart for comparison of people with and without diabetes
p1 <- ggplot(data,aes(x=Outcome)) +
geom_bar()
print(p1)
#We see an imbalance in the dataset;bias towards people without diabetes

#pie chart for comparison of people with and without diabetes
p1b <- ggplot(data, aes(x="",fill = Outcome)) + 
  geom_bar() +
  labs(fill="Outcome", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Outcomes", 
       caption="Source: data")

p1b + coord_polar(theta = "y", start=0)
#Ways to deal with it:Undersampling,oversampling,SMOTE

#box plot for BMI of people with and without diabetes
p2 <- ggplot(data, aes(x=Outcome,y=BMI,fill=Outcome))+
  geom_boxplot()
print(p2)
#We see BMI of non-diabetic patients is considerably lesser than those with diabetes.

#Using scatter plot to observe correlation between age and glucose; color based on Outcome
p3 <- ggplot(data, aes(x=Age,y=Glucose,col=Outcome))+
  geom_point()
p3+geom_smooth(method="loess")
#Glucose level of diabetic patients is seen to be higher.Also, as Age increases, a slight increase
#is seen in glucose levels.

#Scatterplot with Encircling
subset <- data[data$Glucose > 175 & data$Age<30,]
p4 <- ggplot(data, aes(x=Age,y=Glucose,col=Outcome))+
  geom_point()
p4+geom_smooth(method="loess")+
geom_encircle(aes(x=Age, y=Glucose), 
              data=subset, 
              color="red", 
              size=2, 
              expand=0.08) 
#Circled area shows youngest people with the highest Glucose levels, most have diabetes.

#Age vs BMI jitter plot
p5 <- ggplot(data, aes(x=Age,y=BMI))+
  geom_point()
p5+geom_jitter(width = .4, size=1)
#p5+geom_jitter(aes(colour=Outcome))
#Most young people have bmi in the range of 20-35, with exception of some outliers.

#Bubble plot for BMI vs Skin Thickness; also shows relationship with Outcome(color variation) 
#and Age(by variation in size of data points)
subset1 <- data[data$SkinThickness>30,]
theme_set(theme_bw())  # pre-set the bw theme.
p6 <- ggplot(subset1, aes(x=BMI,y=SkinThickness)) 

p6 + geom_jitter(aes(col=Outcome, size=Age)) + 
  geom_smooth(aes(col=Outcome), method="lm", se=F)
#As BMI increases, so does skin thickness (linear relationship). Also, skin thickness is lesser for older people.

# Bar graph for frequency of Pregnancies with respect to Outcome
p7 <- ggplot(data, aes(Pregnancies))
p7 + geom_bar(aes(group=Outcome)) + facet_wrap(~Outcome)


#Histogram for Blood Pressure
p8 <- ggplot(data, aes(BloodPressure)) + scale_fill_brewer(palette = "Spectral")

p8 + geom_histogram(aes(fill=Outcome), 
                   binwidth = 1, #width of bars
                   col="black", #color of boundary between bars
                   size=.5)   

p8 + geom_histogram(aes(fill=Outcome), 
                   bins=8, #number of bars
                   col="black", 
                   size=.5) 
#Most people had blood pressure around 60-70; and most of these people did not have diabetes.



#Histogram for all columns
hist.data.frame(data)

#Can BMI, Number of pregnancies influence Outcome?
p9 <- ggplot(data, aes(Outcome,BMI))
p9 + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .2, 
               fill="red") 
p9b <- ggplot(data, aes(Outcome,Pregnancies))
p9b + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .1, 
               fill="red")
#We see that people with diabetes have a higher BMI and number of pregnancies than those who tested negative.
#Thus, higher BMI would imply higher risk of diabetes.


#Correlation matrix
dat <- read.csv("C:/Users/heena/Downloads/diabetes1.csv")
num_vars <- unlist(lapply(dat, is.numeric))  
dia_nums <- data[ , num_vars]

dia_corr <- cor(dia_nums)
corrplot(dia_corr, method="color")
##Highest correlation between Glucose and Outcome.Also seen between age and pregnancies.
corrplot(dia_corr, method="number")
#for more specific results; do not have to reply on visual perception.

#Impact of Age over the Outcome(density plot)
ggplot(data,aes(x=Age,fill=Outcome))+geom_density(alpha=0.4)+scale_fill_manual(values=c("red", "blue"))
##People with diabetes were seen to be comparatively older.

#Is Outcome related to Diabetes Pedigree Function
p10 <- ggplot(data,aes(x=DiabetesPedigreeFunction))
p10 + geom_bar(aes(fill=Outcome))
#Most people who do not have diabetes were seen to have a Diabetes Pedigree Function less than 0.8.






