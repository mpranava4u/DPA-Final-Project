#' ---
#' title : " Project "
#' date : 4-12-2022
#' author : Karan, Sahil,Pranava,Vidhi
#' ---

#'Importing packages
library(readr)
library(data.table)
library(randomForest)
library(caret)
library(rpart)
library(e1071)
library(party)
library(Epi)
library(ROCR)
library(car)
library(dplyr)
library(VIM)
library(caTools)
library(plotly)
library(scales) 
library(stringr) 
library(ggthemes) 

#' Import Data
data = read_csv("C:/Users/karan/OneDrive/Desktop/IIT/Data Preparation and Analysis/Group Project/bank-full.csv")

head(data)
str(data)
summary(data)

#Checking dimensions of data 
dim(data)

#' Exploratory Data Analysis
# Yes and Output Analysis

data %>% 
  count(y) %>% 
  mutate(perc = n / nrow(data)) -> predictorData2

ggplot(predictorData2, aes(x = y, y = perc)) + geom_bar(stat = "identity")

# Job type analysis

data %>% 
  count(job) %>% 
  mutate(perc = n / nrow(data)) -> bank3

ggplot(bank3, aes(x = job, y = perc)) + geom_bar(stat = "identity")

# Job type analysis with output field

ggplot(data, 
       aes(x = job,
           fill = y)) + 
  geom_bar(position = "dodge")

# Marital type analysis

data %>% 
  count(marital) %>% 
  mutate(perc = n / nrow(data)) -> bank3

ggplot(bank3, aes(x = marital, y = perc)) + geom_bar(stat = "identity")

# Marital type analysis with output field

ggplot(data, 
       aes(x = marital,
           fill = y)) + 
  geom_bar(position = "dodge")

# Defaulter by bank field analysis 

data %>% 
  count(default) %>% 
  mutate(perc = n / nrow(data)) -> bank3

ggplot(bank3, aes(x = default, y = perc)) + geom_bar(stat = "identity")

# Defaulter by bank analysis with output field

ggplot(data, 
       aes(x = default,
           fill = y)) + 
  geom_bar(position = "dodge")

# Housing field analysis 

data %>% 
  count(housing) %>% 
  mutate(perc = n / nrow(data)) -> bank3

ggplot(bank3, aes(x = housing, y = perc)) + geom_bar(stat = "identity")

# Housing field analysis  with output field

ggplot(data, 
       aes(x = housing,
           fill = y)) + 
  geom_bar(position = "dodge")

# Previous marketing outcome analysis

data %>% 
  count(poutcome) %>% 
  mutate(perc = n / nrow(data)) -> bank3

ggplot(bank3, aes(x = poutcome, y = perc)) + geom_bar(stat = "identity")

# Previous marketing outcome  analysis with output field

ggplot(data, 
       aes(x = poutcome,
           fill = y)) + 
  geom_bar(position = "dodge")

# Month fields analysis shows in which month bank contact more with customers\
# Month Note- day of the week is required calculation and its not useful data as usually in any banking sector performance is calculated monthly and quarterly or yearly.

data %>% 
  count(month) %>% 
  mutate(perc = n / nrow(data),Month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                                                "jul", "aug", "sep", "oct", "nov", "dec"))) -> bank3

ggplot(bank3, aes(x = Month, y = perc)) + geom_bar(stat = "identity")

# Month fields analysis shows in which month bank contact more with customers with output fields

bankMutate <- data %>%
  mutate(Month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", 
                                          "jul", "aug", "sep", "oct", "nov", "dec")))

ggplot(bankMutate, 
       aes(x = Month,
           fill = y)) + 
  geom_bar(position = "dodge")

# Call duration analysis

ggplot_duration<- ggplotly(ggplot(data, aes(x=as.factor(y), y=duration)) +
                             geom_boxplot(fill='#A4A4A4', color="black")+
                             theme_classic())
ggplot_duration


#'' Data Manipulation and Descriptive Analysis
# Unique values in different columns
data %>% distinct(job)
data %>% distinct(marital)
data %>% distinct(education)
data %>% distinct(default)
data %>% distinct(loan)
data %>% distinct(contact)
data %>% distinct(poutcome)
data %>% distinct(y)


# Job Column - Spelling error
data$job<-gsub(".", "", data$job, fixed = TRUE)

# Checking missing values in data set
colSums(is.na.data.frame(data))
# No missing values in data

# Substitute unknowns in the data set with NA
data[data == "unknown"] <- NA


#' Data Preparation
# Converting categorical target variable in yes/no form:
data$y<-ifelse(data$y=="yes",1,0)

#' Splitting Data
set.seed(1)

# Use 70% of data set as training set and 30% as test set
sample <- sample.split(data$y, SplitRatio = 0.7)
train  <- subset(data, sample == TRUE)
test   <- subset(data, sample == FALSE)
train<-train %>% mutate(y=as.factor(y))
test<-test %>% mutate(y=as.factor(y))

# Splitting train into X and Y train
x_train<-as.data.frame(train) %>% select(-y)
y_train<-train$y
# Splitting test into X and Y train
x_test<-as.data.frame(test) %>% select(-y)
y_test<-test$y

#' Imbalanced Data set
# We need to balance our data
table(y_train)

set.seed(123)
train_downsample <- downSample(x = x_train,y = y_train,yname = "y")
train_downsample<-train_downsample %>% mutate(y=as.factor(y))

#' Logistic Regression
model_full<-glm(y~.,train_downsample, family = "binomial")
summary(model_full)

# Important coefficients in Model
varImp(model_full, scale = FALSE)

# Predictions
predicted_log<-predict(model_full,x_test,type="response")
pred_log_test <- ifelse(predicted_log > 0.5, 1, 0) %>% as.factor()
cm_log<-confusionMatrix(pred_log_test,y_test,positive = "1")
cm_log

#' Naive Bayes Classifier
model_naive <- naiveBayes(formula = y ~ ., data = train_downsample,laplace=1)
summary(model_naive)

# Predictions
naive_pred = predict(model_naive,x_test,type="class" )
naive_log = confusionMatrix(table(naive_pred,y_test))
naive_log
