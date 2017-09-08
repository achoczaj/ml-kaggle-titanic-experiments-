# Model 1a:  Data Science Dojo
# SScore: 0.77512


# ideas for model's optimization:
# Fare - replace missing valuses with a mode value from titanic.full
# Age - replace missing valuses with a mode value from titanic.full


# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")


# Import data.
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# Compare structure of these datasets.
str(titanic.train)
str(titanic.test)


# Prepare titanic.test to be combine with titanic.train.
# Add column Survived in test dataset and set it as NA.
titanic.test$Survived <- NA

# Prepare datasets to be combined.
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.full <- rbind(titanic.train, titanic.test)
View(titanic.full)


# Check data to see if there are missing values.

# TBD - find better way better to find mis val 

# length(which(!complete.cases(titanic.train)))
#[1] 177
# table(is.na(titanic.full))
## 418 NA is for added titanic.test$Survived  





#1. clean missing values of Embarked class
table(titanic.full$Embarked)
# there are 2 missing values
# mode value is 'S'
# replace missing valuses with a mode value

# cleaning missing values with a filter (only query column Embarked)
titanic.full[titanic.full$Embarked=='', "Embarked"] <-'S'
# check cleaning 
table(titanic.full$Embarked)


#2. clean missing values of Age class
table(is.na(titanic.full$Age))
# what the median is for the Age
median.age <- median(titanic.full$Age, na.rm = TRUE)
# cleaning missing values with a filter (only query column Age)
titanic.full[is.na(titanic.full$Age), "Age"] <- median.age
# check cleaning 
table(is.na(titanic.full$Age))


#3. clean missing values of Fare class
table(is.na(titanic.full$Fare))
# what the median is for the Fare
median.fare <- median(titanic.full$Fare, na.rm = TRUE)
# cleaning missing values with a filter (only query column Age)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- median.fare
# check cleaning 
table(is.na(titanic.full$Fare))


# 4. checking missing values of Survived class
table(is.na(titanic.full$Survived))
## 418 NA is for added titanic.test$Survived  



# Categorical Casting

#  do categorical casting of data 
#    for every column except "Survived"
#    because the "Survived" column 
#    has three categories at this moment:
#    0, 1  and NA.  
table(is.na(titanic.full$Survived))
#    if you do categorical casting of "Survived" now
#    you lose thr binary classification  

# check which classes should be categorical
str(titanic.full)

#assign vectors as factors to variables
titanic.full$Pclass <- as.factor(titanic.full$Pclass) #add ordered
titanic.full$Sex <- as.factor(titanic.full$Sex) 
titanic.full$Embarked <- as.factor(titanic.full$Embarked) 
#also consider SibSp and  Parch to be an ordinal category

str(titanic.full)
#$ Pclass     : Factor w/ 3 levels "1","2","3"
#$ Sex        : Factor w/ 2 levels "female","male"
#$ Embarked   : Factor w/ 3 levels "C","Q","S"


# Split dataset back into train and test datasets
write.csv(titanic.full, file="titanic_full.csv", row.names = FALSE)

titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE, ]
str(titanic.train)

titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE, ]
#or use '!'[not operator] for the flipped the true to false: titanic.test <- titanic.full[!titanic.full$IsTrainSet==TRUE, ]
tail(titanic.test)
#check new factors in train dataset
str(titanic.train)

# make categorical casting of "Survived" 
#   in the train dataset
titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)
#$ Survived   : Factor w/ 2 levels "0","1"
# Keep this as binary classification problem


#
# B. Building a predictive model
#

# B.1. Define in R which variables (columns) are predictors for this model
# and which variables (columns) are ignored
# when you send those into predictive model

#
# building a formula

# standard initial formula for the Random Forest model:
# randomForest(Survived~.)
# use everything execpt Survived to predict Survived
# this requried to drop everything you did not want to use as predictor

#
# Explicitly call out which columns to use to build a predictive model
str(titanic.train)

# "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
# build a set of relationships formula
survived.formula <- as.formula(survived.equation)
#   predict Survived given these columns


# install.packages("randomForest")
library(randomForest)

# call the Random Forest model
#    with skipping 70/30 split here
#    with skipping cross validation here
titanic.model <- randomForest(
  formula=survived.formula, 
  data=titanic.train, 
  ntree=500, 
  mtry=3, 
  nodesize = 0.01*nrow(titanic.train)
)


#
# C. Apply a predictive model
#

# C.1 Specify features
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
# Specify these features because you want to use PassangerID

# C.2 Run a prediction on the trained model
# Assign results of prediction
#   use a name of target column for cbin as a name for this titanic prediction
Survived <- predict(
  titanic.model,  # model for prediction
  newdata = titanic.test # data for prediction 
)



#
# D. Build a dataframe to write data as .csv for Kaggle submition
#

# .csv needs to have only two columns: PassengerID and Survived

# isolate PassengerID and throw it to PassengerId var (a vector)
#   name of var is according to Kaggle req
PassengerId <- titanic.test$PassengerId

# convert to dataframe
output.df <- as.data.frame(PassengerId)
# throw Survived into dataframe as a secondary column
output.df$Survived <- Survived
#check
tail(output.df)

# write output.df dataframe as .csv
#  set row.names = FALSE to remove column with numbers of observation from csv file
write.csv(output.df, file="submission1.csv", row.names = FALSE)
write.csv(titanic.full, file="titanic_full.csv", row.names = FALSE)
