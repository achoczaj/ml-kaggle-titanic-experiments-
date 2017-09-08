# Model RF-basic-v1: Random Forest
# SScoreL: 0.78469
# (DT-basic-v1: 0.77033)      
# (LogReg-v1a: 0.78947)
# 
# Decision Tree - Model RF-basic-v1
# Model - limited_complexity_tree 
# rpart(Survived ~ Sex + Pclass + Age + SibSp +Fare+Embarked,
# cp = 0.001,              # complexity parameter
# maxdepth = 5,            # maximum tree depth
# minbucket = 5,           # min number of obs in leaf nodes
# method = "class",        # return classifications instead of probs
# data = titanic.train)    # use the titanic training data

# Decision trees are a conceptually simple predictive modeling technique, but
# when you start building deep trees, they become complicated and likely to
# overfit your training data. In addition, decision trees are constructed in a
# way such that branch splits are always made on variables that appear to be the
# most significant first, even if those splits do not lead to optimal outcomes
# as the tree grows. Random forests are an extension of decision trees that
# address these shortcomings.



#install.packages("randomForest")   # Uncomment to install the random forest package
#install.packages("caret")   # Uncomment to install the random forest package
library(randomForest)
library(caret)

###################
# Data PreProcesing
###################

# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")    

# Import data set
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
str(titanic.train)


# Convert the feature as ordered factor (ordinal categorical variable)
titanic.train$Pclass <- ordered(titanic.train$Pclass,     
                                levels=c("3","2","1"))

# Convert the feature as nominal categorical variable
titanic.train$Sex <- as.factor(titanic.train$Sex)

# Convert the feature as ordered factor (ordinal categorical variable)
# cleaning missing values with a filter (only query column Embarked)
titanic.train[titanic.train$Embarked=='', "Embarked"] <- 'S'
titanic.train$Embarked <- ordered(titanic.train$Embarked, levels = c("S","C","Q")) 

# # Reduce cabin factor levels - use only deck code (1st char)
# char_cabin <- as.character(titanic.train$Cabin)     
# temp_Cabin <- ifelse(char_cabin == "",          
#                      "",                        
#                      substr(char_cabin, 1, 1))    
# temp_Cabin <- factor(temp_Cabin )                
# titanic.train$Cabin <- temp_Cabin

# Impute missing data 

# Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
colnames(titanic.train)

# Create imputation model for Age variable

# Impute missing ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
impute <- preProcess(titanic.train[, c(6:8,10)],  
                     method = c("knnImpute") 
)

set.seed(42)
titanic.train_imp <- predict(impute, titanic.train[, c(6:8,10)] )     

titanic.train <- cbind(titanic.train[, -c(6:8,10)], #not numeric columns
                       titanic.train_imp
                        )
summary(titanic.train)

# Convert target to factor
titanic.train$Survived <- as.factor(titanic.train$Survived) 

head(titanic.train)

#####################################################
# use a random forest model to predict survival
#####################################################

# Build random forest model
set.seed(42)
titanic.model_RF <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked,
                         data = titanic.train,    # Data set
                         ntree = 1000,            # Number of trees to grow
                         mtry = 2                 # Number of branch variables
                         )

titanic.model_RF
# Call:
#   randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp +      Fare + Embarked, data = titanic.train, ntree = 1000, mtry = 2) 
# Type of random forest: classification
# Number of trees: 1000
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 16.84%
# Confusion matrix:
#   0   1 class.error
# 0 500  49  0.08925319
# 1 101 241  0.29532164

# The model summary output shows us the formula we used to build the model, the
# number of trees, the number of variables used at each branch split.

# The "OOB estimate of error rate" is an estimate of the model's performance 
# based on the performance of each tree on "out of bag" data: 
# the data that was not included in the sample use to create the tree. 
# Checking OOB error is an alternative to assessing a random forest model 
# with holdout validation or cross validation. 
# In this case the OOB error rate of 16.84% suggests 
# the model is about 83.16% accurate.



#################################################################
# random forest model to make predictions on the Titanic test set
#################################################################

# Use the random forest model to make predictions on the Titanic test set 
# and submit them to Kaggle to see how it performs. 
# We can use the same predict() function we used for decision trees 
# to generate predictions:

# Import data set
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# data pre-proces

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.test$Pclass <- ordered(titanic.test$Pclass,     
                               levels=c("3","2","1")) 

# Convert the feature as nominal categorical variable
titanic.test$Sex <- as.factor(titanic.test$Sex)

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.test$Embarked <- ordered(titanic.test$Embarked, levels = c("S","C","Q")) 

# Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
colnames(titanic.test)

# Impute missing test set ages using the previously constructed imputation model
set.seed(42)
titanic.test_imp <- predict(impute, titanic.test[, c(5:7,9)] )     

titanic.test <- cbind(titanic.test[, -c(5:7,9)], #not numeric columns
                       titanic.test_imp
)
summary(titanic.train)


################################################################################
# use the random forest model (titanic.model_RF) to predict survival on test set
################################################################################

test_preds <- predict(titanic.model_RF,              
                      newdata = titanic.test,      
                      type = "class"
                      ) 

prediction_sub <- data.frame(PassengerId=titanic.test$PassengerId, Survived=test_preds)

write.csv(prediction_sub, "titanic-model_RF1-submission.csv", row.names=FALSE)


# If we submit these predictions to Kaggle, we achieve an accuracy of 0.78947 on
# the test data, which is a bit higher than any of our previous scores with
# decision trees or logistic regression. 

# Although random forests often have better predictive performance 
# than decision trees, they aren't without their drawbacks.

# Training a random forest model can take much longer than a single
# decision tree, because you have to build many trees instead of one. The final
# random forest model can also take up a lot of computer memory depending on the
# size of the trees, number of trees and the size of the data size you are
# using. It is easiest start small and ramp up to larger forests with more
# trees.
