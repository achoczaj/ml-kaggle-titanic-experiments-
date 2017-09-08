# Model RF-basic-caret-v1: Random Forest
# SScoreL: 0.77990
# (RF-basic-v1: 0.78469)
# (DT-basic-v1: 0.77033)      
# (LogReg-v1a: 0.78947)
# 
# Decision Tree - Model RF-basic--caret-v1
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
str(titanic.train)
########################################
# Random Forests With the Caret Package
########################################

# Since random forest models consist of a "bag" of decision trees, each built on
# a random sample of the data, we can estimate model performance with out of bag
# error. This means that in the case of random forests, holdout validation and
# cross validation aren't as necessary to get a sense of the model's ability to
# generalize to unseen data as models that don't involve this sort of
# aggregation. Even so, we can use cross validation on a random forest model.
# Let's use the caret' package's train() function to generate a random forest
# model with cross validation:


##################################################
# Train random forest model with 5 repeats of 10-fold cross-validation
###################################################

# 2. Changing the Resampling Technique

# By default, train uses the bootstrap for resampling (method = "boot"). 
# We'll switch to 5 repeats of 10-fold cross-validation instead.


# create a caret control object to control the type and number of cross-validations performed
ctrl <- trainControl(method = "repeatedcv", # use cross validation with simple random sampling
                     number = 10,  # number of folds or number of resampling iterations
                     repeats = 2   # number of complete sets of folds to compute
                    )

# Define tuning parameter grid
grid = expand.grid(mtry=c(2))

# Train model
set.seed(42)
validated_rf <- train(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked, 
                      data=titanic.train,                # data set
                      method="rf",                       # model type: Random Forest
                      trControl= ctrl,                   # model control options
                      tuneGrid = grid,                   # required tuning parameters
                      ntree = 1000)                      # additional parameters


validated_rf          # View a summary of the model
# Random Forest 
# 
# 891 samples
# 6 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 2 times) 
# Summary of sample sizes: 802, 802, 802, 802, 802, 802, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8232309  0.6077827
# 
# Tuning parameter 'mtry' was held constant at a value of 2


# Even with this relatively small data set, 10-fold cross validation 
# takes a little while to complete. 

# When working with large data sets, cross validation may become impractically slow 
# for random forest models; in those cases using out of bag error or holdout validation set 
# is quicker and often sufficient.

# You can use out of bag error for validation when training a model with the
# caret package by changing the trainControl method to "oob".


##################################################
# Train random forest model with out of bag error
###################################################

# # Create a trainControl object
# ctrl <- trainControl(method = "oob" # use out of bag error for resampling
#                      #method = "boot" # default method
#                      )
# 
# # Define tuning parameter grid
# grid = expand.grid(mtry=c(2))
# 
# set.seed(42)
# validated_rf <- train(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked, 
#                       data=titanic.train,                # data set
#                       method="rf",                       # model type: Random Forest
#                       trControl= ctrl,                   # model control options
#                       tuneGrid = grid,                   # required tuning parameters
#                       ntree = 1000)                      # additional parameters
# 
# 
# validated_rf          # View a summary of the model
# Random Forest for out of bag error method
# 
# 891 samples
# 6 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8226712  0.6050798
# 
# Tuning parameter 'mtry' was held constant at a value of 2


##################################################
# Train random forest model with a bootstrap
###################################################

# # Create a trainControl object
# ctrl <- trainControl(method = "boot" # default method
#                      )
# 
# # Define tuning parameter grid
# grid = expand.grid(mtry=c(2))
# 
# set.seed(42)
# validated_rf <- train(Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked, 
#                       data=titanic.train,                # data set
#                       method="rf",                       # model type: Random Forest
#                       trControl= ctrl,                   # model control options
#                       tuneGrid = grid,                   # required tuning parameters
#                       ntree = 1000)                      # additional parameters
# 
# 
# validated_rf          # View a summary of the model
# Random Forest for out of bag error method
# 
# 891 samples
# 6 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8226712  0.6050798
# 
# Tuning parameter 'mtry' was held constant at a value of 2


# Random Forest for bootstrap method
# 
# 891 samples
# 6 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 891, 891, 891, 891, 891, 891, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8231359  0.6120153
# 
# Tuning parameter 'mtry' was held constant at a value of 2


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
summary(titanic.test)


################################################################################
# use the random forest model (validated_rf) to predict survival on test set
################################################################################

test_preds <- predict(validated_rf,              
                      newdata = titanic.test,      
                      type = "raw"
                      #type = "prob"
                      ) 

# There are two types of evaluation we can do here, raw or prob. 
# Raw gives you a class prediction, in our case yes and nope, 
# while prob gives you the probability on how sure the model is 
# about it’s choice. 
# Use prob, if you like to be in control of the threshold 
# and also like to use AUC score which requires probabilities, not classes. 
# There are situations where having class values can come in handy, 
# such as with multinomial models where you’re predicting more than two values.

head(test_preds)

prediction_sub <- data.frame(PassengerId=titanic.test$PassengerId, Survived=test_preds)

write.csv(prediction_sub, "titanic-model_RF1-caret-submission.csv", row.names=FALSE)
