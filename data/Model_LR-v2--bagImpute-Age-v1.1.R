#==============================================================================
#
# File: Model_LR-v2--bagImpute-Age.R
# Author: Arkadiusz Choczaj
# Description:
#
# Model Model_LR-v2--bagImpute-Age
# Logistic Regression 
# Current model SScoreL:
#
# Prev.results:
# (LR-basic-v1a: 0.78947)
#
# Model info: 
# 1. Impute missing data for Age class using bagged decision trees 
# - Impute missing Ages only with the numeric 
#   variables: Age, sibSp, Parch and Fare
#
# 2. Use xgboost model using 10-fold CV repeated 3 times 
# and a hyperparameter grid search to train the optimal model
#  expand.grid(eta = c(0.05, 0.075, 0.1),
#             nrounds = c(50, 75, 100),
#             max_depth = 6:8,
#             min_child_weight = c(2.0, 2.25, 2.5),
#             colsample_bytree = c(0.3, 0.4, 0.5),
#             gamma = 0,
#             subsample = 1)
#
#==============================================================================



# install.packages(c("e1071", "caret", "doSNOW", "ipred", "xgboost"))
library(caret)
library(doSNOW)


#=======================
#  Load Data
#=======================

# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")    

# Import data set
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)


#=======================
#  Data Wrangling
#=======================

# Remove features 
titanic.train$Name  <- NULL        # Remove Name
titanic.train$Ticket  <- NULL      # Remove Ticket


# Replace missing embarked values with mode
table(titanic.train$Embarked)
titanic.train$Embarked[titanic.train$Embarked == ""] <- "S"


# Add a feature for family size
titanic.train$FamilySize <- 1 + titanic.train$SibSp + titanic.train$Parch


# Add a feature for tracking missing ages
summary(titanic.train$Age)
titanic.train$HasMissingAge <- ifelse(is.na(titanic.train$Age),
                           TRUE, FALSE)


# Set up features as factors
titanic.train$Survived <- as.factor(titanic.train$Survived)
titanic.train$Pclass <- as.factor(titanic.train$Pclass)
titanic.train$Sex <- as.factor(titanic.train$Sex)
titanic.train$Embarked <- as.factor(titanic.train$Embarked)
titanic.train$HasMissingAge <- as.factor(titanic.train$HasMissingAge)
str(titanic.train)


#=================================================
#  Impute missing Ages with bagged decision trees
#=================================================

# Subset data to features to be used for imputing missing values of Ages
colnames(titanic.train)

features <- c("Survived", "Pclass", "Sex", "Age", "SibSp",
              "Parch", "Fare", "Embarked", "FamilySize",  "HasMissingAge")
titanic.train_sub <-  titanic.train[, features]
str(titanic.train_sub)
colnames(titanic.train_sub)

# Use carot's dummyVars function to transform all features 
#  to a Full Set of Dummy Variables
## Imputation methods in carot only work on numeric data
## do not work on factors so transform data frame with dummyVars .

# invoke a function (dummyVars) to train model [.fit method]
model.dummy <- dummyVars(~ ., # train model on all features = transform all columns with factors into dummy variables
                        data = titanic.train_sub[, -1] # use only subset train data
                   )
# create predictions using trained model [.predict method]
set.seed(42)
titanic.train.dummy <- predict(model.dummy, titanic.train_sub[, -1])
View(titanic.train.dummy)
## factor features become binary indicators (true / false)


# Use bagged decision trees to impute missing values for the Age feature

# Pre-Processing of Predictors
## Pre-processing transformation (centering, scaling etc.) 
## can be estimated from the training data 
## and applied to any data set with the same variables.

# create an imputation model with preProcess function
# using bagged decision trees imputation
model.pre_Process <- preProcess(titanic.train.dummy, method = "bagImpute")
## function create an imputation model for every column in data set
## as you never know which column in the future will have missing values

# predict missing values
set.seed(42)
imputed.data <- predict(model.pre_Process, titanic.train.dummy)

titanic.train_sub$Age <- imputed.data[, "Age"]
View(titanic.train_sub)


#==============================
#  Split Data of titanic.sub_train
#==============================

# Create two series of test/training partitions with 70/30% split of the training data
# Keep the proportions of Y (Survived feature) the same across splits


# Add Y to titanic.sub_train


set.seed(42)
indx <- createDataPartition(titanic.train_sub$Survived, # use Survived feature vector as outcomes
                            times = 1, # number of partitions
                            p = 0.7, # percentage of data that goes to training
                            list = FALSE # results in a matrix, not a list 
                             )
##  The random sampling is done within the levels of Y (when Y is a factor) 
##  in an attempt to balance the class distributions within the splits.

titanic.train_subTrain_70proc <- titanic.train_sub[indx,]
titanic.train_subTest_30proc <- titanic.train_sub[-indx,]

# Check the proportions of Y (Survival) across the datasets
prop.table(table(titanic.train_sub$Survived))
prop.table(table(titanic.train_subTrain_70proc$Survived))
prop.table(table(titanic.train_subTest_30proc$Survived))


#=========================================
#  Train a model with to predict Survival
#=========================================

# Create a caret control object to control the type and number of cross-validations performed
caret.ctrl <- trainControl(method = "repeatedcv", # use cross validation with simple random sampling
                     number = 10,  # number of folds or number of resampling iterations
                     repeats = 3, # number of complete sets of folds to compute
                     search = "grid" # describe how the tuning parameter grid is determined
                     )
## Set up caret to perform 10-fold cross validation repeated 3 times 
## and to use a grid search (default option) for optimal model hyperparamter values. 

# Define tuning parameter grid
caret.grid = expand.grid(eta = c(0.05, 0.075, 0.1),
                   nrounds = c(50, 75, 100),
                   max_depth = 6:8,
                   min_child_weight = c(2.0, 2.25, 2.5),
                   colsample_bytree = c(0.3, 0.4, 0.5),
                   gamma = 0,
                   subsample = 1
                   )
## Use a grid search of 243 hyperparameters for xgboost
View(caret.grid)


# Use the doSNOW package to enable caret to train in parallel.
# While there are many package options in this space, doSNOW
# has the advantage of working on both Windows and Mac OS X.
#
# Create a socket cluster using 10 processes. 
#
# NOTE - Tune this number based on the number of cores/threads 
# available on your machine!!!
#
cl <- makeCluster(4, type = "SOCK")

# Register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)

View(titanic.train_subTrain_70proc)


# Train the xgboost model using 10-fold CV repeated 3 times 
# and a hyperparameter grid search to train the optimal model.
titanic.model.xgboost_hpgs <- train(Survived ~ ., 
                  data = titanic.train_subTrain_70proc,
                  method = "xgbTree",
                  tuneGrid = caret.grid,
                  trControl = caret.ctrl)
stopCluster(cl)

# Examine caret's processing results
titanic.model.xgboost_hpgs

# eXtreme Gradient Boosting 
# 
# 625 samples
# 9 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 563, 563, 562, 562, 563, 562, ... 
# Resampling results across tuning parameters:
#   
# eta    max_depth  colsample_bytree  min_child_weight  nrounds  Accuracy 
# 0.050  6          0.3               2.00               50      0.8005632
# 0.050  6          0.3               2.00               75      0.8011094
# ...
# Kappa    
# 0.5619584
# 0.5649719
# ...
# 0.5768048
# [ reached getOption("max.print") -- omitted 101 rows ]
# 
#
# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'subsample' was held constant at a value of 1
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were nrounds = 50, max_depth = 6, 
# eta = 0.1, gamma = 0, colsample_bytree = 0.4, min_child_weight = 2.5 
# and subsample = 1.


#==========================================================
# Make predictions on the test part of trainning data set 
# using a xgboost model 
# trained on 625 rows of the training set (titanic.train_sub2_30proc)
# using the found optimal hyperparameter values.
#=============================================
preds <- predict(titanic.model.xgboost_hpgs, titanic.train_subTest_30proc)


# Use caret's confusionMatrix() function to estimate the 
# effectiveness of this model on unseen, new data.
confusionMatrix(preds, titanic.train_subTest_30proc$Survived)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0 148  25
# 1  16  77
# 
# Accuracy : 0.8459          
# 95% CI : (0.7968, 0.8871)
# No Information Rate : 0.6165          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.6685          
# Mcnemar's Test P-Value : 0.2115          
# 
# Sensitivity : 0.9024          
# Specificity : 0.7549          
# Pos Pred Value : 0.8555          
# Neg Pred Value : 0.8280          
# Prevalence : 0.6165          
# Detection Rate : 0.5564          
# Detection Prevalence : 0.6504          
# Balanced Accuracy : 0.8287          
# 
# 'Positive' Class : 0 


#=========================================================================
#=========================================================================
#  Predict Y on titanic.test with the best eXtreme Gradient Boosting model
#=========================================================================
#=========================================================================

# Import data set
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)


#=======================
#  Data Wrangling
#=======================

# Remove features 
titanic.test$Name  <- NULL        # Remove Name
titanic.test$Ticket  <- NULL      # Remove Ticket

# Replace missing embarked values with mode
titanic.test$Embarked[titanic.test$Embarked == ""] <- "S"

# Add new feature for family size
titanic.test$FamilySize <- 1 + titanic.test$SibSp + titanic.test$Parch

# Add new feature for tracking missing ages
summary(titanic.test$Age)
titanic.test$HasMissingAge <- ifelse(is.na(titanic.test$Age),
                                      TRUE, FALSE)

# Set up non-numeric features as factors
titanic.test$Pclass <- as.factor(titanic.test$Pclass)
titanic.test$Sex <- as.factor(titanic.test$Sex)
titanic.test$Embarked <- as.factor(titanic.test$Embarked)
titanic.test$HasMissingAge <- as.factor(titanic.test$HasMissingAge)

str(titanic.test)
summary(titanic.test)


#=================================================
#  Impute missing values with bagged decision trees
#=================================================

# Subset data to features to be used for imputing missing values of Ages
colnames(titanic.train_sub)
colnames(titanic.test)


# use the same features to impute Age as for titanic.train
features.test <- c("Pclass", "Sex", "Age", "SibSp",
              "Parch", "Fare", "Embarked", "FamilySize",  "HasMissingAge")

titanic.test_sub <- titanic.test[, features.test]
## or
## features.test_sub <- features
## or
## features.test_sub <- c("Survived" , "Pclass", "Sex", "Age", "SibSp",
##               "Parch", "Fare", "Embarked", "FamilySize",  "HasMissingAge")
##
## titanic.test_sub <- titanic.test[, features.test_sub]

str(titanic.test_sub)
colnames(titanic.test_sub)


#  Impute missing values with bagged decision trees

# Use carot's dummyVars function to transform all features 
#  to a Full Set of Dummy Variables
## Imputation methods in carot only work on numeric data
## do not work on factors so transform data frame with dummyVars.

# invoke a function (dummyVars) to train model [.fit method]
# model.dummy.test <- dummyVars(~ ., # transform all non-numeric features (factors) to binary indicators
#                         data = titanic.sub_test # use subset data
#                         )

# use the same model as for train data set

# predict binary indicators
titanic.test.dummy <- predict(model.dummy, titanic.test_sub)
View(titanic.test.dummy)


# Use bagged decision trees to impute missing values for the Age feature

# Pre-Processing of Predictors
## Pre-processing transformation (centering, scaling etc.) 
## can be estimated from the training data 
## and applied to any data set with the same variables.
pre.process.test <- preProcess(titanic.test.dummy, method = "bagImpute")

set.seed(42)
imputed.data.test <- predict(pre.process.test, titanic.test.dummy)
View(imputed.data.test)

titanic.test$Age <- imputed.data[, "Age"]
titanic.test$Fare <- imputed.data[, "Fare"]

summary(titanic.test)

#===============================================================================
# use the eXtreme Gradient Boosting model (titanic.model_RF) to predict survival on test set
#===============================================================================

preds.test <- predict(titanic.model.xgboost_hpgs,              
                      newdata = titanic.test,      
                      type = "raw"
) 

prediction_sub <- data.frame(PassengerId=titanic.test$PassengerId, Survived=preds.test)

write.csv(prediction_sub, "titanic-model-LR-v2--bagImpute-Age-submission.csv", row.names=FALSE)