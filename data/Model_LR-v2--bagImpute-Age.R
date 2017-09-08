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

features <- c("Pclass", "Sex", "Age", "SibSp",
              "Parch", "Fare", "Embarked", "FamilySize",  "HasMissingAge")
titanic.sub_train <-  titanic.train[, features]
str(titanic.sub_train)
colnames(titanic.sub_train)

# Use carot's dummyVars function to transform all features 
#  to a Full Set of Dummy Variables
## Imputation methods in carot only work on numeric data
## do not work on factors so transform data frame with dummyVars .

# invoke a function (dummyVars) to train model [.fit method]
model.dummy <- dummyVars(~ ., # train model on all features = transform all columns with factors into dummy variables
                        data = titanic.sub_train # use only subset train data
                   )
# create predictions using trained model [.predict method]
titanic.train.dummy <- predict(model.dummy, titanic.sub_train)
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
imputed.data <- predict(model.pre_Process, titanic.train.dummy)
View(imputed.data)

titanic.sub_train$Age <- imputed.data[, 6]
View(titanic.sub_train)


#==============================
#  Split Data of titanic.train
#==============================

# Create two series of test/training partitions with 70/30% split of the training data
# Keep the proportions of Y (Survived feature) the same across splits


# Add Y to titanic.sub_train
titanic.sub_train <- titanic.train[, Survived]
View(titanic.sub_train)

set.seed(42)
indx <- createDataPartition(titanic.sub_train$Survived, # use Survived feature vector as outcomes
                            times = 1, # number of partitions
                            p = 0.7, # percentage of data that goes to training
                            list = FALSE # results in a matrix, not a list 
                             )
##  The random sampling is done within the levels of Y (when Y is a factor) 
##  in an attempt to balance the class distributions within the splits.

titanic.train_sub1_70proc <- titanic.sub_train[indx,]
titanic.train_sub2_30proc <- titanic.sub_train[-indx,]

# Check the proportions of Y (Survival) across the datasets
prop.table(table(titanic.train$Survived))
prop.table(table(titanic.train_sub1_70proc$Survived))
prop.table(table(titanic.train_sub2_30proc$Survived))


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

View(titanic.train_sub1_70proc)


# Train the xgboost model using 10-fold CV repeated 3 times 
# and a hyperparameter grid search to train the optimal model.
titanic.model.xgboost_hpgs <- train(Survived ~ ., 
                  data = titanic.train_sub1_70proc,
                  method = "xgbTree",
                  tuneGrid = caret.grid,
                  trControl = caret.ctrl)
stopCluster(cl)

# Examine caret's processing results
titanic.model.xgboost_hpgs

# eXtreme Gradient Boosting 
# 
# 891 samples
# 13 predictor
# 2 classes: '0', '1' 
# 
# No pre-processing
# Resampling: Cross-Validated (10 fold, repeated 3 times) 
# Summary of sample sizes: 802, 801, 802, 802, 802, 803, ... 
# Resampling results across tuning parameters:
#   
#   eta    max_depth  colsample_bytree  min_child_weight  nrounds  Accuracy 
# 0.050  6          0.3               2.00               50      0.8155591
# 0.050  6          0.3               2.00               75      0.8207776
# 0.050  6          0.3               2.00              100      0.8249102
# 0.050  6          0.3               2.25               50      0.8114308
# 0.050  6          0.3               2.25               75      0.8196663
# 0.050  6          0.3               2.25              100      0.8192918
# 0.050  6          0.3               2.50               50      0.8132868
# 0.050  6          0.3               2.50               75      0.8170195
# 0.050  6          0.3               2.50              100      0.8196328
# 0.050  6          0.4               2.00               50      0.8211478
# 0.050  6          0.4               2.00               75      0.8196372
# 0.050  6          0.4               2.00              100      0.8215058
# 0.050  6          0.4               2.25               50      0.8241396
# 0.050  6          0.4               2.25               75      0.8204071
# 0.050  6          0.4               2.25              100      0.8211560
# 0.050  6          0.4               2.50               50      0.8215348
# 0.050  6          0.4               2.50               75      0.8222713
# 0.050  6          0.4               2.50              100      0.8203944
# 0.050  6          0.5               2.00               50      0.8252509
# 0.050  6          0.5               2.00               75      0.8267659
# 0.050  6          0.5               2.00              100      0.8256340
# 0.050  6          0.5               2.25               50      0.8215349
# 0.050  6          0.5               2.25               75      0.8252637
# 0.050  6          0.5               2.25              100      0.8245062
# 0.050  6          0.5               2.50               50      0.8219135
# 0.050  6          0.5               2.50               75      0.8260167
# 0.050  6          0.5               2.50              100      0.8241482
# 0.050  7          0.3               2.00               50      0.8170278
# 0.050  7          0.3               2.00               75      0.8226460
# 0.050  7          0.3               2.00              100      0.8271321
# 0.050  7          0.3               2.25               50      0.8125372
# 0.050  7          0.3               2.25               75      0.8200156
# 0.050  7          0.3               2.25              100      0.8196326
# 0.050  7          0.3               2.50               50      0.8133035
# 0.050  7          0.3               2.50               75      0.8174024
# 0.050  7          0.3               2.50              100      0.8170238
# 0.050  7          0.4               2.00               50      0.8248974
# 0.050  7          0.4               2.00               75      0.8286344
# 0.050  7          0.4               2.00              100      0.8275025
# 0.050  7          0.4               2.25               50      0.8241233
# 0.050  7          0.4               2.25               75      0.8263918
# 0.050  7          0.4               2.25              100      0.8260213
# 0.050  7          0.4               2.50               50      0.8234119
# 0.050  7          0.4               2.50               75      0.8267619
# 0.050  7          0.4               2.50              100      0.8226503
# 0.050  7          0.5               2.00               50      0.8260127
# 0.050  7          0.5               2.00               75      0.8237529
# 0.050  7          0.5               2.00              100      0.8241190
# 0.050  7          0.5               2.25               50      0.8282643
# 0.050  7          0.5               2.25               75      0.8256425
# 0.050  7          0.5               2.25              100      0.8252679
# 0.050  7          0.5               2.50               50      0.8263957
# 0.050  7          0.5               2.50               75      0.8275234
# 0.050  7          0.5               2.50              100      0.8263831
# 0.050  8          0.3               2.00               50      0.8129330
# 0.050  8          0.3               2.00               75      0.8234118
# 0.050  8          0.3               2.00              100      0.8256382
# 0.050  8          0.3               2.25               50      0.8155591
# 0.050  8          0.3               2.25               75      0.8185597
# 0.050  8          0.3               2.25              100      0.8222463
# 0.050  8          0.3               2.50               50      0.8192918
# 0.050  8          0.3               2.50               75      0.8222756
# 0.050  8          0.3               2.50              100      0.8245229
# 0.050  8          0.4               2.00               50      0.8248848
# 0.050  8          0.4               2.00               75      0.8301327
# 0.050  8          0.4               2.00              100      0.8259918
# 0.050  8          0.4               2.25               50      0.8185300
# 0.050  8          0.4               2.25               75      0.8211562
# 0.050  8          0.4               2.25              100      0.8233991
# 0.050  8          0.4               2.50               50      0.8226461
# 0.050  8          0.4               2.50               75      0.8263830
# 0.050  8          0.4               2.50              100      0.8256341
# 0.050  8          0.5               2.00               50      0.8226211
# 0.050  8          0.5               2.00               75      0.8248725
# 0.050  8          0.5               2.00              100      0.8222506
# 0.050  8          0.5               2.25               50      0.8252846
# 0.050  8          0.5               2.25               75      0.8237909
# 0.050  8          0.5               2.25              100      0.8260172
# 0.050  8          0.5               2.50               50      0.8241568
# 0.050  8          0.5               2.50               75      0.8245316
# 0.050  8          0.5               2.50              100      0.8241569
# 0.075  6          0.3               2.00               50      0.8241653
# 0.075  6          0.3               2.00               75      0.8237908
# 0.075  6          0.3               2.00              100      0.8222968
# 0.075  6          0.3               2.25               50      0.8245019
# 0.075  6          0.3               2.25               75      0.8241359
# 0.075  6          0.3               2.25              100      0.8286388
# 0.075  6          0.3               2.50               50      0.8151889
# 0.075  6          0.3               2.50               75      0.8162619
# 0.075  6          0.3               2.50              100      0.8204071
# 0.075  6          0.4               2.00               50      0.8192540
# 0.075  6          0.4               2.00               75      0.8248766
# 0.075  6          0.4               2.00              100      0.8226460
# 0.075  6          0.4               2.25               50      0.8196496
# 0.075  6          0.4               2.25               75      0.8192836
# 0.075  6          0.4               2.25              100      0.8219011
# 0.075  6          0.4               2.50               50      0.8211184
# 0.075  6          0.4               2.50               75      0.8245060
# 0.075  6          0.4               2.50              100      0.8248847
# 0.075  6          0.5               2.00               50      0.8226293
# 0.075  6          0.5               2.00               75      0.8249061
# 0.075  6          0.5               2.00              100      0.8196371
# 0.075  6          0.5               2.25               50      0.8215560
# 0.075  6          0.5               2.25               75      0.8211648
# 0.075  6          0.5               2.25              100      0.8177728
# 0.075  6          0.5               2.50               50      0.8245020
# 0.075  6          0.5               2.50               75      0.8226209
# 0.075  6          0.5               2.50              100      0.8248808
# 0.075  7          0.3               2.00               50      0.8226501
# 0.075  7          0.3               2.00               75      0.8297539
# 0.075  7          0.3               2.00              100      0.8260127
# 0.075  7          0.3               2.25               50      0.8159252
# 0.075  7          0.3               2.25               75      0.8215352
# 0.075  7          0.3               2.25              100      0.8207818
# 0.075  7          0.3               2.50               50      0.8196582
# 0.075  7          0.3               2.50               75      0.8159170
# 0.075  7          0.3               2.50              100      0.8181643
# 0.075  7          0.4               2.00               50      0.8241653
# 0.075  7          0.4               2.00               75      0.8215350
# 0.075  7          0.4               2.00              100      0.8215352
# 0.075  7          0.4               2.25               50      0.8252719
# 0.075  7          0.4               2.25               75      0.8274941
# 0.075  7          0.4               2.25              100      0.8278812
# 0.075  7          0.4               2.50               50      0.8222630
# 0.075  7          0.4               2.50               75      0.8211520
# 0.075  7          0.4               2.50              100      0.8218676
# 0.075  7          0.5               2.00               50      0.8230165
# 0.075  7          0.5               2.00               75      0.8222631
# 0.075  7          0.5               2.00              100      0.8196327
# 0.075  7          0.5               2.25               50      0.8271321
# 0.075  7          0.5               2.25               75      0.8237612
# 0.075  7          0.5               2.25              100      0.8226503
# 0.075  7          0.5               2.50               50      0.8248764
# 0.075  7          0.5               2.50               75      0.8248847
# 0.075  7          0.5               2.50              100      0.8252594
# 0.075  8          0.3               2.00               50      0.8159170
# 0.075  8          0.3               2.00               75      0.8140359
# 0.075  8          0.3               2.00              100      0.8189300
# 0.075  8          0.3               2.25               50      0.8189008
# 0.075  8          0.3               2.25               75      0.8204030
# 0.075  8          0.3               2.25              100      0.8204242
# 0.075  8          0.3               2.50               50      0.8181345
# Kappa    
# 0.5935794
# 0.6068849
# 0.6180898
# 0.5842331
# 0.6044035
# 0.6044249
# 0.5879653
# 0.5985676
# 0.6060318
# 0.6071380
# 0.6056229
# 0.6110005
# 0.6130617
# 0.6062304
# 0.6093097
# 0.6078340
# 0.6105562
# 0.6071477
# 0.6165897
# 0.6216943
# 0.6196731
# 0.6085577
# 0.6182596
# 0.6171971
# 0.6100710
# 0.6199142
# 0.6163509
# 0.5972098
# 0.6111448
# 0.6232481
# 0.5857897
# 0.6043911
# 0.6056541
# 0.5880950
# 0.5990790
# 0.6009449
# 0.6151758
# 0.6251679
# 0.6235508
# 0.6131623
# 0.6200444
# 0.6200443
# 0.6134141
# 0.6212524
# 0.6130018
# 0.6182405
# 0.6146660
# 0.6162039
# 0.6252255
# 0.6201126
# 0.6189373
# 0.6191629
# 0.6234120
# 0.6214160
# 0.5870701
# 0.6130842
# 0.6190780
# 0.5939054
# 0.6032207
# 0.6122919
# 0.6021870
# 0.6104701
# 0.6173473
# 0.6169148
# 0.6294802
# 0.6209005
# 0.6025761
# 0.6099580
# 0.6149633
# 0.6110695
# 0.6208556
# 0.6199284
# 0.6121947
# 0.6176843
# 0.6122569
# 0.6172876
# 0.6152098
# 0.6202910
# 0.6148137
# 0.6171087
# 0.6162327
# 0.6148021
# 0.6155192
# 0.6139317
# 0.6140038
# 0.6157057
# 0.6272144
# 0.5941617
# 0.5986460
# 0.6090058
# 0.6042810
# 0.6184513
# 0.6137444
# 0.6059182
# 0.6067257
# 0.6126334
# 0.6085188
# 0.6171665
# 0.6185458
# 0.6119532
# 0.6181872
# 0.6067398
# 0.6105785
# 0.6100867
# 0.6034258
# 0.6160376
# 0.6128245
# 0.6180063
# 0.6110104
# 0.6289342
# 0.6216869
# 0.5956121
# 0.6105522
# 0.6100898
# 0.6044710
# 0.5993044
# 0.6052898
# 0.6145711
# 0.6105990
# 0.6108532
# 0.6174860
# 0.6238197
# 0.6241375
# 0.6106597
# 0.6094488
# 0.6118225
# 0.6139821
# 0.6127631
# 0.6077485
# 0.6230489
# 0.6156557
# 0.6135254
# 0.6182300
# 0.6186490
# 0.6192931
# 0.5980225
# 0.5954455
# 0.6072175
# 0.6030588
# 0.6081682
# 0.6096304
# 0.6017289
# [ reached getOption("max.print") -- omitted 101 rows ]
# 
#
# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning parameter 'subsample' was held constant at a value of 1
# 
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were nrounds = 50, max_depth = 8, eta =
#   0.1, gamma = 0, colsample_bytree = 0.5, min_child_weight = 2.5 and subsample = 1.


##Secound calculation results:
# Tuning parameter 'gamma' was held constant at a value of 0
# Tuning
# parameter 'subsample' was held constant at a value of 1
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were nrounds = 75, max_depth = 6, eta
# = 0.075, gamma = 0, colsample_bytree = 0.4, min_child_weight = 2.25
# and subsample = 1.

#==========================================================
# Make predictions on the test part of trainning data set 
# using a xgboost model 
# trained on 625 rows of the training set (titanic.train_sub2_30proc)
# using the found optimal hyperparameter values.
#=============================================
preds <- predict(titanic.model.xgboost_hpgs, titanic.train_sub2_30proc)


# Use caret's confusionMatrix() function to estimate the 
# effectiveness of this model on unseen, new data.
confusionMatrix(preds, titanic.train_sub2_30proc$Survived)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
#          0 161  12
#          1   3  90
# 
# Accuracy : 0.9436          
# 95% CI : (0.9087, 0.9681)
# No Information Rate : 0.6165          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.8787          
# Mcnemar's Test P-Value : 0.03887         
# 
# Sensitivity : 0.9817          
# Specificity : 0.8824          
# Pos Pred Value : 0.9306          
# Neg Pred Value : 0.9677          
# Prevalence : 0.6165          
# Detection Rate : 0.6053          
# Detection Prevalence : 0.6504          
# Balanced Accuracy : 0.9320          
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

str(titanic.train)
summary(titanic.test)


#=================================================
#  Impute missing Ages with bagged decision trees
#=================================================

# Subset data to features to be used for imputing missing values of Ages
colnames(titanic.sub_train)
colnames(titanic.test)


# use the same features to impute Age as for titanic.train
titanic.sub_test <- titanic.test[, features]
## or
## features.test <- features
## or
## features.test <- c("Pclass", "Sex", "Age", "SibSp",
##               "Parch", "Fare", "Embarked", "FamilySize",  "HasMissingAge")
##
## titanic.sub_test <- titanic.test[, features.test]

str(titanic.sub_test)
colnames(titanic.sub_test)

#=================================================
#  Impute missing Ages with bagged decision trees
#=================================================

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
titanic.test.dummy <- predict(model.dummy, titanic.sub_test)
View(titanic.test.dummy)







# Use bagged decision trees to impute missing values for the Age feature

# Pre-Processing of Predictors
## Pre-processing transformation (centering, scaling etc.) 
## can be estimated from the training data 
## and applied to any data set with the same variables.
pre.process <- preProcess(titanic.test.dummy, method = "bagImpute")

imputed.data <- predict(pre.process, titanic.test.dummy)
View(imputed.data)

titanic.test$Age <- imputed.data[, 6]
View(titanic.test)

#===============================================================================
# use the eXtreme Gradient Boosting model (titanic.model_RF) to predict survival on test set
#===============================================================================

preds.test <- predict(titanic.model.xgboost_hpgs,              
                      newdata = titanic.test,      
                      type = "raw"
) 

prediction_sub <- data.frame(PassengerId=titanic.test$PassengerId, Survived=preds.test)

write.csv(prediction_sub, "titanic-model_RF1-submission.csv", row.names=FALSE)
