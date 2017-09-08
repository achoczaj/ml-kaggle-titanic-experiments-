# Model LR-basic-v1a:  Logistic Regression
# SScoreL: 0.78947
# 
# Logistic Regression - Model LR-basic-v1a
# Using logistic regression as a classification technique

# - Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# - Fit the model as logistic regression:
#   glm(formula = Survived ~ Sex + Pclass + Age + SibSp + Cabin, 
#       family = "binomial", data = titanic.train)


###########################
# Data PreProcesing - ch13
###########################

# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")    


# Import data set
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)

# data pre-proces

titanic.train$PassengerId  <- NULL # Remove PassengerId
titanic.train$Ticket  <- NULL      # Remove Ticket

# Convert name to character
titanic.train$Name <- as.character(titanic.train$Name)    

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.train$Pclass <- ordered(titanic.train$Pclass,     
                                levels=c("3","2","1"))  

# Reduce cabin factor levels - use only deck code (1st char)
char_cabin <- as.character(titanic.train$Cabin)     
temp_Cabin <- ifelse(char_cabin == "",          
                    "",                        
                    substr(char_cabin, 1, 1))    
temp_Cabin <- factor(temp_Cabin )                
titanic.train$Cabin <- temp_Cabin

# Impute missing data 
library(caret)

# Impute missing ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
impute <- preProcess(titanic.train[, c(5:8)],  # select numeric variables
                     method = c("knnImpute") 
                     )

set.seed(42)
titanic.train_imp <- predict(impute, titanic.train[, c(5:8)] )     

titanic.train <- cbind(titanic.train[, c(1:4)], 
                       titanic.train_imp, 
                       titanic.train[, c(9:10)] 
                       )

summary(titanic.train)
# Survived      Pclass      Name               Sex                 Age           
# Min.   :0.0000   3:491   Length:891         Length:891         Min.   :-2.015566  
# 1st Qu.:0.0000   2:184   Class :character   Class :character   1st Qu.:-0.530005  
# Median :0.0000   1:216   Mode  :character   Mode  :character   Median :-0.048127  
# Mean   :0.3838                                                 Mean   :-0.007142  
# 3rd Qu.:1.0000                                                 3rd Qu.: 0.433751  
# Max.   :1.0000                                                 Max.   : 3.462699  
# 
# SibSp             Parch              Fare              Cabin       Embarked        
# Min.   :-0.4743   Min.   :-0.4734   Min.   :-0.64806          :687   Length:891        
# 1st Qu.:-0.4743   1st Qu.:-0.4734   1st Qu.:-0.48887   C      : 59   Class :character  
# Median :-0.4743   Median :-0.4734   Median :-0.35719   B      : 47   Mode  :character  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   D      : 33                     
# 3rd Qu.: 0.4325   3rd Qu.:-0.4734   3rd Qu.:-0.02423   E      : 32                     
# Max.   : 6.7804   Max.   : 6.9702   Max.   : 9.66174   A      : 15                     
# (Other): 18    



#####################################################
# use a logistic regression model to predict survival
#####################################################

# make a logistic regression model - Model LR-basic-v1a
# that includes a few more variables from the titanic training set:
# Survived ~ Sex + Pclass + Age + SibSp + Cabin, 

titanic.model_LR1a <- glm(Survived ~ Sex+Pclass+Age+SibSp+Cabin, # formula for model fitting
                     data= titanic.train, # train set       
                     family="binomial") # for binary logistic regression

# Check model summary
summary(titanic.model_LR1a)
# Call:
#   glm(formula = Survived ~ Sex + Pclass + Age + SibSp + Cabin, 
#       family = "binomial", data = titanic.train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.6288  -0.5745  -0.3982   0.6118   2.4398  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.2369     0.1902   6.502 7.91e-11 ***
#   Sexmale      -2.7460     0.1986 -13.830  < 2e-16 ***
#   Pclass.L      1.2955     0.2863   4.525 6.05e-06 ***
#   Pclass.Q     -0.1792     0.2155  -0.832 0.405671    
# Age          -0.6040     0.1179  -5.121 3.04e-07 ***
#   SibSp        -0.4440     0.1161  -3.823 0.000132 ***
#   CabinA        0.8144     0.6688   1.218 0.223339    
# CabinB        0.8924     0.5466   1.633 0.102535    
# CabinC        0.3838     0.4882   0.786 0.431779    
# CabinD        1.2372     0.5647   2.191 0.028444 *  
#   CabinE        1.5554     0.5570   2.792 0.005236 ** 
#   CabinF        1.1714     0.7135   1.642 0.100656    
# CabinG       -0.8805     1.0544  -0.835 0.403658    
# CabinT      -12.4742   535.4113  -0.023 0.981412    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1186.66  on 890  degrees of freedom
# Residual deviance:  774.04  on 877  degrees of freedom
# AIC: 802.04
# 
# Number of Fisher Scoring iterations: 12

# The summary output for the logistic regression model coefficients shows
# - the model produced a positive intercept value 1.2369 (prev.mod: 1.0480)
# and a weight of -2.7460 (prev.mod: -2.5051) placed on gender level of male; 
# - both the intercept and male variables have high significance. 

# Note that the first level of each class you pass into the model 
# is treated as the default, so we don't need a separate variable for female. 


# Use the model to make predictions on the train set:
train_preds_LR1a <- predict(titanic.model_LR1a,              # model for predicting
                       newdata = titanic.train,    # Data to use for predictions
                       type="response")            # Return predicted probabilities

table(train_preds_LR1a, titanic.train$Sex)
# train_preds         female male
train_preds_LR1a       female male
# 1.28331013298974e-06      0    1
# 0.00902951763442825       0    4
# 0.0158330027964081        0    1
# 0.0182678629125474        0    1
# ...
# 0.929978853794185         1    0
# 0.930096646953399         2    0
# 0.93032018974263          1    0
# 0.931531004647226         1    0


# Convert the predicted probabilities to class (classification) predictions 
# Assume that any observation with a predicted probability of 
# 0.5 or above is a positive result (in this case, survived) 
# and that cases below 0.5 are negative 

# Convert preds to positive (1) or negative (0) class
class_preds_LR1a <- ifelse(train_preds_LR1a >= 0.5, 1, 0)  

# Make a table of predictions vs. actual
result_table_LR1a <- table(class_preds_LR1a,
                      titanic.train$Survived)  
result_table_LR1a
# class_preds_LR1a   0   1 (real)
#        (model) 0 471  90
#        (model) 1  78 252

# The table above shows the classes our model predicted 
# vs. true values of the Survived variable. 
# This table of predicted vs. actual values is known as a confusion matrix.


# The Confusion Matrix

# The confusion matrix is a common tool for assessing the results of classification. 
# Each cell tells us something different about our predictions versus the true values. 

# The bottom right corner indicates the True positives: 
# people the model predicted to survive who actually did survive. 
# The bottom left cell indicates the False positives: 
# people for whom the model predicted survival who did not actually survive. 
# The top left cell indicates the True negatives: 
# people correctly identified as non survivors. 
# Finally the top right cell shows the False negatives: 
# passengers the model identified as non survivors who actually did survive.

# Calculate the Overall Prediction Accuracy from the matrix 
# by adding the total number of correct predictions 
# and dividing by the total number of predictions. 

# In the case of our model, the prediction accuracy is:
(471+252)/sum(result_table_LR1a)
#   0.8114478 
#(prev. mod: 0.7867565)

# Overall prediction accuracy is just one of many quantities you can use to assess a classification model. Oftentimes accuracy is not the best metric for assessing a model.
# Consider a model made to predict the occurrence of a disease that only occurs in 0.01% of people. A model that never predicts that anyone has the disease would be 99.99% accurate, but it also wouldn't help save lives. In this case, we might be more interested in the model's sensitivity: the proportion of positive cases that the model correctly identifies as positive.
# Relying only on sensitivity can also be a problem. Consider a new model that predicts everyone has the disease. This new model would achieve a sensitivity score of 100% since it would correctly label everyone who has the disease as having the disease. In this case, the model's specificity--the number negative cases the model correctly identifies as negative--is zero, so the model loses any value for distinguishing between the classes.
# We won't discuss all the different evaluation metrics that fall out the confusion matrix, but it is a good idea to consider accuracy as well as sensitivity and specificity when assessing model performance. We can view these metrics as well as several others using the caret package's confusionMatrix() function. Let's run it on our Titanic predictions:

confusionMatrix(data= result_table_LR1a, 
                reference= titanic.train$Survived,
                positive = "1") # Set the positive class to Survived

# Confusion Matrix and Statistics
# 
# 
# class_preds_LR1a   0   1
# 0 471  90
# 1  78 252
# 
# Accuracy : 0.8114          
# 95% CI : (0.7842, 0.8366)
# No Information Rate : 0.6162          
# P-Value [Acc > NIR] : <2e-16          
# 
# Kappa : 0.5987          
# Mcnemar's Test P-Value : 0.3961          
# 
# Sensitivity : 0.7368          
# Specificity : 0.8579          
# Pos Pred Value : 0.7636          
# Neg Pred Value : 0.8396          
# Prevalence : 0.3838          
# Detection Rate : 0.2828          
# Detection Prevalence : 0.3704          
# Balanced Accuracy : 0.7974          
# 
# 'Positive' Class : 1  


# The confusion matrix summary output confirms our accuracy calculation 
# and shows us several other evaluation metrics. 

# Balanced accuracy is the average of sensitivity and specificity, 
# which can give us a better sense of overall model performance than pure accuracy.

# For the Titanic competition, accuracy is the scoring metric 
# used to judge the competition, so we don't have to worry too much about other metrics.



###########################################
# make predictions for the Titanic test set
###########################################

# As a final exercise, let's use our logistic regression model 
# to make predictions for the Titanic test set. 
# 
# First we need to load and prepare the test data 
# using the same steps we used to prepare the training data:


# Import data set
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# data pre-proces

ids <- titanic.test$PassengerId


titanic.test$PassengerId  <- NULL # Remove PassengerId
titanic.test$Ticket  <- NULL      # Remove Ticket

# Convert name to character
titanic.test$Name <- as.character(titanic.test$Name)    

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.test$Pclass <- ordered(titanic.test$Pclass,     
                                levels=c("3","2","1")) 
# Reduce cabin factor levels - use only deck code (1st char)
char_cabin <- as.character(titanic.test$Cabin)     
temp_Cabin <- ifelse(char_cabin == "",          
                     "",                        
                     substr(char_cabin, 1, 1))    
temp_Cabin <- factor(temp_Cabin )                
titanic.test$Cabin <- temp_Cabin

# Impute missing data 
library(caret)

# Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
colnames(titanic.test)

# Create imputation model for Age variable
impute <- preProcess(titanic.train[, c(4:7)],  # select numeric variables
                     method = c("knnImpute") 
)
# Impute missing Ages in test set
set.seed(42)
titanic.test_imp <- predict(impute, titanic.test[, c(4:7)])

titanic.test <- cbind(titanic.test[, c(1:3)], titanic.test_imp, titanic.test[, c(8:9)])

# check test data
summary(titanic.test)  

# Pclass      Name               Sex                 Age             SibSp       
# 3:218   Length:418         Length:418         Min.   :-1.433   Min.   :0.0000  
# 2: 93   Class :character   Class :character   1st Qu.: 9.665   1st Qu.:0.0000  
# 1:107   Mode  :character   Mode  :character   Median :25.761   Median :0.0000  
# Mean   :25.880   Mean   :0.4474  
# 3rd Qu.:38.369   3rd Qu.:1.0000  
# Max.   :81.559   Max.   :8.0000  
# 
# Parch             Fare             Cabin       Embarked        
# Min.   :0.0000   Min.   :  0.000          :327   Length:418        
# 1st Qu.:0.0000   1st Qu.:  7.896   C      : 35   Class :character  
# Median :0.0000   Median : 14.454   B      : 18   Mode  :character  
# Mean   :0.3923   Mean   : 35.627   D      : 13                     
# 3rd Qu.:0.0000   3rd Qu.: 31.500   E      :  9                     
# Max.   :9.0000   Max.   :512.329   F      :  8                     
# NA's   :1         (Other):  8   



#####################################################
# use a logistic regression model to predict survival
#####################################################

# make a logistic regression model - Model LR-basic-v1a
# that uses the titanic.model_LR1a as a predictor

test_preds <- predict(titanic.model_LR1a,              
                      newdata = titanic.test,      
                      type = "response") 

class_preds <- ifelse(test_preds >= 0.5, 1, 0)


prediction_sub <- data.frame(PassengerId=ids, Survived=class_preds)


write.csv(prediction_sub, "titanic-model_LR1a-submission.csv", row.names=FALSE)


table(titanic.test$Sex, class_preds)
#          class_preds
#          0   1
# female 145   7
# male   264   2
