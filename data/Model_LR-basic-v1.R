# Model LR-basic-v1:  Logistic Regression
# SScoreL: 
# 
# Logistic Regression - Model LR-basic-v1
# Using logistic regression as a classification technique

# - Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# - Fit the model as logistic regression:
#   glm(formula = Survived ~ Sex, family = "binomial", data = titanic_train)


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

# Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
colnames(titanic.train)

# Create imputation model for Age variable
impute <- preProcess(titanic.train[, c(5:8)],  # select numeric variables
                     method = c("knnImpute") 
                     )
# Impute missing Ages in train set
set.seed(42)
titanic.train_imp <- predict(impute, titanic.train[, c(5:8)] )     

titanic.train <- cbind(titanic.train[, c(1:4)], 
                       titanic.train_imp, 
                       titanic.train[, c(9:10)] 
                       )

# check train data
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

# make a logistic regression model - Model LR-basic-v1
# that only uses the Sex variable as a predictor

# fit the model as logistic regression
titanic.model_LR1 <- glm(Survived ~ Sex,     # formula for model fitting
                     data = titanic.train,   # train set
                     family = "binomial")    # for binary logistic regression

# Check model summary
summary(titanic.model_LR1)

# Call:
#   glm(formula = Survived ~ Sex, family = "binomial", data = titanic.train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.6462  -0.6471  -0.6471   0.7725   1.8256  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   1.0566     0.1290   8.191 2.58e-16 ***
#   Sexmale      -2.5137     0.1672 -15.036  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1186.7  on 890  degrees of freedom
# Residual deviance:  917.8  on 889  degrees of freedom
# AIC: 921.8
# 
# Number of Fisher Scoring iterations: 4


# The summary output for the logistic regression model coefficients shows
# - the model produced a positive intercept value 1.0480 
# and a weight of -2.5051 placed on gender level of male; 
# - both the intercept and male variables have high significance. 

# Note that the first level of each class you pass into the model 
# is treated as the default, so we don't need a separate variable for female. 


# Use the model to make predictions on the train set:
train_preds_LR1 <- predict(titanic.model_LR1,    # model for predicting
                       newdata = titanic.train,    # Data to use for predictions
                       type="response")            # Return predicted probabilities

table(train_preds_LR1, titanic.train$Sex)
# train_preds_LR1     female male
# 0.188908145580594      0  577
# 0.742038216559921    314    0

# The table shows that the model predicted 
# a survival chance of roughly 19% for males 
# and 74% for females. 
# If we used this simple model to predict survival, 
# we'd end up predicting that all women survived 
# and that all men died. 


# Convert the predicted probabilities to class (classification) predictions 
# Assume that any observation with a predicted probability of 
# 0.5 or above is a positive result (in this case, survived) 
# and that cases below 0.5 are negative 

# Convert preds to positive (1) or negative (0) class
class_preds_LR1 <- ifelse(train_preds_LR1 >= 0.5, 1, 0)  

# Make a table of predictions vs. actual
result_table_LR1 <- table(class_preds_LR1,             
                      titanic.train$Survived)  

result_table_LR1
# class_preds_LR1   0   1 (real)
#     (model)   0 468 109
#     (model)   1  81 233

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
(468+233)/sum(result_table_LR1)
#   0.7867565

# Overall prediction accuracy is just one of many quantities you can use 
# to assess a classification model. 
# Oftentimes accuracy is not the best metric for assessing a model.

# Consider a model made to predict the occurrence of a disease 
# that only occurs in 0.01% of people. 
# A model that never predicts that anyone has the disease would be 99.99% accurate, 
# but it also wouldn't help save lives. 

# In this case, we might be more interested in the model's sensitivity: 
# the proportion of positive cases that the model correctly identifies as positive.

# Relying only on sensitivity can also be a problem. 
# Consider a new model that predicts everyone has the disease. 
# This new model would achieve a sensitivity score of 100% 
# since it would correctly label everyone who has the disease 
# as having the disease. 
# In this case, the model's specificity -- the number negative cases 
# the model correctly identifies as negative -- is zero, 
# so the model loses any value for distinguishing between the classes.

# We won't discuss all the different evaluation metrics that fall out 
# the confusion matrix, but it is a good idea to consider accuracy 
# as well as sensitivity and specificity when assessing model performance. 

# We can view these metrics as well as several others 
# using the caret package's confusionMatrix() function. 

confusionMatrix(data= class_preds_LR1, 
                  reference= titanic.train$Survived,
                  positive = "1") # Set the Positive Class to Survived

# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0 468 109
# 1  81 233
# 
# Accuracy : 0.7868          
# 95% CI : (0.7584, 0.8132)
# No Information Rate : 0.6162          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.5421          
# Mcnemar's Test P-Value : 0.05014         
#                                           
#             Sensitivity : 0.6813          
#             Specificity : 0.8525          
#          Pos Pred Value : 0.7420          
#          Neg Pred Value : 0.8111          
#              Prevalence : 0.3838          
#          Detection Rate : 0.2615          
#    Detection Prevalence : 0.3524          
#       Balanced Accuracy : 0.7669          
#                                           
#        'Positive' Class : 1                  


# The confusion matrix summary output confirms our accuracy calculation 
# and shows us several other evaluation metrics. 
 
# Balanced accuracy is the average of sensitivity and specificity, 
# which can give us a better sense of overall model performance than pure accuracy.

# For the Titanic competition, accuracy is the scoring metric 
# used to judge the competition, so we don't have to worry too much about other metrics.



