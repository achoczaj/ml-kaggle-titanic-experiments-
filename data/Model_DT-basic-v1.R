# Model DT-basic-v1:  Decision Tree
# SScoreL: 0.77033      
# (LogReg-v1a: 0.78947)
# 
# Decision Tree - Model DT-basic-v1
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

####################
# Data PreProcesing 
####################

# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")    

# Import data set
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.train$Pclass <- ordered(titanic.train$Pclass,     
                                levels=c("3","2","1"))  

# Impute missing data 
library(caret)

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
titanic.train_imp <- predict(impute, titanic.train[, c(5:8,10)] )     

titanic.train <- cbind(titanic.train[, -c(5:8,10)], #not numeric columns
                       titanic.train_imp
                       )

summary(titanic.train)
# PassengerId       Survived      Pclass 
# Min.   :  1.0   Min.   :0.0000   3:491  
# 1st Qu.:223.5   1st Qu.:0.0000   2:184  
# Median :446.0   Median :0.0000   1:216  
# Mean   :446.0   Mean   :0.3838          
# 3rd Qu.:668.5   3rd Qu.:1.0000          
# Max.   :891.0   Max.   :1.0000          
# Name              Ticket             Cabin          
# Length:891         Length:891         Length:891        
# Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# Embarked             Sex                 Age           
# Length:891         Length:891         Min.   :-2.015566  
# Class :character   Class :character   1st Qu.:-0.530005  
# Mode  :character   Mode  :character   Median :-0.048127  
# Mean   :-0.007142  
# 3rd Qu.: 0.433751  
# Max.   : 3.462699  
# SibSp             Parch              Fare         
# Min.   :-0.4743   Min.   :-0.4734   Min.   :-0.64806  
# 1st Qu.:-0.4743   1st Qu.:-0.4734   1st Qu.:-0.48887  
# Median :-0.4743   Median :-0.4734   Median :-0.35719  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000  
# 3rd Qu.: 0.4325   3rd Qu.:-0.4734   3rd Qu.:-0.02423  
# Max.   : 6.7804   Max.   : 6.9702   Max.   : 9.66174   



#####################################################
# use a decision tree model to predict survival
#####################################################

# install and load the "rpart" package to use generate decision tree models and the "rpart.plot" package to print nice plots of the trees we generate

#install.packages("rpart")      ## Uncomment to install rpart
#install.packages("rpart.plot") ## Uncomment to install rpart.plot
library(rpart)
library(rpart.plot)


# 1. Create a new decision tree - titanic.model_DT.Sex
# that includes the following variables from the titanic training set:
# Survived ~ Sex

titanic.model_DT.Sex <- (Survived ~ Sex) # formula for model fitting

tree.Sex <- rpart(titanic.model_DT.Sex, # Predict survival based on gender
                     data = titanic.train # Use the titanic training data
                     )

# Plot the decision tree
prp(tree.Sex)

# It appears that the rpart() function managed to create 
# our simple gender based model: 
# if a passenger is a male the model gives him a 19% chance of survival 
# while non males have a 74% chance of survival. 


# 2. Create a new decision tree that adds the passenger class variable:

# make a decision tree model - titanic.model_DT.SexPclass
# that includes the following variables from the titanic training set:
# Survived ~ Sex + Pclass 

titanic.model_DT.SexPclass <- (Survived ~ Sex + Pclass) # formula for model fitting

tree.SexPclass <- rpart(titanic.model_DT.SexPclass, # Predict survival based on gender
                     data = titanic.train # Use the titanic training data
)

# Plot the decision tree
prp(tree.SexPclass)

 
# Adding more variables to a decision tree lets it create 
# more branches resulting in a more complex model 
# with greater expressive power. 

# In this case we see that within each gender, the model assigns 
# a lower survival probability to passenger with lower passenger classes: 
# men of class 3 and 2 only have a 14% chance of survival 
# while women of classes 2 and 1 have a 95% chance of survival. 

# It is interesting to note, however, that despite this new layer of branches, 
# the classification predictions this model would output 
# is the same as the original gender based model: 
# all males still have a survival probability below 0.5 
# and all women have a survival probability of 0.5 or higher.


# 3. Create a more complex decision tree adding a few more variables

titanic.model_DT.Complex <- (Survived ~ Sex + Pclass + Age + SibSp + Fare + Embarked) # formula for model fitting

tree.Complex <- rpart(titanic.model_DT.Complex,
                      cp = 0.001,             # Set complexity parameter*
                      data = titanic.train)   # Use the titanic training data

prp(tree.Complex)

# The plot above illustrates how complex decision trees can become 
# when you start adding several explanatory variables. 
# A model that is too complex is prone to overfitting the training data, 
# which can lead to poor generalization to unseen data. 

# The rpart() function includes several optional parameters 
# that you can set to control model complexity. 
# As noted above, the "cp" parameter lets you adjust model complexity 
# (cp adjusts the improvement of the model fit necessary for it to create a new branch.). 

# Apart from the complexity parameter you can also adjust 
# the maximum depth of the tree 
# and the minimum number of observations at each leaf node 
# to limit model complexity.


# 3. Create a less complex decision tree

limited_complexity_tree <- rpart(Survived ~ Sex + Pclass + Age + SibSp +Fare+Embarked,
                                 cp = 0.001,              # Set complexity parameter
                                 maxdepth = 5,            # Set maximum tree depth
                                 minbucket = 5,           # Set min number of obs in leaf nodes
                                 method = "class",        # Return classifications instead of probs
                                 data = titanic.train)    # Use the titanic training data

# Plot the decision tree
prp(limited_complexity_tree)

# The model above seems a little more reasonable. 
# Let's use this model to generate predictions on the training set and check the accuracy with a confusion matrix:

# Return class predictions
train_preds <- predict(limited_complexity_tree, 
                       newdata = titanic.train, 
                       type="class")              

confusionMatrix(train_preds, titanic.train$Survived)
# Confusion Matrix and Statistics
# 
# Reference
# Prediction   0   1
# 0 496  93
# 1  53 249
# 
# Accuracy : 0.8361          
# 95% CI : (0.8102, 0.8599)
# No Information Rate : 0.6162          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.6458          
# Mcnemar's Test P-Value : 0.001248        
# 
# Sensitivity : 0.9035          
# Specificity : 0.7281          
# Pos Pred Value : 0.8421          
# Neg Pred Value : 0.8245          
# Prevalence : 0.6162          
# Detection Rate : 0.5567          
# Detection Prevalence : 0.6611          
# Balanced Accuracy : 0.8158          
# 
# 'Positive' Class : 0 


# The tree model has an accuracy of 0.8414 on the training set, 
# but we'd expect it to perform well on the data set used to create it. 



#####################################################
# a decision tree model to make predictions on the Titanic test set
#####################################################

# Let's use the model to make predictions on the Titanic test data 
# and submit them to Kaggle to see how well it performs on unseen data:


# Import data set
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# data pre-proces

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.test$Pclass <- ordered(titanic.test$Pclass,     
                               levels=c("3","2","1")) 

# Impute missing data 
library(caret)

# Impute missing Ages only with the numeric variables: Age, sibSp, Parch and Fare
# preProcess() function scales and centers the data before imputation
colnames(titanic.test)

# Impute missing test set ages using the previously constructed imputation model
set.seed(42)
titanic.test_imp <- predict(impute, titanic.test[, c(5:7,9)])

titanic.test <- cbind(titanic.test[,-c(5:7,9)], #not numeric columns
                      titanic.test_imp
                      )

# check test data
summary(titanic.test)
# PassengerId     Pclass      Name               Sex           
# Min.   : 892.0   3:218   Length:418         Length:418        
# 1st Qu.: 996.2   2: 93   Class :character   Class :character  
# Median :1100.5   1:107   Mode  :character   Mode  :character  
# Mean   :1100.5                                                
# 3rd Qu.:1204.8                                                
# Max.   :1309.0                                                
# Ticket             Cabin             Embarked        
# Length:418         Length:418         Length:418        
# Class :character   Class :character   Class :character  
# Mode  :character   Mode  :character   Mode  :character  
# 
# 
# 
# Age               SibSp              Parch         
# Min.   :-2.03278   Min.   :-0.47428   Min.   :-0.47341  
# 1st Qu.:-0.52656   1st Qu.:-0.47428   1st Qu.:-0.47341  
# Median :-0.11697   Median :-0.47428   Median :-0.47341  
# Mean   : 0.02594   Mean   :-0.06859   Mean   : 0.01334  
# 3rd Qu.: 0.51292   3rd Qu.: 0.43255   3rd Qu.:-0.47341  
# Max.   : 3.18734   Max.   : 6.78036   Max.   :10.69205  
# Fare         
# Min.   :-0.64806  
# 1st Qu.:-0.48917  
# Median :-0.35719  
# Mean   : 0.06837  
# 3rd Qu.:-0.01474  
# Max.   : 9.66174  


#####################################################
# use a decision tree model to predict survival
#####################################################

test_preds <- predict(limited_complexity_tree,              
                      newdata = titanic.test,      
                      type = "class") 

prediction_sub <- data.frame(PassengerId=titanic.test$PassengerId, Survived=test_preds)

write.csv(prediction_sub, "titanic-model_DT1-submission.csv", row.names=FALSE)

