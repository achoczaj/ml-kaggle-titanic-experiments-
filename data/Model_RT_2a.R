# Model 2a:  Data Science Dojo
# SScore: 0.78947 (2: 0.75598, )

# RF - Model 2a 
# 1.  build an ordinary linear regression (using least squares) 
# to predict the missing values for Age 
# equation.Age = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
# 
# use upper.whisker.Age = 66 as filter for an imput data
# 
# 2.  reuse an ordinary linear regression (using least squares) 
# to predict the missing values for Fare (only 1 value )
# equation.Fare = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
# 
# 3. feed predicted values into dataset
# 
# 4. build a RF predictive model
# equation.survived <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
#
# 5. apply the predictive model
# features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
#
############################################################



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

# Combine train and test datasets.
titanic.full <- rbind(titanic.train, titanic.test)
## View(titanic.full) ##
str(titanic.full)
tail(titanic.full)

#
# Data Exploration.
#

# Data Dictionary

# Variable:	  Definition:	          Key:
# survival	  Survival	            0 = No, 1 = Yes
# pclass	    Ticket class	        1 = 1st, 2 = 2nd, 3 = 3rd
# sex	        Sex	
# Age	        Age in years	
# sibsp	      # of siblings / spouses aboard the Titanic	
# parch	      # of parents / children aboard the Titanic	
# ticket	    Ticket number	
# fare	      Passenger fare	
# cabin	      Cabin number	
# embarked	  Port of Embarkation	    C = Cherbourg, Q = Queenstown, S = Southampton

# Variable Notes
#
# pclass: A proxy for socio-economic status (SES)
# 1st = Upper
# 2nd = Middle
# 3rd = Lower
#
# age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5
#
# sibsp: The dataset defines family relations in this way...
#  Sibling = brother, sister, stepbrother, stepsister
#  Spouse = husband, wife (mistresses and fiancés were ignored)
#
# parch: The dataset defines family relations in this way...
#  Parent = mother, father
#  Child = daughter, son, stepdaughter, stepson
#  Some children travelled only with a nanny, therefore parch=0 for them.
#



#
# Check data to see if there are missing values.
#

# TBD - find better way better to find mis val 

length(which(!complete.cases(titanic.train)))
#[1] 177
table(is.na(titanic.full))
# 682 NA
# including 418 NA is for added titanic.test$Survived  


########################
# Preprocessing Data.
########################

# 1. Clean missing values of Embarked class
table(titanic.full$Embarked)
# There are 2 missing values.
# The mode value for titanic.full dataset is 'S'
# so replace missing valuses with a mode value.

# cleaning missing values with a filter (only query column Embarked)
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

# check after cleaning 
table(titanic.full$Embarked)
# is OK


# 
# Inicial Categorical casting.
#

# In R, categorical data is stored in factors.
# Numeric vectors are used to store **continuous** variables.
# 

#    Do categorical casting of data 
#    for every column except "Survived"
#    because the "Survived" column 
#    has three categories at this moment:
#    0, 1  and NA.  
table(is.na(titanic.full$Survived))
#    if you do categorical casting of "Survived" now
#    you lose the binary classification  


# Assign vectors as factors to variables.
# nominal categorical variables:
titanic.full$Sex <- as.factor(titanic.full$Sex) 
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

## titanic.full$Pclass <- as.factor(titanic.full$Pclass) ##

# Test option: Consider Ticket and Cabin to be a nominal category
## titanic.full$Ticket <- as.factor(titanic.full$Ticket) ##
## titanic.full$Cabin <- as.factor(titanic.full$Cabin) ##
# A nominal variables is a categorical variable without an implied order.
## table(titanic.full$Cabin) ##

# ordinal categorical variables:
# Convert to ordered factor
titanic.full$Pclass <- ordered(titanic.full$Pclass, levels = c("3","2","1")) #levels = c(1,2,3) ##
table(titanic.full$Pclass)

# Test option: Consider SibSp and Parch to be an ordinal category
## titanic.full$SibSp <- as.ordered(titanic.full$SibSp) #levels = c(0,1,2,..,8) ##
## titanic.full$Parch <- as.ordered(titanic.full$Parch) #levels = c(0,1,2,...,9) ##
# Ordinal variables do have a natural ordering like for example `"Low"`, `"Medium"` and `"High"`.
## table(titanic.full$SibSp) ##
## table(titanic.full$Parch) ##


str(titanic.full)
#$ Pclass     : Ord.factor w/ 3 levels "1"<"2"<"3"
#$ Sex        : Factor w/ 2 levels "female","male"
#$ Embarked   : Factor w/ 3 levels "C","Q","S"



#2. Clean missing values of Age column.

table(is.na(titanic.full$Age))
# There are 263 missing values.

# Build a regression model to predict the missing values of Age class

# variant a) linear regression using gradient descent
# variant b) ordinary linear regression using least squares 
#    Ordinary linear regression is very sensitive to Outliers. It can terribly affect the regression line and eventually the forecasted values.

# Develop variant b) and filter the outliers

# use boxplot to identify the outliers
boxplot(titanic.full$Age)
summary(titanic.full$Age)

# derive value of upper whisker (upper bound) as a limit for outliers
## boxplot.stats(titanic.full$Age) ##
## boxplot.stats(titanic.full$Age)$stats ##
## boxplot.stats(titanic.full$Age)$stats[5] ##
upper.whisker.Age <- boxplot.stats(titanic.full$Age)$stats[5]
upper.whisker.Age

# build a logic filter
outliers.filter.Age <- titanic.full$Age < upper.whisker.Age
# we do not have outliers below the minimum = 0

#  filter the data - only rows that are not with outlier
filtered.data.Age <- titanic.full[outliers.filter.Age, ]

# create formula for the model
## str(titanic.full)
equation.Age = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"

# build and fit the model
lm.model.Age <- lm(
  formula = equation.Age,
  data = filtered.data.Age
)

lm.model.Age
# Call:
#   lm(formula = equation.Age, data = filtered.data.Age)
# 
# Coefficients:
#   (Intercept)     Pclass.L     Pclass.Q      Sexmale         Fare        SibSp        Parch  
# 29.662178    10.745621     2.546251     2.252937    -0.004917    -3.153610    -0.645513  
# EmbarkedQ    EmbarkedS  
# 4.045609     2.117625 


#########
# see results
anova(lm.model.Age)
summary(lm.model.Age)

# Multiple R-squared:  0.2412,	Adjusted R-squared:  0.2353 
# F-statistic: 40.77 on 8 and 1026 DF,  p-value: < 2.2e-16



## opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
## plot(Age.lm.model, las = 1)      # Residuals, Fitted, ...
## par(opar)
##########


# find rows with missing values of Age class
## is.na(titanic.full$Fare) ##

# build a query for rows with missing values of Fare and specific columns
NArows.Age <- titanic.full[
  is.na(titanic.full$Age),
  c("Pclass", "Sex", "Fare", "SibSp", "Parch","Embarked")           
  ]

# predict missing values
predictions.Age <- predict(
  lm.model.Age,
  newdata = NArows.Age 
)

# throw back predictons into dataset
titanic.full[is.na(titanic.full$Age), "Age"] <- predictions.Age

# check after cleaning 
table(is.na(titanic.full$Age))
# OK 




#3. Clean missing values of Fare class.

table(is.na(titanic.full$Fare))
# There are 1 missing value.
median(titanic.full$Fare, na.rm = TRUE)
# 14.4542

# Build a regression model to predict the missing value of Fare

# variant a) linear regression using gradient descent
# variant b) ordinary linear regression using least squares 
#    Ordinary linear regression is very sensitive to Outliers. It can terribly affect the regression line and eventually the forecasted values.

# Develop variant b) and filter the outliers

# use boxplot to identify the outliers
boxplot(titanic.full$Fare)
summary(titanic.full$Fare)

# derive value of upper whisker (upper bound) as a limit for outliers
## boxplot.stats(titanic.full$Fare)
## boxplot.stats(titanic.full$Fare)$stats
## boxplot.stats(titanic.full$Fare)$stats[5]
upper.whisker.Fare <- boxplot.stats(titanic.full$Fare)$stats[5]
upper.whisker.Fare

# build a logic filter
outliers.filter.Fare <- titanic.full$Fare < upper.whisker.Fare
# we do not have outliers below the minimum = 0

#  filter the data - only rows that are not with outlier
filtered.data.Fare <- titanic.full[outliers.filter.Fare, ]

# create formula for the model
## str(titanic.full)
equation.Fare = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"

# build and fit the model
lm.model.Fare <- lm(
  formula = equation.Fare,
  data = filtered.data.Fare
  )

#########
# see results
anova(lm.model.Fare)
summary(lm.model.Fare)

## opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
## plot(fare.lm.model, las = 1)      # Residuals, Fitted, ...
## par(opar)
##########

# find rows with missing values of Fare
## is.na(titanic.full$Fare) ##

# build query for rows with missing values of Fare and specific columns
rows.Fare <- titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex","Age", "SibSp", "Parch","Embarked")           
]

# predict missing values
predictions.Fare <- predict(
  lm.model.Fare,
  newdata = rows.Fare #here only row
)

# throw back predictons into dataset
predictions.Fare
##    1044 
##  8.187402
median(titanic.full$Fare, na.rm = TRUE)
## [1] 14.4542

titanic.full[is.na(titanic.full$Fare), "Fare"] <- predictions.Fare

# check cleaning 
table(is.na(titanic.full$Fare))
# OK



# 4. checking missing values
table(is.na(titanic.full))
## 418 NA is for added titanic.test$Survived




#
# Split titanic.full to train and test datasets
#

# Split dataset back into train and test datasets
write.csv(titanic.full, file="titanic_full_2.csv", row.names = FALSE)

# Split dataset back into train and test datasets

titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE, ]
tail(titanic.train)

titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE, ]
#or use '!'[not operator] for the flipped the true to false: titanic.test <- titanic.full[!titanic.full$IsTrainSet==TRUE, ]
tail(titanic.test)


#
# Final check of factors in train dataset
#
str(titanic.train)

# Make categorical casting of Survived class in train dataset 
titanic.train$Survived <- as.factor(titanic.train$Survived)

# Check
str(titanic.train)
#$ Survived   : Factor w/ 2 levels "0","1"
# So this is a binary classification problem



################################## 
# B. Building a predictive model
##################################

# B.1. Define in R which variables (columns) are predictors for this model
# and which variables (columns) are ignored
# when you send those into predictive model

#
# Building a formula

# standard initial formula for the Random Forest model:
# randomForest(Survived ~ .)
# use everything execpt Survived to predict Survived
# this requried to drop everything you did not want to use as predictor


# Explicitly call out which columns to use to build a predictive model
str(titanic.train)

# "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
equation.survived <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Build a set of relationships formula
survived.formula <- as.formula(equation.survived)

#  Predict Survived given these columns
## install.packages("randomForest")
library(randomForest)

# Call the Random Forest model
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
# Apply the predictive model
#

# C.1 Specify features
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
# Specify these features because you want to use PassangerID

# C.2 Run a prediction on the trained model
#  Create a new column with a results of prediction
Survived <- predict(
  titanic.model,  # model for prediction
  newdata = titanic.test # data for prediction 
)


#
# Build a dataframe to write this data as .csv
#

# .csv needs to have only two columns: PassengerID and Survived

# Isolate PassengerID and throw it to PassengerId var (a vector)
#   name of var is according to Kaggle req
PassengerId <- titanic.test$PassengerId

# Convert to dataframe
output.df <- as.data.frame(PassengerId)
# Throw Survived into dataframe as a secondary column
output.df$Survived <- Survived
# Check
tail(output.df)

# Write output.df dataframe as .csv
#  set row.names = FALSE to remove column with numbers of observation from csv file
write.csv(output.df, file="submission2a.csv", row.names = FALSE)

