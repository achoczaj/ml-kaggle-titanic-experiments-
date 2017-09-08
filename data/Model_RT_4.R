# Model 4:  Random Forest
# SScoreL 
# Archiv SScores: 3-0.77990, 2a - 0.78947,2 - 0.75598, )

# RF - Model 4
# Using feature engineering to create new features

#
# 



# 1. Impute missing data for Age class using nearest-neighbor method - knnimpute
#      preProcess(
#        titanic_train[,c(6:8,10)],  # Impute missing values
#        method=c("knnImpute")       # using nearest-neighbor method
#      )
# 
# 2.  use an ordinary linear regression (using least squares) 
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


# library(ggplot)


# Step 1 - Import raw data

# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")

# Import data sets
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# Add mising column Survived in titanic.test dataset and set it as NA.
titanic.test$Survived <- NA


# Step 2
# Join together the test and train sets for joint pre-processing of features
# Create an indicator tag for train and test sets
titanic.train$IsTrainingSet <- TRUE
titanic.test$IsTrainingSet <- FALSE

# Combine by rows train and test datasets
titanic.full <- rbind(titanic.train, titanic.test)



# check result 
## View(titanic.full) ##
## str(titanic.full) ## 
## tail(titanic.full) ## 


# Step 3
# 3.1 Data Exploration

# Data Dictionary
# Variable:	  Definition:	          Key:
# Survival	  Survival	            0 = No, 1 = Yes
# Pclass	    Ticket class	        1 = 1st, 2 = 2nd, 3 = 3rd
# Sex	        Sex	
# Age	        Age in years	
# Sibsp	      # of siblings / spouses aboard the Titanic	
# Parch	      # of parents / children aboard the Titanic	
# Ticket	    Ticket number	
# Fare	      Passenger fare	
# Cabin	      Cabin number	
# Embarked	  Port of Embarkation	   C = Cherbourg, Q = Queenstown, S = Southampton
# IsTrainSet  Is from training set   TRUE / FALSE  

# Variable Notes
#
# Pclass: A proxy for socio-economic status (SES)
# 1st = Upper
# 2nd = Middle
# 3rd = Lower
#
# Age: Age is fractional if less than 1. If the age is estimated, is it in the form of xx.5
#
# Sibsp: The dataset defines family relations in this way...
#  Sibling = brother, sister, stepbrother, stepsister
#  Spouse = husband, wife (mistresses and fiancés were ignored)
#
# Parch: The dataset defines family relations in this way...
#  Parent = mother, father
#  Child = daughter, son, stepdaughter, stepson
#  Some children travelled only with a nanny, therefore parch=0 for them.


# 3.2 Preprocessing Data - Features
Features.names <- colnames(titanic.full)
Features.names

# Features in train set with missing values
lapply(titanic.train, function(x) any(is.na(x)))

# Features in full set with missing values
lapply(titanic.full, function(x) any(is.na(x)))



###############################
# 3.2.1. Feature - Survived
#
# Transform Survived to factors for binary classification
titanic.full$Survived <- factor(titanic.full$Survived);
titanic.train$Survived <- factor(titanic.train$Survived);

#titanic.train$Survived <- ordered(titanic.train$Survived, levels = c(1,0))

table(titanic.train$Survived)

# rate of Survived based on titanic.train data
342/(549+342)
# 0.3838384

# Check if there are mising values
table(is.na(titanic.train$Survived))
# OK: 0 mising values 
table(is.na(titanic.full$Survived))
# OK: 418 mising values -> all from titanic.test



##############################
# 3.2.2. Feature - Pclass
#
# Convert the feature as ordered factor (ordinal categorical variable)
titanic.full$Pclass <- ordered(titanic.full$Pclass, levels = c(3,2,1))
titanic.train$Pclass <- ordered(titanic.train$Pclass, levels = c(3,2,1))

summary(titanic.full$Pclass)

# See the distribution of people across classes
table(titanic.train$Pclass, titanic.train$Survived)
plot(titanic.train$Pclass, titanic.train$Survived)

# Check if there are mising values
table(is.na(titanic.full$Pclass))
# OK: no mising values

## NAvalues.Pclass <- is.na(titanic.full$Pclass) 
## NAvalues.rows.Pclass <- which(is.na(comb$Fare))

# Plots
# install.packages("ggplot2")
# install.packages("magrittr")

library(ggplot2)
library(magrittr)
# Plot the counts for survival per Pclass
plot.Pclass_Survival_count  <- ggplot(titanic.full %>% filter('IsTrainingSet'==TRUE), aes('Pclass')) + geom_bar(alpha=0.9, aes(fill=factor('Survived'))) + labs(title="Survival count per Pclass")
print(plot.Pclass_Survival_count)


##############################
# 3.2.3. Feature - Name
#
# 
# Check if there are mising values
table(is.na(titanic.full$Name))
# OK: no mising values


# Extract Pasenger's Name 
## This will be used later to help finding groups
titanic.full$Surname <- sapply(as.character(titanic.full$Name), function(x) {strsplit(x, split='[,."()]')[[1]][1]});
table(titanic.full$Surname)
# Convert the feature as nominal categorical variable
titanic.full$Surname <- as.factor(titanic.full$Surname)


# Extract Pasenger's Title
titanic.full$Title <- sapply(as.character(titanic.full$Name), function(x) {strsplit(x, split='[,."()]')[[1]][2]});
table(titanic.full$Title)
# Convert the feature as nominal categorical variable
titanic.full$Title <- as.factor(titanic.full$Title)

##############################
# 3.2.4. Feature - Sex
#
# Convert the feature as nominal categorical variable
titanic.full$Sex <- as.factor(titanic.full$Sex) 
titanic.train$Sex <- as.factor(titanic.train$Sex)

# It was better to be a woman to survive - so try casting as ordinal categorical variable:
# titanic.full$Sex <- ordered(titanic.full$Sex, levels = c("female","male"))
# titanic.train$Sex <- ordered(titanic.train$Sex, levels = c("female","male"))

# Check if there are mising values
table(is.na(titanic.full$Sex))
# OK: no mising values

# See the survival distribution of the feature
table(titanic.train$Sex, titanic.train$Survived)
plot(titanic.train$Sex, titanic.train$Survived)

# Plots
# Plot the density for survival per Sex
plot.Sex_Survival_count  <- ggplot(titanic.full %>% filter(IsTrainSet=="TRUE"), aes(Sex)) + geom_bar(alpha=0.9, aes(fill=factor(Survived))) + labs(title="Survival count per Sex")
print(plot.Sex_Survival_count)


##############################
# 3.2.4. Feature - Age [num] 
#
# Check if there are mising values
table(is.na(titanic.full$Age))
# !!! 263 mising values

# See the distribution of the feature
summary(titanic.full$Age)

# See the survival distribution of the feature
table(titanic.train$Age, titanic.train$Survived)

hist(titanic.train$Age, breaks = summary(titanic.full$Age)[6]/5)
hist(titanic.train$Age, breaks = 40)

# Plots
# # Plot the density for survival per Sex

# The next plot shows that Age information is significantly more reliable for Pclass 1 and 2 than Pclass 3
plot.Pas_has_Age  <- ggplot(titanic.full, aes(Pclass, fill=!is.na(Age))) + geom_bar(position="dodge") + labs(title="Passenger Has Age",fill="Has Age")

# Using the previous obsevation, we plot the density for survival per age for Pclass 1 and 2.
plot.Age_survival_density_12  <- ggplot(titanic.full %>% filter(IsTrainSet=="TRUE", Pclass!=3), aes(Age)) + geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass 1 and 2");
plot.Age_survival_density_123  <- ggplot(titanic.full %>% filter(IsTrainSet=="TRUE"), aes(Age)) + geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass 1, 2 and 3");
print(plot.Age_survival_density_12)
print(plot.Age_survival_density_123)
print(plot.Pas_has_Age)

########################
# Check for logical errors reg age
# Predict mising values for Age
# create new eng feature AgeClass: Infant, YoungChil, Child, YoungAdult, Adult (has a baby), ElderyPerson >= 60?
########################


##############################
# 3.2.5. Feature - SibSp
# SibSp: The dataset defines family relations in this way...
#  Sibling = brother, sister, stepbrother, stepsister
#  Spouse = husband, wife (mistresses and fiancés were ignored)

# Check if there are mising values
table(is.na(titanic.full$SibSp))
# OK: no mising values

# See the survival distribution of the feature
table(titanic.train$SibSp, titanic.train$Survived)

# See the distribution of the feature
summary(titanic.full$SibSp)
hist(titanic.train$SibSp)


##############################
# 3.2.6. Feature - Parch
# Parch: The dataset defines family relations in this way...
#   Parent = mother, father
#   Child = daughter, son, stepdaughter, stepson
#   Some children travelled only with a nanny, therefore parch=0 for them.

# Check if there are mising values
table(is.na(titanic.full$Parch))
# OK: no mising values

# See the survival distribution of the feature
table(titanic.train$Parch, titanic.train$Survived)

# See the distribution of the feature
summary(titanic.full$Parch)
hist(titanic.train$Parch)

########################
# Check for Parents (women) with one or more children
########################


##############################
# 3.2.7. New Feature - FamilySize
# FamilySize = Person + SibSp + Parch 
titanic.full$FamilySize = 1 + titanic.full$SibSp + titanic.full$Parch
titanic.train$FamilySize = 1 + titanic.train$SibSp + titanic.train$Parch

# Check if there are mising values
table(is.na(titanic.full$FamilySize))
# OK: no mising values

# See the survival distribution of the feature
table(titanic.train$FamilySize, titanic.train$Survived)
table(titanic.train$FamilySize, titanic.train$Survived, titanic.train$Pclass)

# See the distribution of the feature
summary(titanic.full$FamilySize)
hist(titanic.train$Parch)

# df['Alone'] = df['FamilySize'].map(lambda s : 1 if s == 1 else 0)
# df['Couple'] = df['FamilySize'].map(lambda s : 1 if s==2 else 0)
# df['Family'] = df['FamilySize'].map(lambda s : 1 if 3<=s else 0)
# almost the same is done by carot dummyVar



########################
# Consider new feature - Mather (women) with one or more children
# Age >= 15
########################





##############################
# 3.2.8. Feature - Ticket

# Convert the feature as nominal categorical variable
titanic.full$Ticket <- as.factor(titanic.full$Ticket) 

# Check if there are mising values
table(is.na(titanic.full$Ticket))
# OK: no mising values



##############################
# 3.2.x. New Feature - TicketOnlyNumber

# TBD with RegEx

# 3.2.x. New Feature - TicketOnlyNumberType
# e.g. 3xxx or 3xxxxx

# TBD with RegEx



##############################
# 3.2.9. Feature - Fare

# Convert the feature as nominal categorical variable
# titanic.full$Fare <- as.factor(titanic.full$Fare) 

# Check if there are mising values
table(is.na(titanic.full$Fare))
# 1: no mising values

# Find missing values
NAvalues.Fare <- which(is.na(titanic.full$Fare)); 
NAvalues.Fare 
# for PassengerId = 1044, Ticket = 3701
# no family, no similar ticket numbers 3xxx

# See the survival distribution of the feature
table(titanic.train$Fare, titanic.train$Survived)

# See the distribution of the feature
summary(titanic.full$Fare)
hist(titanic.train$Fare)

# big outliers
# Check if there are mising values
which(titanic.full$Fare>=300)
# 1: no mising values

########################
# Predict missing values of Fare
########################



##############################
# 3.2.8. Feature - Cabin

# Convert the feature as nominal categorical variable
titanic.full$Cabin <- as.factor(titanic.full$Cabin) 

# Check if there are mising values
table(is.na(titanic.full$Cabin))
# ???: no mising values
# should be 1014 missing values

# See the distribution of the feature
summary(titanic.full$Cabin)
hist(titanic.train$Cabin)


##############################
# 3.2.8. New Feature - Deck

titanic.full$Deck <- as.character(substring(titanic.full$Cabin, 1, 1))

## titanic.full$Deck <- as.factor(titanic.full$Deck) 
## See the distribution of the feature
## summary(titanic.full$Deck)
# Number of Decks on the Titanic: 5 & 2 Partial (F and G)
# A, B, C, D, E, F, G
# ?T -> tank top is the lowest deck of a ship, below the Orlop Deck. On the Titanic, it housed the engines and boiler rooms


# Convert the feature as ordered factor (ordinal categorical variable)
titanic.full$Deck <- ordered(titanic.full$Deck, levels = c("A","B","C","D","E","F","G","T"))

# See the distribution of the feature
summary(titanic.full$Deck)


##############################
# 3.2.x. Feature - Embarked
# Port of Embarkation	   S = Southampton -> C = Cherbourg -> Q = Queenstown, 

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.full$Embarked <- ordered(titanic.full$Embarked, levels = c("S","C","Q")) 
titanic.train$Embarked <- ordered(titanic.train$Embarked, levels = c("S","C","Q"))

summary(titanic.full$Embarked)

# Check if there are mising values
table(is.na(titanic.full$Embarked))
# 2 mising values

# See the survival distribution of the feature
table(titanic.train$Embarked, titanic.train$Survived)
table(titanic.full$Embarked, titanic.full$Pclass)

# See the distribution of the feature
summary(titanic.full$Embarked)

########################
# Predic missing values of Embarked
#
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




###############################################################
###############################################################
###############################################################

# Predictions of missing values


###############################################################
###############################################################
###############################################################
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

# Build a 

# use boxplot to identify the outliers
boxplot(titanic.full$Age)
summary(titanic.full$Age)

install.packages("lattice")
install.packages("ggplot2")
install.packages("caret")
library(lattice)
library(ggplot2)
library(caret)


# predict missing values
impute <- preProcess(titanic.full[,c(6:8,10)],  # Impute missing ages
                     method=c("knnImpute"))

titanic.full.imp <- predict(impute, titanic.full[,c(6:8,10)])     

# throw back predictons into dataset
titanic.full <- cbind(titanic.full[,-c(6:8,10)], titanic.full.imp)

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



# TBD ? Create a new feature - FareFactor
# titanic.full$FareFacor <- factor(titanic.full$Fare); 



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
write.csv(output.df, file="submission3.csv", row.names = FALSE)

