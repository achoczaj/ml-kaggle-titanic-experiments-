# Model 2a - LM-Age, Fare_CV-caret, RF
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

a.RT_2c <- "Current model RT 2c"


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


##################################
# Data Exploration.
###########################

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



# 3.2 Preprocessing Data - Checking Current Features
Features.names <- colnames(titanic.full)
Features.names

# Features in train set with missing values
lapply(titanic.train, function(x) any(is.na(x)))

# Features in full set with missing values
lapply(titanic.full, function(x) any(is.na(x)))

  


#######################
# Preprocessing Data  #
#######################


############################
# Feature - Embarked class #
############################

# 1. Clean missing values of Embarked class
table(titanic.full$Embarked)
# There are 2 missing values.
# The mode value for titanic.full dataset is 'S'
# so replace missing valuses with a mode value.

# Imply missing values with a filter (only query column Embarked)
titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

# check after cleaning 
table(titanic.full$Embarked)
# is OK

# Convert the feature as nominal categorical variable
titanic.full$Embarked <- as.factor(titanic.full$Embarked)


##############################
# Feature - Name             #
##############################
 
# Check if there are mising values
table(is.na(titanic.full$Name))
# OK: no mising values

##############################
# Feature - Surname          #
##############################

# Extract Family Name from Name 
# This will be used later to help finding groups
titanic.full$Surname <- sapply(as.character(titanic.full$Name), function(x) {strsplit(x, split='[,."()]')[[1]][1]});

# Convert the feature as nominal categorical variable
titanic.full$Surname <- as.factor(titanic.full$Surname)


##############################
# Feature - Sex             #
##############################

# Check if there are mising values
table(is.na(titanic.full$Sex))
# OK: no mising values

# Convert the feature as nominal categorical variable
titanic.full$Sex <- as.factor(titanic.full$Sex) 

##############################
# Feature - Pclass           #
##############################

# Check if there are mising values
table(is.na(titanic.full$Pclass))
# OK: no mising values

# Convert the feature as ordinal categorical variable
titanic.full$Pclass <- ordered(titanic.full$Pclass, levels = c("3","2","1")) 
table(titanic.full$Pclass)

##############################
# Feature - Cabin            #
##############################

# Convert the feature as nominal categorical variable
titanic.full$Cabin <- as.factor(titanic.full$Cabin) 


titanic.full$Cabin[titanic.full$Cabin==''] <- NA


# Check if there are mising values
table(is.na(titanic.full$Cabin))
# 1014 missing values

# See the distribution of the feature
summary(titanic.full$Cabin)


# Extract First Cabin Number if many
titanic.full$Cabin1stNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][1]})
table(titanic.full$Cabin1stNumb)

# Extract Secound Cabin Number if many
titanic.full$Cabin2ndNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][2]})
table(titanic.full$Cabin2ndNumb)

# Extract 3rd Cabin Number if many
titanic.full$Cabin3rdNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][3]})
table(titanic.full$Cabin3rdNumb)

# Extract 4th Cabin Number if many
titanic.full$Cabin4thNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][4]})
table(titanic.full$Cabin4thNumb)


##############################
# New Feature - Deck         #
##############################
titanic.full$Deck <- as.character(substring(titanic.full$Cabin, 1, 1))

## titanic.full$Deck <- as.factor(titanic.full$Deck) 
## See the distribution of the feature
## summary(titanic.full$Deck)
# Number of Decks on the Titanic: 5 & 2 Partial (F and G)
# A, B, C, D, E, F, G
# ?T

# Correct error - non-existing deck T
# Imply error value with NA using filter (only query column Deck)
# titanic.full[titanic.full$Deck=='T', "Deck"] <- ''

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.full$Deck <- ordered(titanic.full$Deck, levels = c("A","B","C","D","E","F","G","T"))

# See the distribution of the feature
summary(titanic.full$Deck)


##############################
# Feature - Ticket           #
##############################

# Check if there are mising values
table(is.na(titanic.full$Ticket))
# OK: no mising values

# Convert the feature as nominal categorical variable
titanic.full$Ticket <- as.factor(titanic.full$Ticket) 

# See the distribution of the feature
summary(titanic.full$Ticket)


##########################################################
# New Features - TicketDigits, TicketCode, TicketCodeSTD #
##########################################################

# temp colums - Extract Ticket Digits
titanic.full$TicketPart1 <- NA
titanic.full$TicketPart2 <- NA
titanic.full$TicketPart3 <- NA

titanic.full$TicketPart1 <- sapply(as.character(titanic.full$Ticket), function(x) {strsplit(x, split='[ ]')[[1]][1]})
titanic.full$TicketPart2 <- sapply(as.character(titanic.full$Ticket), function(x) {strsplit(x, split='[ ]')[[1]][2]})
titanic.full$TicketPart3 <- sapply(as.character(titanic.full$Ticket), function(x) {strsplit(x, split='[ ]')[[1]][3]})

titanic.full$TicketDigits <- NA
titanic.full$TicketCode <- NA
titanic.full$TicketCodeSTD <- NA

for (i in 1:nrow(titanic.full)) {
  if (length(grep('^[[:digit:]]', titanic.full$TicketPart1[i])) == TRUE) { # Check if Part1 starts with digit 
    titanic.full$TicketDigits[i] <- titanic.full$TicketPart1[i]
      } else {
        if (length(grep('^[[:digit:]]', titanic.full$TicketPart3[i])) == TRUE) { # Check if Part3 starts with digit 
          titanic.full$TicketDigits[i] <- titanic.full$TicketPart3[i]
          titanic.full$TicketCode[i] <- c(titanic.full$TicketPart1[i], titanic.full$TicketPart1[i])
          } else {
            if (length(grep('^[[:digit:]]', titanic.full$TicketPart2[i])) == TRUE) { # Check if Part2 starts with digit 
              titanic.full$TicketCode[i] <- titanic.full$TicketPart1[i]
              titanic.full$TicketDigits[i] <- titanic.full$TicketPart2[i]
            } 
          }
      }
  }

titanic.full$TicketPart1 <- NULL
titanic.full$TicketPart2 <- NULL
titanic.full$TicketPart3 <- NULL

titanic.full$TicketDigits <- as.integer(titanic.full$TicketDigits)

titanic.full$TicketCode <- toupper(titanic.full$TicketCode)
titanic.full$TicketCodeSTD <- gsub('[[:punct:] ]+','', toupper(titanic.full$TicketCode))

titanic.full$TicketCode <- as.factor(titanic.full$TicketCode)
summary(titanic.full$TicketCode)

titanic.full$TicketCodeSTD <- as.factor(titanic.full$TicketCodeSTD)
summary(titanic.full$TicketCodeSTD)

View(titanic.full)


colnames(titanic.full)
titanic.TicketFareCabin <- titanic.full[ c(1,14,5,6,9, 23,20, 10, 3, 19,11)]
View(titanic.TicketFareCabin)

# Compute Ticket, Fare, and Cabin frequencies
# These are used to detect potentially related individuals
# comb$TFreq <- ave(seq(nrow(comb)), comb$Ticket,  FUN=length);
# comb$FFreq <- ave(seq(nrow(comb)), comb$FareFac, FUN=length);
# comb$CFreq <- ave(seq(nrow(comb)), comb$Cabin,   FUN=length);



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







#########################################
# Cleaning and predicting missing values - Fare, Age, ..
########################################

#2. Cleaning and predicting missing values of Fare class.

# Check if there are mising values
table(is.na(titanic.full$Fare))
# 1: no mising values

# Find missing values
NAvalues.Fare <- which(is.na(titanic.full$Fare)); 
NAvalues.Fare 

titanic.full[is.na(titanic.full$Fare), ]
# for PassengerId = 1044, Ticket = 3701, Pclass = 3
# ticket 3701 - no similar ticket numbers 3xxx
# there are 
# no family, 


# Count median value of Fare 
median.Fare <- median(titanic.full$Fare, na.rm = TRUE)
# 14.4542

# filter on Fare by Pclass==3
median.Fare.3class <- median(titanic.full[titanic.full$Pclass==3, "Fare"], na.rm = TRUE)
# 8.05
summary(titanic.full[titanic.full$Pclass==3, "Fare"])
hist(titanic.full[titanic.full$Pclass==3, "Fare"], breaks = 40)

# filter on Fare by Pclass==2
median.Fare.2class <- median(titanic.full[titanic.full$Pclass==2, "Fare"], na.rm = TRUE)
# 15.0458
summary(titanic.full[titanic.full$Pclass==2, "Fare"])
hist(titanic.full[titanic.full$Pclass==2, "Fare"], breaks = 40)

# filter on Fare by Pclass==1
median.Fare.1class <- median(titanic.full[titanic.full$Pclass==1, "Fare"], na.rm = TRUE)
# 60
summary(titanic.full[titanic.full$Pclass==1, "Fare"])
hist(titanic.full[titanic.full$Pclass==1, "Fare"], breaks = 40)

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
summary(filtered.data.Fare)
dim(filtered.data.Fare)

#################################
###############################
###############################


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


#####################################
#####################################
#####################################

#3. Cleaning and predicting missing values of Age column.

table(is.na(titanic.full$Age))

# Convert values of Age to integer
titanic.full$AgeINT <- as.integer(titanic.full$Age)
table(titanic.full$AgeINT)
summary(titanic.full$AgeINT)
length(titanic.full$AgeINT)

# Adjust Age to 1 for values < 1 
titanic.full[which(titanic.full$AgeINT < 1), ]

titanic.full[which(titanic.full$AgeINT < 1), "AgeINT"] <- 1
table(titanic.full$AgeINT)
summary(titanic.full$AgeINT)
length(titanic.full$AgeINT)
# There are 263 missing values for AgeINT.


# Build a regression model to predict the missing values of Age class

# variant a) linear regression using gradient descent
# variant b) ordinary linear regression using least squares 
#    Ordinary linear regression is very sensitive to Outliers. It can terribly affect the regression line and eventually the forecasted values.

# Develop variant b) and filter the outliers

# use boxplot to identify the outliers
boxplot(titanic.full$AgeINT)

# derive value of upper whisker (upper bound) as a limit for outliers
## boxplot.stats(titanic.full$Age) ##
## boxplot.stats(titanic.full$Age)$stats ##
## boxplot.stats(titanic.full$Age)$stats[5] ##
upper.whisker.AgeINT <- boxplot.stats(titanic.full$AgeINT)$stats[5]
upper.whisker.AgeINT

# build a logic filter
outliers.filtered.AgeINT <- titanic.full$AgeINT < upper.whisker.AgeINT
table(outliers.filtered.AgeINT)

# we do not have outliers below the minimum = 1 

#show records with missing value of Age and AgeINT
titanic.full[which(is.na(titanic.full$AgeINT)), ]
titanic.full[is.na(titanic.full$AgeINT), ]

NA.filtered.AgeINT <- !(is.na(titanic.full$AgeINT))


#  filter the data - only rows that are not with outlier
temp.AgeINT <- titanic.full[outliers.filtered.AgeINT, ]
summary(temp.AgeINT$AgeINT)
dim(temp.AgeINT)

#  filter the data - only rows with a value
train.AgeINT <- titanic.full[NA.filtered.AgeINT, ]
summary(train.AgeINT$AgeINT)
dim(train.AgeINT)

lapply(train.AgeINT, function(x) any(is.na(x)))

table(is.na(train.AgeINT$Age))
# no NA values in train.AgeINT$Age
table(is.na(train.AgeINT))

# create formula for the model
## str(titanic.full)
equation.AgeINT = "Age ~ Pclass + Sex + Fare + SibSp + Parch + Embarked"
formula.AgeINT <- as.formula(equation.AgeINT)
# build and fit the model
# lm.model.Age <- lm(
#   formula = equation.Age,
#   data = filtered.data.Age
# )
library(lattice)
library(ggplot2)
library(caret)


# Set seed
set.seed(42)

# Fit lm model using 5-fold CV: model
lm.model.AgeINT <- train(
  formula.AgeINT, 
  train.AgeINT,
  method = "lm", 
  na.action = na.omit,
  trControl = trainControl(
    method = "cv", 
    number = 5,
    repeats = 1, # repeat your entire cross-validation procedure 5 times
    verboseIter = FALSE
    )
)

# Print model to console
lm.model.AgeINT

# No pre-processing
# Resampling: Cross-Validated (2 fold) 
# Summary of sample sizes: 522, 523 
# Resampling results:
#   
#   RMSE      Rsquared 
#   12.58715  0.2377346



#########
# see results
summary(lm.model.AgeINT)

# Residual standard error: 12.56 on 1036 degrees of freedom
# Multiple R-squared:  0.244,	Adjusted R-squared:  0.2382 
# F-statistic:  41.8 on 8 and 1036 DF,  p-value: < 2.2e-16



## opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
## plot(Age.lm.model, las = 1)      # Residuals, Fitted, ...
## par(opar)
##########



#######################
#######################
#######################




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

