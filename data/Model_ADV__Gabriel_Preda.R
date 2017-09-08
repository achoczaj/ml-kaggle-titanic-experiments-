# Model ADV template: Gabriel Preda
# Score: ?

# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(randomForest)
library(mice)
library(caret)


# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

preprocessTitanicData <- function(titanicData, isTraining = FALSE) {
  
  mice_titanic_data <- mice(titanicData[, !names(titanicData) %in% 
                                          c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf')
  mice_titanic_data_out <- complete(mice_titanic_data)
  titanicData$Age <- mice_titanic_data_out$Age
  titanicData$Fare <- mice_titanic_data_out$Fare
  titanicData$Family_Size = titanicData$SibSp+titanicData$Parch + 1
  titanicData$Is_Mr = (regexpr('Mr.|Don|Capt|Jonkheer|Rev|Col', titanicData$Name) > 0)
  titanicData$Is_Mrs = (regexpr('Mrs.|Countess|Mme', titanicData$Name) > 0)
  titanicData$Is_Miss = (regexpr('Miss|Mlle|Ms', titanicData$Name) > 0)
  titanicData$Social_Status = 100 * as.integer(titanicData$Is_Miss) +
    10 * as.integer(titanicData$Is_Mrs)  +
    as.integer(titanicData$Is_Mr)
  
  titanicData$Embarkation_Port = as.integer((factor(titanicData$Embarked)))
  titanicData$Deck = factor(substring(titanicData$Cabin,1,1))
  titanicData$Deck = as.integer(titanicData$Deck)
  titanicData$Fare_Per_Person = titanicData$Fare/(titanicData$Family_Size+1)
  
  varNames <- c("Age","Sex", "Pclass", "Family_Size","Fare", "Social_Status") #0.78469
  
  if(isTraining)
    varNames <- c("Survived", varNames)
  
  titanicSet <- cbind(titanicData[,varNames]) 
  
  return(titanicSet);
}

# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")


#training set - will be split between training and verification set
raw.training.data <- read.csv("train.csv")
df <- preprocessTitanicData(raw.training.data, isTraining = TRUE)
# end of training set

# testing 
raw.testing.data <- read.csv("test.csv")
dt <- preprocessTitanicData(raw.testing.data, isTraining =  FALSE)
## end of testing set

# prepare to split the training set in training and verification
nrows <- nrow(df)
indexT <- sample(1:nrow(df), 0.7 * nrows)

#separate train and validation set
trainset = df[indexT,]
verset =   df[-indexT,]

n <- names(trainset)
rf.form <- as.formula(paste("Survived ~", paste(n[!n %in% "Survived"], collapse = " + ")))

trainset.rf <- randomForest(rf.form,
                            trainset,
                            ntree=800,
                            importance=T)
varImpPlot(trainset.rf,
           sort = T,
           main="Variable Importance",
           n.var=6)

# verification set (sampled from the total training set, not used in training)
verset$predicted.response <- round(predict(trainset.rf ,verset),0)

levels(verset$predicted.response)
#calculate confusion matrix for the verification set
cM <- confusionMatrix(data = verset$Survived, verset$predicted.response, positive = "1")

# Prediction with fresh, unknown data
submission <- data.frame(raw.testing.data$PassengerId)
# prediction for testing data
submission$Survived <- round(predict(trainset.rf ,dt),0)
colnames(submission) <- c("PassengerId","Survived")

#save result
write.csv(submission, file="submission-GP.csv", row.names = FALSE)
