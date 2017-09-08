# Model ForestTree - std solution - ver 3:  Data Science Dojo
# SScore 0.77990
# SScores: 2a - 0.78947,2 - 0.75598, )

# RF - Model 3

colnames(titanic.full)

# Keeping only relevant features and discarding as much as possible to avoid
# over-fitting. 

# New features to consider:

# Family_Size
titanicData$Family_Size = titanicData$SibSp+titanicData$Parch + 1

# Social_Status
titanicData$Is_Mr = (regexpr('Mr.|Don|Capt|Jonkheer|Rev|Col', titanicData$Name) > 0)
titanicData$Is_Mrs = (regexpr('Mrs.|Countess|Mme', titanicData$Name) > 0)
titanicData$Is_Miss = (regexpr('Miss|Mlle|Ms', titanicData$Name) > 0)
titanicData$Social_Status = 100 * as.integer(titanicData$Is_Miss) +
  10 * as.integer(titanicData$Is_Mrs)  +
  as.integer(titanicData$Is_Mr)

# Embarkation_Port as integer
titanicData$Embarkation_Port = as.integer((factor(titanicData$Embarked)))

# Social_Deck
titanicData$Deck = factor(substring(titanicData$Cabin,1,1))
titanicData$Deck = as.integer(titanicData$Deck)
# Number of Decks: 5 & 2 Partial
# A, B, C, D, E, F, G
# ?T

# Number of Cabins: 
Passenger capacity:
First class: 735
Second class: 674
Third class: 1026
Crew: 885
Number of lifeboats: 20; capacity 1178 persons


# Social_Fare_Per_Person
titanicData$Fare_Per_Person = titanicData$Fare/(titanicData$Family_Size+1)


#

# "score" feature so called "log likelihood ratio" is slowly extracted out of the raw features

# Extract Family Name 
# This will be used later to help finding groups
comb$Surname <- sapply(as.character(comb$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]});

# Create a fare factor
comb$FareFac <- factor(comb$Fare);

# Predict missing fares
faremiss <- which(is.na(comb$Fare)); # missing fares (only PassengerId = 1044 is missing)
modelf   <- train( FareFac ~ Pclass + Sex + Embarked + SibSp + Parch, data = comb, trControl = trControl, method="rpart", na.action = na.pass);
comb$FareFac[faremiss] = predict(modelf, comb[faremiss,]); 
comb$Fare[faremiss] <- as.numeric(as.character(comb$FareFac[faremiss]));


# Engineer GID - group Ids
# Compute Ticket, Fare, and Cabin frequencies
# These are used to detect potentially related individuals
comb$TFreq <- ave(seq(nrow(comb)), comb$Ticket,  FUN=length);
comb$FFreq <- ave(seq(nrow(comb)), comb$FareFac, FUN=length);
comb$CFreq <- ave(seq(nrow(comb)), comb$Cabin,   FUN=length);

# Secret sauce #1. Try to find connection between individuals through various means.
# Engineer group Ids (GID) using SibSp, Parch, TFreq, FFreq, CFreq to label groups
comb$GID <- rep(NA, nrow(comb));
maxgroup <- 12; # maximum size of a group
for ( i in comb$PassengerId) {
  if(comb$SibSp[i] + comb$Parch[i] > 0) { # Check first if ID has relatives 
    comb$GID[i] <- paste0(comb$Surname[i], comb$SibSp[i] + comb$Parch[i]);	
  } else {
    if(comb$TFreq[i] > 1 & is.na(comb$GID[i])) { # Next if shares ticket number with others 
      comb$GID[i] <- as.character(comb$Ticket[i]);
    } else {
      if(comb$CFreq[i] > 1 & comb$CFreq[i]<maxgroup & is.na(comb$GID[i])) { # Next if shares cabin with others
        comb$GID[i] <- as.character(comb$Cabin[i]);
      }
      else {
        if(comb$FFreq[i] > 1 & comb$FFreq[i]<maxgroup & is.na(comb$GID[i])) { # Next if shares Fare value with others
          comb$GID[i] <- as.character(comb$FareFac[i]);
        } else { 
          comb$GID[i] <- "Single"; # Individual doesn't belong to any group
        }
      }
    }	
  }
}
comb$GID <- factor(comb$GID);

# Example of why ticket values might tell potential groups difficult to be distinguished other features
print(comb %>% filter(GID=="6.75")) # young couple?


# Calculate fare per person for those with same ticket numbers
comb$FareScaled <- comb$Fare/comb$TFreq;

# Predict missing Embarked
embarkmiss <- which(comb$Embarked == ''); 
modelp <- train( Embarked ~ Pclass + Sex + FareScaled, data = comb, trControl = trControl, method="rpart", na.action = na.pass);
comb$Embarked[embarkmiss] = predict(modelp, comb[embarkmiss, ]); 


# Transform Survived to factors for binary classification
comb$Survived <- factor(comb$Survived);
