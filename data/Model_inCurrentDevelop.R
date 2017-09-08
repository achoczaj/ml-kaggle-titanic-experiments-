# Model 
# SScore:


# Set working directory in R.
setwd("C:/Labs_ML/Kaggle/C01/data")

a.model <- "Current dev"


# Import data
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# Compare structure of these datasets
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


#=================================
#  Data Exploration
#=================================

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
features.names.rawData <- colnames(titanic.full)
features.names.rawData



## This counts "" (empty) and NA values as missing values ##

# Features in full set with missing values
sapply(titanic.full, function(x) any(is.na(x)) )
sapply(titanic.full, function(x) any(x=="") )

sapply(titanic.full, function(x) any(is.na(x))|any(x=="") )

## or: unlist(lapply(titanic.full, function(x) any(is.na(x))|any(x=="") )

# Show data type for each feature
sapply(titanic.full, class)

# Features in full set with missing values
sort(sapply(titanic.full, function(x) any(is.na(x))|any(x=="") ), decreasing = TRUE)


names(titanic.full)[sapply(titanic.full, function(x) any(is.na(x)) )]

names(titanic.full)[colSums(is.na(titanic.full))>0]


#======================
#
#  Preprocessing Data  
#
#======================

#---------------------------
# Feature - Embarked class
#---------------------------

# Check the Embarked class for empty values
table(titanic.full$Embarked)
## There are 2 missing values
## The mode value is 'S'

# Code records with NA values
titanic.full$Embarked_IsMissing <- ifelse(is.na(titanic.full$Embarked), TRUE, FALSE)

# Find missing values with a filter (only query column Embarked)
ID.NAvalues.Embarked <- which(titanic.full$Embarked=='')
# NAvalues.Embarked <- which(is.na(titanic.full$Embarked)); 
ID.NAvalues.Embarked

titanic.full[62,]
titanic.full[830,]
## The same Ticket (113572), Cabin (B28), Fare (80) 

# Check missing values 
# use a filter: only query column Embarked for ""
titanic.full$Embarked==""

# Replace missing valuses with the mode value
# titanic.full[titanic.full$Embarked=="", "Embarked"] <- "S"

# Check data 
table(titanic.full$Embarked)
table(titanic.full$Survived, titanic.full$Embarked)
prop.table(table(titanic.full$Survived, titanic.full$Embarked), 2)
## TO DO: 55% passengers from C survived - why ?


# Cast the feature as categorical (nominal) variable
titanic.full$Embarked <- as.factor(titanic.full$Embarked)



#------------------
#  Feature - Name             
#------------------

# 1. Check the Name class for empty values
table(titanic.full$Name=="") 
## OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$Name))
## OK: no mising values

#------------------------
# new feature - Surname          
#------------------------

# Extract family name from Name
titanic.full$Surname <- sapply(as.character(titanic.full$Name), function(x) {strsplit(x, split='[,."()]')[[1]][1]});
## This will be used later to help finding families

# Convert the feature as nominal categorical variable
titanic.full$Surname <- as.factor(titanic.full$Surname)

# See some details
sort(table(titanic.full$Surname), decreasing = TRUE)
## the most popular surnames: Andersson, Sage - 11 cases

# Count unique surnames
length(unique(titanic.full$Surname))
## 875 unique surnames in data set

#-----------------------
#  New feature - Title          
#-----------------------

# Extract pasenger's title
titanic.full$Title <- sapply(as.character(titanic.full$Name), function(x) {strsplit(x, split='[,."()]')[[1]][2]})
# Return string w/o leading whitespace
titanic.full$Title <- sapply(as.character(titanic.full$Title), function (x)  sub("^\\s+", "", x))

# See some details
sort(table(titanic.full$Title), decreasing = TRUE)
## the most popular title: Mr - 757 cases

# See details in a Data Viewer
colnames(titanic.full)
Titles_bySex_byAge <- titanic.full[ c("Title", "Sex", "Surname", "Name", "Age", "SibSp", "Parch", "Ticket",  "Cabin")]
View(Titles_bySex_byAge)

# See aggregated data
aggregate(Age ~ Sex + Title, titanic.full, mean)
aggregate(Age ~ Sex + Title, titanic.full, length)

aggregate(Survived ~ Title, titanic.full, mean)

# Convert the feature as nominal categorical variable
titanic.full$Title <- as.factor(titanic.full$Title)


#-------------------------------
#  New Feature - TitlesGrouped       
#-------------------------------

## Group some titles indicating similar socio-demo groups 
unique(titanic.full$Title)

# Group Titles into few categories
for (i in 1:nrow(titanic.full) ) {
          ## "Title_Mr" - for adult men (married or unmarried)
          if( (titanic.full$Title[i] %in% c("Mr")) && titanic.full$Sex[i]=="male") {
          titanic.full$TitlesGrouped[i] <- "Title_Mr"
        } else if
          ## "Title_Sir" - for titles of nobility and professional honorifics for man 
          ( (titanic.full$Title[i] %in% c("Don", "Dr","Sir","Col","Capt", "Major","Jonkheer")) && titanic.full$Sex[i]=="male") {
          titanic.full$TitlesGrouped[i] <- "Title_Sir"
        } else if
          ## "Title_Rev" - for honorific titles of Christian clergy and ministers
          ( (titanic.full$Title[i] %in% c("Rev")) && titanic.full$Sex[i]=="male") {
          titanic.full$TitlesGrouped[i] <- "Title_Rev"
        } else if
          ## "Title_Mrs" - for married woman
          ## "Madame" (Mme) for a married woman. The plural is Mesdames (Mmes).
          ( (titanic.full$Title[i] %in% c("Mrs", "Mme")) && (titanic.full$Sex[i]=="female")) {
          titanic.full$TitlesGrouped[i] <- "Title_Mrs"
        } else if
          ## "Title_Lady" - for titles of nobility and professional honorifics for woman
          ( (titanic.full$Title[i] %in% c("Dr", "the Countess", "Lady", "Dona")) && (titanic.full$Sex[i]=="female")) {
          titanic.full$TitlesGrouped[i] <- "Title_Lady"
        } else if
          ## "Title_Miss" - for young or older unmarried woman
          ## "Mademoiselle" (Mlle) is a traditional alternative for an unmarried woman. The plural is Mesdemoiselles (Mlles).
          ( (titanic.full$Title[i] %in% c("Miss","Mlle", "Ms"))  && (titanic.full$Sex[i]=="female")) {
          titanic.full$TitlesGrouped[i] <- "Title_Miss"
        } else if
          ## "Title_Master" - for young unmarried men
          ( (titanic.full$Title[i]=="Master") && (titanic.full$Sex[i]=="male")) {
          titanic.full$TitlesGrouped[i] <- "Title_Master"
        } else {
          ## if-else control value for errors
          titanic.full$TitlesGrouped[i] <- "If-Else_Error_TitlesGrouped"
        }
}


# See details in a table
table(titanic.full$Survived, titanic.full$TitlesGrouped)
prop.table(table(titanic.full$Survived, titanic.full$TitlesGrouped), 2)

# See details in a Data Viewer
colnames(titanic.full)
Titles_bySex_byAge <- titanic.full[ c("TitlesGrouped", "Sex", "Surname", "Name", "Age", "SibSp", "Parch", "Ticket",  "Cabin")]
View(Titles_bySex_byAge)

# See aggregated data
aggregate(Age ~ TitlesGrouped, titanic.full, mean)

aggregate(Age ~ TitlesGrouped, titanic.full, length)
aggregate(Age ~ TitlesGrouped, titanic.full, min)
aggregate(Age ~ TitlesGrouped, titanic.full, max)

## The younger Age of TitlesGrouped:
# TitlesGrouped   Age
# Master          0.33
# Miss            0.17
# Mr              11.00 --> the most started at 14
# Mrs             14.00 --> married

## The older Age of TitlesGrouped:
# TitlesGrouped   Age
# Master          14.5  --> the most ended on 13
# Miss            63.0 -->
# Mr              80.0
# Mrs             76.0

## So take age < 15 as a limit for childhood

#-------------------------
#  New feature - IsChild             
#-------------------------

titanic.full$IsChild <- ifelse(titanic.full$Age < 15, TRUE, FALSE)

table(titanic.full$IsChild)
## 109 children (person with Age)

#-------------------------
#  New feature - IsAdult           
#-------------------------

titanic.full$IsAdult <- ifelse(titanic.full$Age >= 15, TRUE, FALSE)

table(titanic.full$IsAdult)
## 937 adults (person with Age)


# See details in a table
table(titanic.full$Survived, titanic.full$IsChild)
prop.table(table(titanic.full$Survived, titanic.full$IsChild), 2)

summary(titanic.full[titanic.full$IsChild==TRUE, "Age"])
hist(titanic.full[titanic.full$IsChild==TRUE, "Age"])


# See details in a Data Viewer
colnames(titanic.full)
IsChild_bySex <- titanic.full[ c("IsChild", "TitlesGrouped", "Sex", "Surname", "Age", "SibSp", "Parch", "Cabin", "Ticket")]
View(IsChild_bySex)

# See aggregated data
aggregate(Survived ~ TitlesGrouped, titanic.full, mean)
aggregate(Survived ~ TitlesGrouped + IsChild, titanic.full, mean)

aggregate(Age ~ TitlesGrouped + IsChild, titanic.full, length)
aggregate(Age ~ TitlesGrouped + IsChild, titanic.full, min)
aggregate(Age ~ TitlesGrouped + IsChild, titanic.full, max)



#----------------------------
#  Feature - Sex             
#----------------------------

# Check the Sex class for empty values
table(titanic.full$Sex=="") 
## OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$Sex))
## OK: no mising values

# Convert the feature as nominal categorical variable
titanic.full$Sex <- as.factor(titanic.full$Sex) 

# See details in a table
table(titanic.full$Sex)
table(titanic.full$Survived, titanic.full$Sex)
prop.table(table(titanic.full$Survived, titanic.full$Sex), 2)

# See aggregated data
aggregate(Survived ~ Sex, titanic.full, mean)



#----------------------------
#  Feature - Pclass           
#----------------------------

# Check the Pclass class for empty values
table(titanic.full$Pclass=="") 
## OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$Pclass))
## OK: no mising values

# Convert the feature as ordinal categorical variable
titanic.full$Pclass <- ordered(titanic.full$Pclass, levels = c("3","2","1")) 

# See details in a table
table(titanic.full$Pclass)
table(titanic.full$Survived, titanic.full$Pclass)
prop.table(table(titanic.full$Survived, titanic.full$Pclass), 2)

# See aggregated data
aggregate(Survived ~ Pclass, titanic.full, mean)



#----------------------------
#  Feature - Cabin            
#----------------------------

# Check the Cabin class for empty values
table(titanic.full$Cabin=="") 
## 1014 empty values

# Check if there are NA values
table(is.na(titanic.full$Cabin))
## No mising values

# Code records with NA values
titanic.full$Cabin_IsMissing <- ifelse(titanic.full$Cabin=="", TRUE, FALSE)

# See details in a table
table(titanic.full$Cabin_IsMissing)
table(titanic.full$Survived, titanic.full$Cabin_IsMissing)
prop.table(table(titanic.full$Survived, titanic.full$Cabin_IsMissing), 2)

# Change empty values to NA
titanic.full$Cabin[titanic.full$Cabin==''] <- NA

# Convert the feature as nominal categorical variable
titanic.full$Cabin <- as.factor(titanic.full$Cabin) 

# See details in a table
table(titanic.full$Cabin)
length(unique(titanic.full$Cabin))-1  ##subtract "NA" as a unique class 
## 186 Levels
## TO DO: Consider to reduce the number of levels for Cabin var using Fare

#------------------------------------------------------------------------
#  New features - Cabin1stNumb, Cabin2ndNumb, Cabin3rdNumb, Cabin4thNumb     
#------------------------------------------------------------------------

## Extract cabins' numbers from aggregated info 

# Extract First Cabin Number if Cabin var has many numbers
titanic.full$Cabin1stNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][1]})
table(titanic.full$Cabin1stNumb)

# Extract Secound Cabin Number if Cabin var has many numbers
titanic.full$Cabin2ndNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][2]})
table(titanic.full$Cabin2ndNumb)

# Extract 3rd Cabin Number if Cabin var has many numbers
titanic.full$Cabin3rdNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][3]})
table(titanic.full$Cabin3rdNumb)

# Extract 4th Cabin Number if Cabin var has many numbers
titanic.full$Cabin4thNumb <- sapply(as.character(titanic.full$Cabin), function(x) {strsplit(x, split='[ "]')[[1]][4]})
table(titanic.full$Cabin4thNumb)

# cabins.occupied <- cbind(titanic.full$Cabin1stNumb, titanic.full$Cabin2ndNumb, titanic.full$Cabin3rdNumb, titanic.full$Cabin4thNumb)

#------------------------------------------------------
#  New features - NumbOfCabinsPerTicket, FarePerCabin
#------------------------------------------------------

# Compare Fare with FarePerCabin 

for (i in 1:nrow(titanic.full) ) {
  titanic.full$NumbOfCabinsPerTicket[i] <- ifelse(is.na(titanic.full$Cabin1stNumb[i]),0,1) + ifelse(is.na(titanic.full$Cabin2ndNumb[i]),0,1) + ifelse(is.na(titanic.full$Cabin3rdNumb[i]),0,1) + ifelse(is.na(titanic.full$Cabin4thNumb[i]),0,1) 
  if (titanic.full$NumbOfCabinsPerTicket[i] >= 1) {
  titanic.full$FarePerCabin[i] <- titanic.full$Fare[i]/titanic.full$NumbOfCabinsPerTicket[i] }
  else { titanic.full$FarePerCabin[i] <- NA }
  }

# See more details in a table view
colnames(titanic.full)
titanic.TicketFareCabin <- titanic.full[ c("Surname","Sex", "Pclass", "Cabin", "Fare", "FarePerCabin", "NumbOfCabinsPerTicket")]
View(titanic.TicketFareCabin)

#----------------------------
#  New Feature - Deck         
#----------------------------

# Extract deck name from Cabin
titanic.full$Deck <- as.character(substring(titanic.full$Cabin, 1, 1))

# See the distribution of the feature
summary(as.factor(titanic.full$Deck))
# Number of Decks on the Titanic: 5 & 2 Partial (F and G)
# A, B, C, D, E, F, G
# ?T

# Correct name for non-existing deck T. It is a Boat Deck, Cabin T
titanic.full[ which(titanic.full$Deck=="T"), "Deck"] <- "Boat"

# Convert the feature as ordered factor (ordinal categorical variable)
titanic.full$Deck <- ordered(titanic.full$Deck, levels = c("Boat","A","B","C","D","E","F","G"))

# See the distribution of the feature
summary(titanic.full$Deck)

# Expert data about Titanic's decks and cabins - just to quick view 
titanic.decksCabins <- c("Boat"=6,"A"=36,"B"=107,"C"=148,"D"=136,"E"=167,"F"=204,"G"=41)
titanic.decksCabins

# See more details in a table view
colnames(titanic.full)
titanic.TicketFareCabin <- titanic.full[ c("Surname","Sex", "Pclass", "Cabin", "Deck", "Fare", "FarePerCabin", "NumbOfCabinsPerTicket")]
View(titanic.TicketFareCabin)


#---------------------------------
#  Feature - Ticket           
#---------------------------------

# Check the Ticket class
table(titanic.full$Ticket=="") 
## OK: no "" empty values

# Check if there are NA values
table(is.na(titanic.full$Ticket))
## OK: no mising values

# Convert the feature as nominal categorical variable
titanic.full$Ticket <- as.factor(titanic.full$Ticket) 

# See the distribution of the feature
summary(titanic.full$Ticket)


#---------------------------------------------------------
#  New features - TicketNumber, TicketCode, TicketCodeSTD 
#---------------------------------------------------------

# consider to refactor this code
# tck<-sapply(as.character(alldata$Ticket),function(s) strsplit(s," "))
# tick<-as.numeric(sapply(tck,function(s){
#   if(length(s)==1) s 
#   else if (length(s)==2) s[2]
#   else s[3]
# }))
# alldata$Ticket <-tick


# temp colums - Extract Ticket Digits
titanic.full$TicketPart1 <- NA
titanic.full$TicketPart2 <- NA
titanic.full$TicketPart3 <- NA

titanic.full$TicketPart1 <- sapply(as.character(titanic.full$Ticket), function(x) {strsplit(x, split='[ ]')[[1]][1]})
titanic.full$TicketPart2 <- sapply(as.character(titanic.full$Ticket), function(x) {strsplit(x, split='[ ]')[[1]][2]})
titanic.full$TicketPart3 <- sapply(as.character(titanic.full$Ticket), function(x) {strsplit(x, split='[ ]')[[1]][3]})

titanic.full$TicketNumber <- NA
titanic.full$TicketCode <- NA
titanic.full$TicketCodeSTD <- NA

for (i in 1:nrow(titanic.full)) {
  if (length(grep('^[[:digit:]]', titanic.full$TicketPart1[i])) == TRUE) { # Check if Part1 starts with digit 
    titanic.full$TicketNumber[i] <- titanic.full$TicketPart1[i]
      } else {
        if (length(grep('^[[:digit:]]', titanic.full$TicketPart3[i])) == TRUE) { # Check if Part3 starts with digit 
          titanic.full$TicketNumber[i] <- titanic.full$TicketPart3[i]
          titanic.full$TicketCode[i] <- c(titanic.full$TicketPart1[i], titanic.full$TicketPart1[i])
          } else {
            if (length(grep('^[[:digit:]]', titanic.full$TicketPart2[i])) == TRUE) { # Check if Part2 starts with digit 
              titanic.full$TicketNumber[i] <- titanic.full$TicketPart2[i]
              titanic.full$TicketCode[i] <- titanic.full$TicketPart1[i]
              } 
          }
      }
  }

# Drop temp columns
titanic.full$TicketPart1 <- NULL
titanic.full$TicketPart2 <- NULL
titanic.full$TicketPart3 <- NULL

# Convert the feature as nominal categorical variable
titanic.full$TicketNumber <- as.factor(titanic.full$TicketNumber)
summary(titanic.full$TicketNumber)

length(unique(titanic.full$TicketNumber))-1 ##subtract "NA" as a unique class
## 923 Levels


titanic.full$TicketCode <- toupper(titanic.full$TicketCode)
titanic.full$TicketCodeSTD <- gsub('[[:punct:] ]+', '', titanic.full$TicketCode)

titanic.full$TicketCode <- as.factor(titanic.full$TicketCode)
summary(titanic.full$TicketCode)

titanic.full$TicketCodeSTD <- as.factor(titanic.full$TicketCodeSTD)
summary(titanic.full$TicketCodeSTD)
length(unique(titanic.full$TicketCodeSTD))-1  ##subtract "NA" as a unique class
## 35 Levels

# See more detail in table view 
titanic.TicketFareCabin <- titanic.full[ c("Surname","Sex", "Age","Pclass", "Cabin", "Deck", "Ticket", "TicketCodeSTD", "TicketNumber", "Fare", "FarePerCabin", "NumbOfCabinsPerTicket")]
View(titanic.TicketFareCabin)

#-----------------------------------------------------
#  New features - GroupSize, FarePerPerson, GroupCode            
#-----------------------------------------------------

## GroupSize = Numb of pasangers with the same ticket number
## FarePerPerson = Fare / GroupSize
## GroupCode = TicketNumber + "_" + GroupSize


## It appears that fares for multiple tickets are a total fare for the group
## need to divide fare by number of times ticket number appears

temp.ticket <- table(titanic.full$TicketNumber)

ticket.counts <- data.frame(TicketNum=names(temp.ticket), counts <- as.numeric(temp.ticket) )

for (i in 1:nrow(titanic.full)){ 
  if (!is.na(titanic.full$Fare[i]) & !is.na(titanic.full$TicketNumber[i])) {
    titanic.full$GroupSize[i] <- ticket.counts[which(ticket.counts[, 1]==titanic.full$TicketNumber[i]), 2]
    titanic.full$FarePerPerson[i] <- titanic.full$Fare[i] / titanic.full$GroupSize[i]
    titanic.full$GroupCode[i] <- paste(titanic.full$TicketNumber[i], titanic.full$GroupSize[i], sep = "_" )
    } else {
      titanic.full$GroupSize[i] <- NA
      titanic.full$FarePerPerson[i] <- NA
      titanic.full$GroupCode[i] <- paste(titanic.full$TicketNumber[i], titanic.full$GroupSize[i], sep = "_" )
    }
  }

summary(titanic.full$GroupSize)
table(titanic.full$Survived, titanic.full$GroupSize)
prop.table(table(titanic.full$Survived, titanic.full$GroupSize),2)


summary(titanic.full$FarePerPerson)
summary(titanic.full$FarePerPerson[titanic.full$Pclass=="1"])
summary(titanic.full$FarePerPerson[titanic.full$Pclass=="2"])
summary(titanic.full$FarePerPerson[titanic.full$Pclass=="3"])


titanic.full$GroupCode <- as.factor(titanic.full$GroupCode)
summary(titanic.full$GroupCode)


# See more details in table view
colnames(titanic.full)
titanic.TicketFareCabin <- titanic.full[ c("Surname","Sex", "Age","Pclass", "Cabin", "Deck", "Ticket", "TicketCodeSTD", "TicketNumber", "Fare", "FarePerCabin", "FarePerPerson", "GroupCode")]
View(titanic.TicketFareCabin)


## TO DO: FarePerPerson_WithDiscounts
## children and dogs travelled at half fare in First Class.


# Compute Ticket, Fare, and Cabin frequencies
# These are used to detect potentially related individuals
# comb$TFreq <- ave(seq(nrow(comb)), comb$Ticket,  FUN=length);
# comb$FFreq <- ave(seq(nrow(comb)), comb$FareFac, FUN=length);
# comb$CFreq <- ave(seq(nrow(comb)), comb$Cabin,   FUN=length);



#----------------------------
#  Feature - SibSp             
#----------------------------

# Check the SibSp class
table(titanic.full$SibSp=="") 
# OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$SibSp))
# OK: no mising values

# Convert the feature as nominal categorical variable
# titanic.full$SibSp <- as.factor(titanic.full$SibSp) 
# Test option: Consider SibSp and Parch to be an ordinal category
## titanic.full$SibSp <- as.ordered(titanic.full$SibSp) #levels = c(0,1,2,..,8) ##



#----------------------------
#  Feature - Parch             
#----------------------------

# Check the Parch class
table(titanic.full$Parch=="") 
# OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$Parch))
# OK: no mising values

# Convert the feature as nominal categorical variable
# titanic.full$Parch <- as.factor(titanic.full$Parch)
# Test option: Consider SibSp and Parch to be an ordinal category
## titanic.full$Parch <- as.ordered(titanic.full$Parch) #levels = c(0,1,2,...,9) ##

# Ordinal variables do have a natural ordering like for example `"Low"`, `"Medium"` and `"High"`.
## table(titanic.full$SibSp) ##
## table(titanic.full$Parch) ##



#----------------------------
#  New Feature - FamilySize
#----------------------------

## FamilySize = Person + SibSp + Parch 
titanic.full$FamilySize = 1 + titanic.full$SibSp + titanic.full$Parch


# Check the FamilySize class
table(titanic.full$FamilySize=="") 
# OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$FamilySize))
# OK: no mising values

# Convert the feature as nominal categorical variable
titanic.full$FamilySize <- as.factor(titanic.full$FamilySize) 

# See the distribution of the feature
summary(titanic.full$FamilySize)
table(titanic.full$Survived, titanic.full$FamilySize)
prop.table(table(titanic.full$Survived, titanic.full$FamilySize), 2)

#----------------------------
#  New Feature - FamilyCode
#----------------------------

## FamilyCode = Surname + "_" + FamilySize 
titanic.full$FamilyCode <- paste(titanic.full$Surname, titanic.full$FamilySize, sep = "_" )

# Check the FamilyCode class
table(titanic.full$FamilyCode=="") 
# OK: no "" (empty) values

# Check if there are NA values
table(is.na(titanic.full$FamilyCode))
# OK: no mising values

# Convert the feature as nominal categorical variable
titanic.full$FamilyCode <- as.factor(titanic.full$FamilyCode) 

# See the distribution of the feature
summary(titanic.full$FamilyCode)


#----------------------------
#  New Feature - FamilyCategory
#----------------------------

## Discretize the family size into few categories for more clarity
titanic.full$FamilyCategory[titanic.full$FamilySize == 1] <- "FamilySize=1"
titanic.full$FamilyCategory[titanic.full$FamilySize == 2] <- "FamilySize=2"
titanic.full$FamilyCategory[titanic.full$FamilySize == 3] <- "FamilySize=3"
titanic.full$FamilyCategory[titanic.full$FamilySize == 4] <- "FamilySize=4"
titanic.full$FamilyCategory[titanic.full$FamilySize %in% c(5,6,7,8,9,10,11)] <- "FamilySize_>=5"



# titanic.full$FamilyCategory <- titanic.full$FamilySize.map(lambda s: )

# titanic.full['Alone'] = titanic.full['FamilySize'].map(lambda s : 1 if s==1 else 0)
# titanic.full['Couple'] = titanic.full['FamilySize'].map(lambda s : 1 if s==2 else 0)

# titanic.full['Family3'] = titanic.full['FamilySize'].map(lambda s : 1 if s>=3 else 0)
# almost the same is done by carot dummyVar



#----------------------------
#  New feature - PassengerCat       
#----------------------------

## 
for (i in 1:nrow(titanic.full)){ 
    if 
      (is.na(titanic.full[i, "Age"]) ) {
      titanic.full$PassengerCat[i] <- NA
    } else if 
      (titanic.full$Age[i] > 14 ){    
      titanic.full$PassengerCat[i] <- "adult"
    } else if 
      (titanic.full$Age[i]<=14 & titanic.full$Sex[i]=="female" & titanic.full$Title[i] == "Miss" ){
      titanic.full$PassengerCat[i] <- "girl"
    } else if 
      (titanic.full$Age[i]<=14 & titanic.full$Sex[i]=="female" & titanic.full$Title[i] == "Mrs" ){
      titanic.full$PassengerCat[i] <- "adult"
    } else if 
      (titanic.full$Sex[i]=="male" & titanic.full$Age[i]<=14 ){
      titanic.full$PassengerCat[i] <- "boy"
    } else {
      titanic.full$PassengerCat[i] <- NA
    }
}        

# Convert the feature as nominal categorical variable
titanic.full$PassengerCat <- as.factor(titanic.full$PassengerCat) 

# See the distribution of the feature
summary(titanic.full$PassengerCat)
table(titanic.full$Survived, titanic.full$PassengerCat)
prop.table(table(titanic.full$Survived, titanic.full$PassengerCat), 2)


#----------------------------
#  New feature - IsMother             
#----------------------------

# it needs development and using Age differences
# new feature - IsMother (female, Mrs, with one or more children)
# ?? Age > 14

for (i in 1:nrow(titanic.full)){ 
  if 
  (
  #   is.na(titanic.full[i, "Age"]) ) {
  #   titanic.full$IsMother[i] <- NA
  # } else if 
  # (titanic.full$Age[i] > 14 & 
  titanic.full$Sex[i]=="female" & titanic.full$Title[i] == "Mrs" & titanic.full$Parch[i]>0){
    titanic.full$IsMother[i] <- TRUE
  # } else if 
  # (titanic.full$Age[i]<=14 & titanic.full$Sex[i]=="female" & titanic.full$Title[i] == "Mrs" ){
  #   titanic.full$PassengerCat[i] <- "adult"
  # } else if 
  # (titanic.full$Sex[i]=="male" & titanic.full$Age[i]<=14 ){
  #   titanic.full$PassengerCat[i] <- "boy"
  } else {
    titanic.full$IsMother[i] <- FALSE
  }
}        

# Convert the feature as nominal categorical variable
titanic.full$IsMother <- as.factor(titanic.full$IsMother) 

# See the distribution of the feature
summary(titanic.full$IsMother)
table(titanic.full$Survived, titanic.full$IsMother)
prop.table(table(titanic.full$Survived, titanic.full$IsMother), 2)











#########################################
#  Predicting missing values - Fare, Age, ..
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

