# Model ADV template -  Divide and Conquer: Oscar Takeshita
# Score: [0.82297]

#
# This script is for those that have investigated the Titanic problem in depth
# but are having trouble getting a public score better than 0.8.
#
# The script was designed with these in mind:
# Keeping only relevant features and discarding as much as possible to avoid
# over-fitting. One of the most important charts appears to be Sex vs Pclass vs Survival so
# we first focus on it.
# 
# This script breaks the problem in sub-stages where a 
# "score" feature so called "log likelihood ratio" is slowly extracted out of
# the raw features. 
# In the end, we make the final model and prediction using just the log likelihood as a "super" feature.
#
# The script is still a work in progress. There appears to be places for improvement
# and we are working on them.

rm(list=ls());
inpath  <- "../data/";
outpath <- "../data/";

kaggle <- F;
if(!dir.exists(inpath)) {
  kaggle  <- T;
  inpath  <- "../input/"; # changing path to Kaggle's environment
  outpath <- "";
}
train <- read.csv(paste0(inpath,"train.csv"));
test  <- read.csv(paste0(inpath,"test.csv"));

test_index <- seq(418)+891; # test set of 418 items starts from index 891
library(plyr); # load plyr prior to dplyr to avoid warnings
library(caret);
library(dplyr);
library(gridExtra);

# training control for caret
trControl <- trainControl(method="repeatedcv", number=7, repeats=5);

# Join together the test and train sets for joint pre-processing of features
# Create an indicator tag for train and test sets
test$Survived <- NA;
test$Set  <- "Test";
train$Set <- "Train";
comb <- rbind(train, test);

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

# Predict missing embarked entries
embarkmiss <- which(comb$Embarked == ''); 
modelp <- train( Embarked ~ Pclass + Sex + FareScaled, data = comb, trControl = trControl, method="rpart", na.action = na.pass);
comb$Embarked[embarkmiss] = predict(modelp, comb[embarkmiss, ]); 

# Transform Survived to factors for binary classification
comb$Survived <- factor(comb$Survived);

# The next plot shows that Age information is significantly more reliable for Pclass 1 and 2 than Pclass 3
plot_has_age  <- ggplot(comb, aes(Pclass,fill=!is.na(Age))) + geom_bar(position="dodge") + labs(title="Passenger Has Age",fill="Has Age")

# Using the previous obsevation, we plot the density for survival per age for Pclass 1 and 2.
plot_age_survival_density  <- ggplot(comb %>% filter(Set=="Train", Pclass!=3), aes(Age)) + geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass 1 and 2");
print(plot_age_survival_density)
print(plot_has_age);

# The previous graphs support the case that children under 14 for Pclass 1 and 2 have high likelihood of survival and other age bands is likely to have
# little impact for predictions. 
# We therefore try to identify Minors. Other than that, Age will be ignored. 
child <- 14;
MinorFare <- summarize(group_by(comb , Fare), n = mean(Age)) %>% filter(n<child) %>% .$Fare
comb$Minor <- ifelse(comb$Age<child&comb$Pclass!=3, 1, 0);
comb$Minor <- ifelse(is.na(comb$Minor), 0, comb$Minor);

# define function to compute log likelihood of a/(1-a)
logl <- function(a) {
  a <- max(a,0.1); # avoids log(0)
  a <- min(a,0.9); # avoids division by 0
  return (log(a/(1-a)));
}

# Secret sauce #2: Engineer a log likelihood survival feature. 
# The idea is to consolidate Survival likelihood to a single scalar metric
comb$SLogL <- rep(0,nrow(comb));

# Calculate the log likelihood ratio of survival probability as function of sex and pclass
for (sex in c("male", "female")) {
  for (class in c(1:3)) {
    comb$SLogL[comb$Sex==sex& comb$Pclass==class] <- logl(nrow(comb %>% filter(Survived==1, Sex==sex, Pclass==class))/nrow(comb %>% filter(Set=="Train", Sex==sex, Pclass==class)));
  }
}

# This plot confirms there is information about survival by looking at the frequency of duplicate tickets and fares
plot_fare_density <- ggplot(comb %>% filter(Set=="Train"), aes(x=FFreq, y=TFreq, color=Survived)) + geom_density_2d() + labs(title="Ticket Frequency and Fare Frequency Density");
print(plot_fare_density)

# The next plot shows the fate of passengers in class 2 is almost certain.
# Later will compute the log likelihood ratio for each of the 6 areas in the grid.
p <- list();
item <- 1;
ylim <- 300;
for(class in c(1:3)){
  for(sex in c("male", "female")) {
    p[[item]] <- ggplot(comb %>% filter(Set=="Train", Pclass==class, Sex==sex), aes(x=Survived)) + geom_bar(aes(fill=Survived)) + scale_y_continuous(limits=c(0,ylim)) + theme(legend.position="none") + labs(title=paste('Pclass=', as.character(class), sex));
    item <- item + 1;
  }
}
print(do.call(grid.arrange, p))

plot_fare <- ggplot(comb %>% filter(Set=="Train"), aes(x=FFreq, y=TFreq, color=Survived)) + geom_count(position=position_dodge(width=5)) + labs(title="Ticket and Fare Frequencies");
print(plot_fare)

# Next we reward or penalize groups of people by adjusting their SLogL values. 
ticket_stats <- comb %>% group_by(Ticket) %>% summarize(l = length(Survived), na = sum(is.na(Survived)), c = sum(as.numeric(Survived)-1, na.rm=T));

# 1) By incrementing the log likelihood score for groups larger than one that have survivors.
# Note we apply this bias only to groups that contain individuals we need to predict.
# Applying to all seems to add noise. It is something that's worth more investigation.
for ( i in 1:nrow(ticket_stats)) {
  plist <- which(comb$Ticket==ticket_stats$Ticket[i]);
  if(ticket_stats$na[i] > 0 & ticket_stats$l[i] > 1 & ticket_stats$c[i] > 0) {
    comb$SLogL[plist] <- comb$SLogL[plist] + 3;
  }
}

# 2) Prior to version 30 this was justified as penalizing singles
# However, after testing a range of constants (positive or negative other than 0) we conclude that this is 
# being used by the final optimizer to differente singles rather than a being interpreted as penalty or reward
sconst <- -2.1;
comb$SLogL[comb$GID=="Single"] <- comb$SLogL[comb$GID=="Single"] - sconst;

# 3) By penalizing large group sizes (See TFreq vs Pclass vs SLogL graph)
comb$SLogL[comb$TFreq ==  7] <- comb$SLogL[comb$TFreq == 7] - 3;
comb$SLogL[comb$TFreq ==  8] <- comb$SLogL[comb$TFreq == 8] - 1;
comb$SLogL[comb$TFreq == 11] <- comb$SLogL[comb$TFreq == 11] - 3;

# 4) By rewarding 100% survival in of Minors in Pclass 1 and Pclass 2.
comb$SLogL[comb$Minor==1] <- 8;

# Write a preprocessed feature set
if(!kaggle) {
  write.csv(comb, paste0(outpath,"comb.csv"), quote=c(4), row.names=F);
}

# TFreq vs Pclass vs SLogL graph
# this graph was used to tune item 3) above
plot_slogl <- ggplot(comb %>% filter(Set=="Train"), aes(x=Pclass, y=SLogL)) + geom_jitter(aes(color=Survived)) + facet_grid(  . ~ TFreq,  labeller=label_both) + labs(title="SLogL vs Pclass vs TFreq")
print(plot_slogl)

# The last modeling and predict uses SLogL as the sole feature.
# This should prevent a lot of potential over-fitting.
fms <- formula("Survived ~ SLogL"); 

set.seed(2017);
model_m <- train(fms, data = comb %>% filter(Set=="Train"), metric="Accuracy", trControl = trControl, method = "knn"); 
comb$Pred <- predict(model_m, comb);

df_final <- data.frame(PassengerId = comb$PassengerId[test_index], Survived=comb$Pred[test_index]);
write.csv(df_final, paste0(outpath,"pred_divconq.csv"), row.names =F, quote=F);