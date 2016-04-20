#Packages required
library(RSQLite)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(XML)
library(e1071)
library(ROCR)
library(rpart)
library(mgcv)
library(randomForest)
#Sources
source(file = "MM_Master_ref v2.R") #Generate Master
source(file = "MM_Create_Training_set v2.R") # Generate training data frame
source(file = "Log5 KenPomPyth v1.R") #KP Log5 predictions
source(file= "Logistic Regression v2.R") #Logistic Regression
source(file = "Naive Bayes v1.R") #Naive Bayes classification
source(file = "Random Forest v3.R") #Random Forest predictions
source(file = "Output v1.R") #Take SS$Id (game ID) into a Prob by pulling and transforming info
#from Master database.  