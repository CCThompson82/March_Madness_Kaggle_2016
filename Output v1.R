


Preds <- function(input, model){
  #populate tempDF based on input
  split_input <- function(input){
    unlist(str_split(input, "_"))[1] -> year 
    unlist(str_split(input, "_"))[2] -> a
    unlist(str_split(input, "_"))[3] -> b
    c(year, a, b) -> TempVec
    return(TempVec)
  }
  sapply(input, split_input) -> TempDF
  t(TempDF) -> TempDF
  as.data.frame((TempDF))-> TempDF
  names(TempDF) <- c("Year","A", "B")
  as.numeric(as.character(TempDF$Year)) -> TempDF$Year
  as.numeric(as.character(TempDF$A)) -> TempDF$A
  as.numeric(as.character(TempDF$B)) -> TempDF$B
  ratios(TempDF, TRUE) -> OP
  if(model == "randomforest"){
    #Predict prob of Awins using RF model
    Prob <- predict(RF2, newdata= OP, type = "prob")[,2]   
  }else if( model == "logistic"){
    Prob <- predict(LogModel3, newdata= OP, type = "response")  
  } else if(model=="KPL5") {
    #Use KP Log5 determination
    Prob <- KP5(OP) 
  } else if(model == "NaiveBayes")
    Prob <- predict(NB.model, newdata= OP, type="raw")[,2]
  return(Prob)
}
#Load submission prompt
SS <- read.csv("/Users/ccthomps/Documents/R Files/Kaggle comps/MarchMad/Submission5050(1).csv", header=T)

Preds(SS$Id, "randomforest") -> SS$RF
Preds(SS$Id, "logistic") -> SS$LR
Preds(SS$Id, "KPL5") -> SS$L5
Preds(SS$Id, "NaiveBayes") -> SS$NB


#mutate(SS, "RF" = ifelse(RF < 0.01, 0.01, RF)) -> SS
#mutate(SS, "RF" = ifelse(RF > 0.99, 0.99, RF)) -> SS

#Graphical
ggplot(SS, aes(x=RF)) + 
  geom_histogram(fill = "grey", binwidth=0.05,col = "black") +
  ggtitle("Distribution of Predictions binned by Confidence in RF") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(SS, aes(x=LR)) + 
  geom_histogram(fill = "grey", binwidth=0.05,col = "black") +
  ggtitle("Distribution of Predictions binned by Confidence in LR") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(SS, aes(x=L5)) + 
  geom_histogram(fill = "grey", binwidth=0.05,col = "black") +
  ggtitle("Distribution of Predictions binned by Confidence in Log5") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(SS, aes(x=NB)) + 
  geom_histogram(fill = "grey", binwidth=0.05,col = "black") +
  ggtitle("Distribution of Predictions binned by Confidence in NaiveBayes") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
#LogReg only
SS %>% transmute("Id" = Id,
                 "Pred" = LR) %>% write.csv("StageII LogReg.csv", row.names=F)
#RF only
SS %>% transmute("Id" = Id,
                 "Pred" = RF) %>% write.csv("StageII RandFor.csv", row.names=F)

#RF with LR
for(r in 1:dim(SS)[1]){
  SS$Mean_LR.RF[r] <- ((SS$RF[r] + SS$LR[r] + SS$NB[r]) / 3)
}
ggplot(SS, aes(x=Mean_LR.RF)) + 
  geom_histogram(fill = "grey", binwidth=0.05,col = "black") +
  ggtitle("Distribution of Predictions binned by Confidence in NaiveBayes") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )

SS %>% transmute("Id" = Id,
                 "Pred" = Mean_LR.RF) %>% write.csv("StageII Mean.csv", row.names=F)

##Test mean for log loss on tourney data
for(r in 1:dim(Test.r)[1]){
  Test.r$Mean_LR.RF[r] <- ((Test.r$RF2.Prob[r] + Test.r$Log.Prob3[r] + Test.r$NB.Prob[r]) / 3)
}
print("Tournament games only:")
logloss(Test.r$A.wins, Test.r$Mean_LR.RF)
ggplot(Test.r, aes(x=Mean_LR.RF)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.05, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Mean LR.RF Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=Mean_LR.RF)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.05,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Distribution of Mean LogReg and RF predictions") +
  xlab("Confidence of A to win") + ylab("Counts of A (1) or B (0) wins")
##Looks suitable.  Log Loss is good.  