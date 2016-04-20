

paste(names(Train.r)[9:47], collapse = "+") -> IVs
paste("factor(A.wins)", IVs, sep  = "~") -> formula
##error about NANs; summary shows the issue may be SOS_SR.r.  Removed from IV list here:
IVs2<- "Net_wins+Net_losses+Pyth.r+AdjustedO.r+AdjustedD.r+AdjustedT.r+SOS_by_Pyth.r+SOS_by_OppO.r+SOS_by_OppD.r+NC_SOS_by_Pyth.r+Net_SRS+FGA.r+FGpc.r+Triple_Att.r+Triple_pc.r+FTA.r+FT.r+FT_pc.r+OffReb.r+TotReb.r+Assist.r+Steal.r+Blocks.r+TurnOvers.r+PF.r+Pace.r+Off_Rating.r+FTA_per_FGA.r+FGA.pc_from_three.r+True_shooting_pc.r+TotReb_pc.r+Assist_pc.r+Steal_pc.r+Block_pc.r+eff_FG_pc.r+TurnOver_pc.r+OffReb_pc.r+FT_per_FGA.r"
paste("factor(A.wins)", IVs2, sep  = "~") -> formula2

##Weights #2
#Weighted towards close games
wts <- 1 / (1 + abs(log10(Train.r$Pyth.r)) + (1- (1*Train.r$Tournament))) 
wts / 10 -> wts
LogModel3 <- glm(as.formula(formula2), data=Train.r, family= "binomial", weights = wts)
Train.r$Log.Prob3 <- predict(LogModel3, newdata = Train.r, type="response")
Test.r$Log.Prob3 <- predict(LogModel3, newdata= Test.r, type= "response")

ggplot(Train.r, aes(x=Log.Prob3, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Test.r,  aes(x=Log.Prob3, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Train.r, aes(x=Log.Prob3)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Train.r, aes(x=Log.Prob3)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=Log.Prob3)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=Log.Prob3)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Distribution of Logistic Regression Predictions") +
  xlab("Confidence of A to win") + ylab("Counts of A (1) or B (0) wins")

print("All games:")
logloss(Train.r$A.wins, Train.r$Log.Prob3)
print("Tournament games only:")
logloss(Test.r$A.wins, Test.r$Log.Prob3)

##############

#clean environment
rm(formula)

    