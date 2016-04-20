NB.model <- naiveBayes(factor(A.wins)~Net_wins,
                       Net_losses,
                       Pyth.r,
                       AdjustedO.r,
                       AdjustedD.r,
                       Net_SRS,
                       True_shooting_pc.r,
                       TotReb_pc.r,
                       Off_Rating.r,
                       NC_SOS_by_Pyth.r,
                       SOS_by_Pyth.r,
                       data=Train.r, laplace = 1)
predict(NB.model, newdata = Train.r, type="raw")[,2] -> Train.r$NB.Prob
predict(NB.model, newdata = Test.r, type="raw")[,2] -> Test.r$NB.Prob

#graphical analysis
ggplot(Train.r, aes(x=NB.Prob, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Test.r,  aes(x=NB.Prob, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Train.r, aes(x=NB.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Train.r, aes(x=NB.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=NB.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=NB.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Distribution of Random ForestPredictions") +
  xlab("Confidence of A to win") + ylab("Counts of A (1) or B (0) wins")

print("All games:")
logloss(Train.r$A.wins, Train.r$NB.Prob)
print("Tournament games only:")
logloss(Test.r$A.wins, Test.r$NB.Prob)




