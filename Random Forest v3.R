

####################################
#Generate model2
formula3 <- factor(A.wins)~Net_wins+Net_losses+Pyth.r+AdjustedO.r+AdjustedD.r+SOS_by_Pyth.r+SOS_by_OppO.r+SOS_by_OppD.r+Net_SRS+Off_Rating.r+True_shooting_pc.r+eff_FG_pc.r

RF2 <- randomForest(as.formula(formula3), data= Train.r, importance = T, do.trace = 50,
                    nodesize = 35)
summary(RF2)

Train.r$RF2.Prob <- predict(RF2, newdata= Train.r, type = "prob")[,2] 
Test.r$RF2.Prob <- predict(RF2, newdata = Test.r, type = "prob")[,2]

as.numeric(as.character(Train.r$RF2.Prob)) -> Train.r$RF2.Prob
mutate(Train.r, "RF2.Prob" = ifelse(RF2.Prob < 0.01, 0.01, RF2.Prob) ) -> Train.r
mutate(Train.r, "RF2.Prob" = ifelse(RF2.Prob > 0.99, 0.99, RF2.Prob)) -> Train.r

as.numeric(as.character(Test.r$RF2.Prob)) -> Test.r$RF2.Prob
mutate(Test.r, "RF2.Prob" = ifelse(RF2.Prob < 0.01, 0.01, RF2.Prob) ) -> Test.r
mutate(Test.r, "RF2.Prob" = ifelse(RF2.Prob > 0.99, 0.99, RF2.Prob)) -> Test.r


ggplot(Train.r, aes(x=RF2.Prob, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Test.r,  aes(x=RF2.Prob, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Train.r, aes(x=RF2.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Train.r, aes(x=RF2.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Counts") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=RF2.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.05, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy of Predictions binned by Confidence") +
  xlab("Model Probability (A to win)") + ylab("Proportion of B (0) or A (1) actual wins") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Test.r, aes(x=RF2.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.05,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Distribution of Random ForestPredictions") +
  xlab("Confidence of A to win") + ylab("Counts of A (1) or B (0) wins")

print("All games:")
logloss(Train.r$A.wins, Train.r$RF2.Prob)
print("Tournament games only:")
logloss(Test.r$A.wins, Test.r$RF2.Prob)




