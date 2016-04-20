##Formula for Log5 winner prediction is:
# Pa - (Pb * Pa) / (Pa + (Pb - (2 * Pb * Pa))) = Probability that A wins, 
# where Ps is pythagorean rating from 0 to 1


#Write KenPom Log5 function
KP5 <- function(df) {  #where the first 3 cols are Year, A, B
  #write formula for individual match, then use apply within this function to generate vector of probabilities
  df[,c(1:3)] -> df
  unlist(apply(df, 1, function(x) {
    #pull data from Master data frame
    Master$Pyth[which(Master$Team_Id == x[2] & Master$Year == x[1])] -> Pa
    Master$Pyth[which(Master$Team_Id == x[3] & Master$Year == x[1])] -> Pb
    #Some Team_Id pulls are not bringing in values.  Thus the length of the produced vector is less than the 
    #input number.  This code provides dummy data to prevent length return error. 
    if(length(Pa) == 0 ) {
      Pa <- 0.4862 # mean Pyth for all observations
    }
    if(length(Pb) == 0) {
      Pb <- 0.4862 # see above
    }
    PAvB <- (Pa - (Pb * Pa)) / (Pa + (Pb - (2 * Pb * Pa)))
    as.vector(PAvB) -> PAvB
    return(PAvB)
  }))
}



#obtain Probabilities using L5 
KP5(Train) -> Train$KP5.Prob
KP5(Test) -> Test$KP5.Prob

#Graphical Analysis
##Density plots
ggplot(Train, aes(x=KP5.Prob, group=A.wins,col=factor(A.wins))) + geom_density()
ggplot(Test,  aes(x=KP5.Prob, group=A.wins,col=factor(A.wins))) + geom_density()

#Histograms
ggplot(Train, aes(x=KP5.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy binned by Log5 Prediction probability using KP Pythagorean ratings") +
  xlab("Confidence of A to win") + ylab("Proportion of A (1) or B (0) win") +
  geom_hline(yintercept= 0.5, lty =2 )
ggplot(Train, aes(x=KP5.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Distribution of Log5 Prediction probability using KP Pythagorean ratings") +
  xlab("Confidence of A to win") + ylab("Counts of A (1) or B (0) wins")

ggplot(Test, aes(x=KP5.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02, position="fill",col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Accuracy binned by Log5 Prediction probability using KP Pythagorean ratings on Tournament games only") +
  xlab("Confidence of A to win") + ylab("Proportion of A (1) or B (0) win")+
  geom_hline(yintercept= 0.5, lty =2 )

ggplot(Test, aes(x=KP5.Prob)) + 
  geom_histogram(aes(fill= factor(A.wins)), binwidth=0.02,col = "black") +
  scale_fill_manual(values = c("grey", "red")) + 
  ggtitle("Distribution of Log5 Prediction probability using KP Pythagorean ratings on Tournament games only") +
  xlab("Confidence of A to win") + ylab("Counts of A (1) or B (0) wins")


#Check LogLoss Score
logloss <- function(a, p){
  df <- data.frame("Actual" = a, 
                   "Probability" = p)
  unlist(apply(df, 1, function(x) {
    x[1] -> aw
    x[2] -> pr
    ll <- ((aw * log(pr) + ((1-aw)*log(1-pr))))
    as.vector(ll) -> ll
    return(ll)
  })) -> ll  #must return this to a vector to use the vector outside the apply, but inside the function.  
  LL <- (-1/length(a)) * sum(ll)
  print(paste("LogLoss:", round(LL, digits = 6)))
}


##LogLoss all games
print("All games:")
logloss(Train$A.wins, Train$KP5.Prob)
print("Tournament games only:")
logloss(Test$A.wins, Test$KP5.Prob)



