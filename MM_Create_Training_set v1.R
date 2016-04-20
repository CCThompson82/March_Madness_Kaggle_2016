##Mutate Results DFs to represent regular season or tournament match
Results.reg %>% mutate("Tournament" = 0) -> Results.reg
Results.tourn %>% mutate("Tournament" = 1) -> Results.tourn
bind_rows(Results.reg, Results.tourn) -> Results
#Transmutate into necessary information only
Results %>% transmute("Year" = Season,
                      "A" = ifelse(Wteam < Lteam, Wteam, Lteam),
                      "B" = ifelse(Wteam > Lteam, Wteam, Lteam),
                      "A.wins" = ifelse(Wteam < Lteam, 1, 0),
                      "PD" = ifelse(Wteam < Lteam, Wscore - Lscore, Lscore - Wscore),
                      "TP" = Wscore + Lscore,
                      "Wloc" = Wloc,
                      "Tournament" = Tournament) -> Results
Results -> pre_Results #Store this for future reference, as actual Results df will be appended with
#probabilities and predictions.  


#function to turn the observation list into a training data set, that has the ratio or differences
#of the average statistics for teams A and B
ratios <- function(df, rat_ret){
  names(df)[2] <- "Team_Id"
  left_join(df, Master, by = c("Year", "Team_Id")) -> Adf
  select(Adf, 9:dim(Adf)[2]) -> Adf
  names(Adf) <- paste("A",names(Adf), sep="_")
  names(df)[2] <- "A"
  names(df)[3] <- "Team_Id"
  left_join(df, Master, by = c("Year", "Team_Id")) -> Bdf
  names(df)[3] <- "Team_Id"  #not sure if this is necessary
  select(Bdf, 9:dim(Bdf)[2]) -> Bdf
  names(Bdf) <- paste("B",names(Bdf), sep="_")
  bind_cols(Results, bind_cols(Adf, Bdf))  -> Train
  Train %>% select(Year,
                   A,
                   B,
                   A.wins,
                   PD,
                   TP,
                   Wloc,
                   Tournament,
                   ends_with("_Team"),
                   ends_with("_Wins"),
                   ends_with("_Losses"),
                   ends_with("_Pyth"),
                   ends_with("_AdjustO"),
                   ends_with("_AdjustD"),
                   ends_with("_AdjustT"),
                   ends_with("_SOS.Pyth"),
                   ends_with("_SOS.OppO"),
                   ends_with("_SOS.OppD"),
                   ends_with("_NCSOS.Pyth"),
                   ends_with("_SRS"),
                   ends_with("_SOS"),
                   ends_with("_FGA"),
                   ends_with("_FG_pc"),
                   ends_with("_Trip_Att"),
                   ends_with("_Trip_pc"),
                   ends_with("_FT"),
                   ends_with("_FTA"),
                   ends_with("_FT_pc"),
                   ends_with("_ORB"),
                   ends_with("_TRB"),
                   ends_with("_AST"),
                   ends_with("_STL"),
                   ends_with("_BLK"),
                   ends_with("_TOV"),
                   ends_with("_PF"),
                   ends_with("_Pace"),
                   ends_with("_ORtg"),
                   ends_with("_FTr"),
                   ends_with("_Trip_PAr"),
                   ends_with("_TS_pc"),
                   ends_with("_TRB_pc"),
                   ends_with("_AST_pc"),
                   ends_with("_STL_pc"),
                   ends_with("_BLK_pc"),
                   ends_with("_eFG_pc"),
                   ends_with("_TOV_pc"),
                   ends_with("_ORB_pc"),
                   ends_with("_FT_by_FGA")) -> Train
  print(dim(Train))
  
  #some teams, probably in transition to D1 from D1AA, etc are missing.  Removed observations
  #from Train set:
  Train %>% filter(!(is.na(A_Team) | is.na(B_Team))) -> Train
  print(dim(Train))
  #no more NA's due to missing team data.  
  #Is there a way to replace the missing values in:
    #pace (32k)
    #Off_rating(32k)
    #TotReb (30)
    #steal_pc (32)
    #Block_pc (28)
    #Off_Reb_pc(32k)
  #Pace modeling
  Train %>% filter(!(is.na(A_Pace))) -> Train.pace
  Pace.model <- glm(A_Pace~A_AdjustT+A_AdjustO, data=Train.pace)
  cor(Train.pace$A_Pace,Pace.model$fitted.values) -> Pace_corr
  print(paste("GLM using A_AdjustT and A_AdjustO predicts A_Pace at r^2 of:", Pace_corr))
  #add values to A_pace that are missing
  if(Pace_corr > 0.9){
    print("Replacing NAs in A_Pace...")
    Train$A_Pace[is.na(Train$A_Pace)] <- 
      predict(Pace.model, newdata= Train[is.na(Train$A_Pace),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  
  #Pace_model_B
  Train %>% filter(!(is.na(B_Pace))) -> Train.pace
  Pace.model <- glm(B_Pace~B_AdjustT+B_AdjustO, data=Train.pace)
  cor(Train.pace$B_Pace,Pace.model$fitted.values) -> Pace_corr
  print(paste("GLM using B_AdjustT and B_AdjustO predicts B_Pace at r^2 of:", Pace_corr))
  #add values to A_pace that are missing
  if(Pace_corr > 0.9){
    print("Replacing NAs in B_Pace...")
    Train$B_Pace[is.na(Train$B_Pace)] <- 
      predict(Pace.model, newdata= Train[is.na(Train$B_Pace),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  ######ORB
  #A
  Train %>% filter(!(is.na(A_ORB))) -> Train.nona
  nona.model <- glm(A_ORB~A_TRB+A_AdjustO, data=Train.nona)
  cor(Train.nona$A_ORB,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using A_TRB and A_AdjustO predicts A_ORB at r^2 of:", stat_corr))
  #add values to A_ORB that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in A_ORB...")
    Train$A_ORB[is.na(Train$A_ORB)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_ORB),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  
  #ORB_model_B
  Train %>% filter(!(is.na(B_ORB))) -> Train.nona
  nona.model <- glm(B_ORB~B_TRB+B_AdjustO, data=Train.nona)
  cor(Train.nona$B_ORB,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using B_TRB and B_AdjustO predicts B_ORB at r^2 of:", stat_corr))
  #add values to B_ORB that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in B_ORB...")
    Train$B_ORB[is.na(Train$B_ORB)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_ORB),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  ####Off_Rating
  #A
  Train %>% filter(!(is.na(A_ORtg))) -> Train.nona
  nona.model <- glm(A_ORtg~A_AdjustO+A_TS_pc, data=Train.nona)
  cor(Train.nona$A_ORtg,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using A_AdjustO and A_TS_pc predicts A_ORtg at r^2 of:", stat_corr))
  #add values to A_ORtg that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in A_ORtg...")
    Train$A_ORtg[is.na(Train$A_ORtg)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_ORtg),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  #B
  Train %>% filter(!(is.na(B_ORtg))) -> Train.nona
  nona.model <- glm(B_ORtg~B_AdjustO+B_TS_pc, data=Train.nona)
  cor(Train.nona$B_ORtg,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using B_AdjustO and B_TS_pc predicts B_ORtg at r^2 of:", stat_corr))
  #add values to B_ORtg that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in B_ORtg...")
    Train$B_ORtg[is.na(Train$B_ORtg)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_ORtg),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  
  ####STL_pc
  #A
  Train %>% filter(!(is.na(A_STL_pc))) -> Train.nona
  nona.model <- glm(A_STL_pc~A_AdjustD+A_STL, data=Train.nona)
  cor(Train.nona$A_STL_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using A_AdjustD and A_STL predicts A_STL_pc at r^2 of:", stat_corr))
  #add values to A_STL_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in A_STL_pc...")
    Train$A_STL_pc[is.na(Train$A_STL_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_STL_pc),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  Train %>% filter(!(is.na(B_STL_pc))) -> Train.nona
  nona.model <- glm(B_STL_pc~B_AdjustD+B_STL, data=Train.nona)
  cor(Train.nona$B_STL_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using B_AdjustD and B_STL predicts B_STL_pc at r^2 of:", stat_corr))
  #add values to B_STL_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in B_STL_pc...")
    Train$B_STL_pc[is.na(Train$B_STL_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_STL_pc),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  ####ORB_pc
  #A
  Train %>% filter(!(is.na(A_ORB_pc))) -> Train.nona
  nona.model <- glm(A_ORB_pc~A_ORB+A_TRB, data=Train.nona)
  cor(Train.nona$A_ORB_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using A_ORB and A_TRB predicts A_ORB_pc at r^2 of:", stat_corr))
  #add values to A_ORB_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in A_ORB_pc...")
    Train$A_ORB_pc[is.na(Train$A_ORB_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_ORB_pc),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  #B
  Train %>% filter(!(is.na(B_ORB_pc))) -> Train.nona
  nona.model <- glm(B_ORB_pc~B_ORB+B_TRB, data=Train.nona)
  cor(Train.nona$B_ORB_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using B_ORB and B_TRB predicts B_ORB_pc at r^2 of:", stat_corr))
  #add values to B_ORB_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in B_ORB_pc...")
    Train$B_ORB_pc[is.na(Train$B_ORB_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_ORB_pc),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  ##TRB_pc
  #A
  Train %>% filter(!(is.na(A_TRB_pc))) -> Train.nona
  nona.model <- glm(A_TRB_pc~A_ORB+A_TRB, data=Train.nona)
  cor(Train.nona$A_TRB_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using A_ORB and A_TRB predicts A_TRB_pc at r^2 of:", stat_corr))
  #add values to A_TRB_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in A_TRB_pc...")
    Train$A_TRB_pc[is.na(Train$A_TRB_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_TRB_pc),])
    print("Done!")
  } else {
    print("Correlation too low, replaced anyway due to low number of observations <20")
    Train$A_TRB_pc[is.na(Train$A_TRB_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_TRB_pc),])
    print("Done!")
  }
  #B
  Train %>% filter(!(is.na(B_TRB_pc))) -> Train.nona
  nona.model <- glm(B_TRB_pc~B_ORB+B_TRB, data=Train.nona)
  cor(Train.nona$B_TRB_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using B_ORB and B_TRB predicts B_TRB_pc at r^2 of:", stat_corr))
  #add values to B_TRB_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in B_TRB_pc...")
    Train$B_TRB_pc[is.na(Train$B_TRB_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_TRB_pc),])
    print("Done!")
  } else {
    print("Correlation too low, replaced anyway due to low number of observations <20")
    Train$B_TRB_pc[is.na(Train$B_TRB_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_TRB_pc),])
    print("Done!")
  }
  #BLK_pc
  #A
  Train %>% filter(!(is.na(A_BLK_pc))) -> Train.nona
  nona.model <- glm(A_BLK_pc~A_AdjustD+A_BLK, data=Train.nona)
  cor(Train.nona$A_BLK_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using A_AdjustD and A_BLK predicts A_BLK_pc at r^2 of:", stat_corr))
  #add values to A_BLK_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in A_BLK_pc...")
    Train$A_BLK_pc[is.na(Train$A_BLK_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$A_BLK_pc),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  Train %>% filter(!(is.na(B_BLK_pc))) -> Train.nona
  nona.model <- glm(B_BLK_pc~B_AdjustD+B_BLK, data=Train.nona)
  cor(Train.nona$B_BLK_pc,nona.model$fitted.values) -> stat_corr
  print(paste("GLM using B_AdjustD and B_BLK predicts B_BLK_pc at r^2 of:", stat_corr))
  #add values to B_BLK_pc that are missing
  if(stat_corr > 0.8){
    print("Replacing NAs in B_BLK_pc...")
    Train$B_BLK_pc[is.na(Train$B_BLK_pc)] <- 
      predict(nona.model, newdata= Train[is.na(Train$B_BLK_pc),])
    print("Done!")
  } else {
    print("Correlation too low, revise model")
  }
  #Create statistic ratios for each game observations, if rat_ret == TRUE
  if(rat_ret == TRUE){
  Train.r <- Train  %>% transmute("Year" = Year,
                                     "A" = A,
                                     "B" = B,
                                     "A.wins" = A.wins,
                                     "PD" = PD,
                                     "TP" = TP,
                                     "Wloc" = Wloc,
                                     "Tournament" = Tournament,
                                     "Net_wins" = A_Wins - B_Wins,
                                     "Net_losses" = A_Losses - B_Losses,
                                     "Pyth.r" = A_Pyth / B_Pyth,
                                     "AdjustedO.r" = A_AdjustO / B_AdjustO,
                                     "AdjustedD.r" = A_AdjustD / B_AdjustD,
                                     "AdjustedT.r" = A_AdjustT / B_AdjustT,
                                     "SOS_by_Pyth.r" = A_SOS.Pyth / B_SOS.Pyth,
                                     "SOS_by_OppO.r" = A_SOS.OppO / B_SOS.OppO,
                                     "SOS_by_OppD.r" = A_SOS.OppD / B_SOS.OppD,
                                     "NC_SOS_by_Pyth.r" = A_NCSOS.Pyth / B_NCSOS.Pyth,
                                     "Net_SRS" = A_SRS - B_SRS,
                                     "SOS_SR.r" = A_SOS / B_SOS,
                                     "FGA.r" = A_FGA / B_FGA,
                                     "FGpc.r" = A_FG_pc / B_FG_pc,
                                     "Triple_Att.r" = A_Trip_Att / B_Trip_Att,
                                     "Triple_pc.r" = A_Trip_pc / B_Trip_pc,
                                     "FTA.r" = A_FTA / B_FTA,
                                     "FT.r" = A_FT / B_FT,
                                     "FT_pc.r" = A_FT_pc / B_FT_pc,
                                     "OffReb.r" = A_ORB / B_ORB,
                                     "TotReb.r" = A_TRB / B_TRB,
                                     "Assist.r" = A_AST / B_AST,
                                     "Steal.r" = A_STL / B_STL,
                                     "Blocks.r" = A_BLK / B_BLK,
                                     "TurnOvers.r" = A_TOV / B_TOV,
                                     "PF.r" = A_PF / B_PF,
                                     "Pace.r" = A_Pace / B_Pace,
                                     "Off_Rating.r" = A_ORtg / B_ORtg,
                                     "FTA_per_FGA.r" = A_FTr / B_FTr,
                                     "FGA.pc_from_three.r" = A_Trip_PAr / B_Trip_PAr,
                                     "True_shooting_pc.r" = A_TS_pc / B_TS_pc,
                                     "TotReb_pc.r" = A_TRB_pc / B_TRB_pc,
                                     "Assist_pc.r" = A_AST_pc / B_AST_pc,
                                     "Steal_pc.r" = A_STL_pc / B_STL_pc,
                                     "Block_pc.r" = A_BLK_pc / B_BLK_pc,
                                     "eff_FG_pc.r" = A_eFG_pc / B_eFG_pc,
                                     "TurnOver_pc.r" = A_TOV_pc / B_TOV_pc,
                                     "OffReb_pc.r" = A_ORB_pc / B_ORB_pc,
                                     "FT_per_FGA.r" = A_FT_by_FGA / B_FT_by_FGA)
  return(Train.r)
  }else{
    return(Train)
  }
}
ratios(Results, FALSE) -> Train
ratios(Results, TRUE) -> Train.r


