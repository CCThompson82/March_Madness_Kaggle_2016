#Connect to SQLite file, structure necessary data into dataframes
con <- dbConnect(SQLite(), dbname = "march-machine-learning-mania-2016-v1/database.sqlite")
dbListTables(con)
teams <- dbReadTable(con, "Teams")
seeds <- dbReadTable(con, "TourneySeeds")
Results.tourn <- dbReadTable(con, "TourneyDetailedResults")
#Results.reg <- dbReadTable(con, "RegularSeasonDetailedResults")
Results.reg <- read.csv("/Users/ccthomps/Documents/R Files/Kaggle comps/MarchMad/Prelim_RegularSeasonDetailedResults_thru_Day132.csv", header=T)
SS <- read.csv("/Users/ccthomps/Documents/R Files/Kaggle comps/MarchMad/Submission5050(1).csv", header=T)
dbDisconnect(con)
#Generate a 'Master' list of every team that played a game from 2012-2016.
#List to be populated with relavent yearly statistics
Master <- data.frame("Team_Id" = numeric(0),
                     "Year" = numeric(0), stringsAsFactors = FALSE)
for(y in 2003:2016){  ##the 2016 data is not out yet.  This should be incorporated upon Kaggle release
  filter(Results.reg, Season == y) %>% select(Season, Wteam, Lteam) -> temp.df 
  for(t in 1011:1463){
    vec <- data.frame("Team_Id" = numeric(0),
                      "Year" = numeric(0), stringsAsFactors = FALSE)
    if(t %in% temp.df$Wteam | t %in% temp.df$Lteam) {
      vec[1,1] <- t
      vec[1,2] <- y
      rbind(Master, vec) -> Master
    } 
  }
}
# each line is a team by year
left_join(Master, teams, by= "Team_Id") -> Master

## KenPom Data download.  Perform scrape in Python using communal code immediately before running this script.  
#CSV file stored locally.
KenPomScrape <- 
  read.csv("/Users/ccthomps/Documents/R Files/Kaggle comps/MarchMad/KenPom.csv", 
           strip.white = TRUE)
#Prepare Data sets for merging
names(Master)[3] <- "Team"
##Remove all the "." from KenPomScrape
as.character(KenPomScrape$Team) -> KenPomScrape$Team
gsub("[.]", "", KenPomScrape$Team) -> KenPomScrape$Team
#weird case in which college changed their name.  Changed old versions 
gsub("Texas Pan American","UT Rio Grande Valley", KenPomScrape$Team) -> KenPomScrape$Team
# changed Master names to match KenPom data
gsub("Albany NY", "Albany", Master$Team) -> Master$Team
gsub("American Univ", "American", Master$Team) -> Master$Team
gsub("Ark Little Rock", "Arkansas Little Rock", Master$Team) -> Master$Team
gsub("Ark Pine Bluff", "Arkansas Pine Bluff", Master$Team) -> Master$Team
gsub("Bethune-Cookman", "Bethune Cookman", Master$Team) -> Master$Team
gsub("Boston Univ", "Boston University", Master$Team) -> Master$Team
gsub("C Michigan", "Central Michigan", Master$Team) -> Master$Team
gsub("Cal Poly SLO", "Cal Poly", Master$Team) -> Master$Team
gsub("Cent Arkansas", "Central Arkansas", Master$Team) -> Master$Team
gsub("Central Conn", "Central Connecticut", Master$Team) -> Master$Team
gsub("Charleston So", "Charleston Southern", Master$Team) -> Master$Team
gsub("Citadel", "The Citadel", Master$Team) -> Master$Team
gsub("Coastal Car", "Coastal Carolina", Master$Team) -> Master$Team
gsub("Col Charleston", "College of Charleston", Master$Team) -> Master$Team
gsub("CS Bakersfield", "Cal St Bakersfield", Master$Team) -> Master$Team
gsub("CS Fullerton", "Cal St Fullerton", Master$Team) -> Master$Team
gsub("CS Northridge", "Cal St Northridge", Master$Team) -> Master$Team
gsub("CS Sacramento", "Sacramento St", Master$Team) -> Master$Team
gsub("E Illinois", "Eastern Illinois", Master$Team) -> Master$Team
gsub("E Kentucky", "Eastern Kentucky", Master$Team) -> Master$Team
gsub("E Michigan", "Eastern Michigan", Master$Team) -> Master$Team
gsub("E Washington", "Eastern Washington", Master$Team) -> Master$Team
gsub("Edwardsville", "SIU Edwardsville", Master$Team) -> Master$Team
gsub("ETSU", "East Tennessee St", Master$Team) -> Master$Team
gsub("F Dickinson", "Fairleigh Dickinson", Master$Team) -> Master$Team
gsub("FL Atlantic", "Florida Atlantic", Master$Team) -> Master$Team
gsub("FL Gulf Coast", "Florida Gulf Coast", Master$Team) -> Master$Team
gsub("Florida Intl", "FIU", Master$Team) -> Master$Team
gsub("G Washington", "George Washington", Master$Team) -> Master$Team
gsub("Ga Southern", "Georgia Southern", Master$Team) -> Master$Team
gsub("Grambling", "Grambling St", Master$Team) -> Master$Team
gsub("Houston Bap", "Houston Baptist", Master$Team) -> Master$Team
gsub("IL Chicago", "Illinois Chicago", Master$Team) -> Master$Team
gsub("Kennesaw", "Kennesaw St", Master$Team) -> Master$Team
gsub("\\Kent\\b", "Kent St", Master$Team) -> Master$Team
gsub("Long Island", "LIU Brooklyn", Master$Team) -> Master$Team
gsub("Loy Marymount", "Loyola Marymount", Master$Team) -> Master$Team
gsub("Loyola-Chicago", "Loyola Chicago", Master$Team) -> Master$Team
gsub("MD E Shore", "Maryland Eastern Shore", Master$Team) -> Master$Team
gsub("Missouri KC", "UMKC", Master$Team) -> Master$Team
gsub("Monmouth NJ", "Monmouth", Master$Team) -> Master$Team
gsub("MS Valley St", "Mississippi Valley St", Master$Team) -> Master$Team
gsub("Mt St Mary's", "Mount St Mary's", Master$Team) -> Master$Team
gsub("MTSU", "Middle Tennessee", Master$Team) -> Master$Team
gsub("N Colorado", "Northern Colorado", Master$Team) -> Master$Team
gsub("N Dakota St", "North Dakota St", Master$Team) -> Master$Team
gsub("N Illinois", "Northern Illinois", Master$Team) -> Master$Team
gsub("NC A&T", "North Carolina A&T", Master$Team) -> Master$Team
gsub("NC Central", "North Carolina Central", Master$Team) -> Master$Team
gsub("NC State", "North Carolina St", Master$Team) -> Master$Team
gsub("NE Omaha", "Nebraska Omaha", Master$Team) -> Master$Team
gsub("Northwestern LA", "Northwestern St", Master$Team) -> Master$Team
gsub("Prairie View", "Prairie View A&M", Master$Team) -> Master$Team
gsub("S Carolina St", "South Carolina St", Master$Team) -> Master$Team
gsub("S Dakota St", "South Dakota St", Master$Team) -> Master$Team
gsub("S Illinois", "Southern Illinois", Master$Team) -> Master$Team
gsub("Santa Barbara", "UC Santa Barbara", Master$Team) -> Master$Team
gsub("SC Upstate", "USC Upstate", Master$Team) -> Master$Team
gsub("SE Louisiana", "Southeastern Louisiana", Master$Team) -> Master$Team
gsub("SE Missouri St", "Southeast Missouri St", Master$Team) -> Master$Team
gsub("SF Austin", "Stephen F Austin", Master$Team) -> Master$Team
gsub("S Carolina St", "South Carolina St", Master$Team) -> Master$Team
gsub("Southern Univ", "Southern", Master$Team) -> Master$Team
gsub("St Joseph's PA", "Saint Joseph's", Master$Team) -> Master$Team
gsub("St Louis", "Saint Louis", Master$Team) -> Master$Team
gsub("St Mary's CA", "Saint Mary's", Master$Team) -> Master$Team
gsub("St Peter's", "Saint Peter's", Master$Team) -> Master$Team
gsub("TAM C. Christi", "Texas A&M Corpus Chris", Master$Team) -> Master$Team
gsub("TN Martin", "Tennessee Martin", Master$Team) -> Master$Team
gsub("TX Southern", "Texas Southern", Master$Team) -> Master$Team
gsub("ULL", "Louisiana Lafayette", Master$Team) -> Master$Team
gsub("ULM", "Louisiana Monroe", Master$Team) -> Master$Team
gsub("UT San Antonio", "UTSA", Master$Team) -> Master$Team
gsub("VA Commonwealth", "VCU", Master$Team) -> Master$Team
gsub("W Carolina", "Western Carolina", Master$Team) -> Master$Team
gsub("W Illinois", "Western Illinois", Master$Team) -> Master$Team
gsub("WKU", "Western Kentucky", Master$Team) -> Master$Team
gsub("W Michigan", "Western Michigan", Master$Team) -> Master$Team
gsub("WI Green Bay", "Green Bay", Master$Team) -> Master$Team
gsub("WI Milwaukee", "Milwaukee", Master$Team) -> Master$Team
gsub("N Kentucky", "Northern Kentucky", Master$Team) -> Master$Team
gsub("Abilene Chr", "Abilene Christian", Master$Team) -> Master$Team
gsub("MA Lowell", "UMass Lowell", Master$Team) -> Master$Team
gsub("UTRGV", "UT Rio Grande Valley", Master$Team) -> Master$Team
#attach KenPom Stats to each time by year
left_join(Master, KenPomScrape, by = c("Year", "Team")) -> Master


#BPI data
#scrape BPI data from ESPN using R code
url_list <- c("http://espn.go.com/mens-college-basketball/bpi",
              "http://espn.go.com/mens-college-basketball/bpi/_/season/2015",
              "http://espn.go.com/mens-college-basketball/bpi/_/season/2014",
              "http://espn.go.com/mens-college-basketball/bpi/_/season/2013",
              "http://espn.go.com/mens-college-basketball/bpi/_/season/2012")
year_vec <- c(2016, 2015, 2014, 2013, 2012)
ESPN <- list(NULL)
for(u in 1:length(url_list)){
  readHTMLTable(url_list[u], header = T, stringsAsFactors=F) -> EL
  EL[[1]] -> df
  df[1,] -> names(df) #retitle as header = True did not work
  gsub(" ", ".", names(df)) -> names(df) # get rid of names with spaces
  df %>% filter(!(TEAM == "TEAM")) -> df #gets rid of header breaks
  mutate(df, "Year" = year_vec[u]) -> df
  df -> ESPN[[u]]
}
BPI_df <- bind_rows(bind_rows(bind_rows(bind_rows(ESPN[[1]],
                                                  ESPN[[2]]), 
                                        ESPN[[3]]), 
                              ESPN[[4]]), 
                    ESPN[[5]])

#preparation work to merge BPI into Master data set that already includes KenPom
names(BPI_df)[2] <- "Team"
#
gsub("State", "St", BPI_df$Team) -> BPI_df$Team
gsub("\\UNC\\b", "North Carolina", BPI_df$Team, fixed=F) -> BPI_df$Team
gsub("North Carolina Asheville", "UNC Asheville", BPI_df$Team, fixed =TRUE) -> BPI_df$Team
gsub("North Carolina Wilmington", "Wilmington", BPI_df$Team, fixed = TRUE) -> BPI_df$Team
gsub("Omaha", "Nebraska Omaha", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UNCG", "UNC Greensboro", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("North Carolina Greensboro", "UNC Greensboro",BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Abil Christian", "Abilene Christian", BPI_df$Team) -> BPI_df$Team
gsub("Alabama State", "Alabama St", BPI_df$Team) -> BPI_df$Team
gsub("American U", "American", BPI_df$Team) -> BPI_df$Team
gsub("AR-", "Arkansas ", BPI_df$Team) -> BPI_df$Team
gsub("Bethune-Cookman", "Bethune Cookman", BPI_df$Team) -> BPI_df$Team
gsub("Boston U", "Boston University", BPI_df$Team) -> BPI_df$Team
gsub("Cent Arkansas", "Central Arkansas", BPI_df$Team) -> BPI_df$Team
gsub("Cent Conn St", "Central Connecticut", BPI_df$Team) -> BPI_df$Team
gsub("Cent Michigan", "Central Michigan", BPI_df$Team) -> BPI_df$Team
gsub("\\Charleston\\b", "College of Charleston", BPI_df$Team) -> BPI_df$Team
gsub("\\Charleston So\\b", "Charleston Southern", BPI_df$Team) -> BPI_df$Team
gsub("\\College of Charleston Southern\\b", "Charleston Southern", BPI_df$Team) -> BPI_df$Team
gsub("Coast Carolina", "Coastal Carolina", BPI_df$Team) -> BPI_df$Team
gsub("CS Fullerton", "Cal St Fullerton", BPI_df$Team) -> BPI_df$Team
gsub("CS Northridge", "Cal St Northridge", BPI_df$Team) -> BPI_df$Team
gsub("CSU Bakersfield", "Cal St Bakersfield", BPI_df$Team) -> BPI_df$Team
gsub("E Illinois", "Eastern Illinois", BPI_df$Team) -> BPI_df$Team
gsub("E Kentucky", "Eastern Kentucky", BPI_df$Team) -> BPI_df$Team
gsub("E Michigan", "Eastern Michigan", BPI_df$Team) -> BPI_df$Team
gsub("E Washington", "Eastern Washington", BPI_df$Team) -> BPI_df$Team
gsub("ECU", "East Carolina", BPI_df$Team) -> BPI_df$Team
gsub("ETSU", "East Tennessee St", BPI_df$Team) -> BPI_df$Team
gsub("Fair Dickinson", "Fairleigh Dickinson", BPI_df$Team) -> BPI_df$Team
gsub("FAU", "Florida Atlantic", BPI_df$Team) -> BPI_df$Team
gsub("FGCU", "Florida Gulf Coast", BPI_df$Team) -> BPI_df$Team
gsub("Cal", "California", BPI_df$Team) -> BPI_df$Team
gsub("FSU", "Florida St", BPI_df$Team) -> BPI_df$Team
gsub("G Washington", "George Washington", BPI_df$Team) -> BPI_df$Team
gsub("Ga Southern", "Georgia Southern", BPI_df$Team) -> BPI_df$Team
gsub("Gardner-Webb", "Gardner Webb", BPI_df$Team) -> BPI_df$Team
gsub("JMU", "James Madison", BPI_df$Team) -> BPI_df$Team
gsub("LA Tech", "Louisiana Tech", BPI_df$Team) -> BPI_df$Team
gsub("LA-Lafayette", "Louisiana Lafayette", BPI_df$Team) -> BPI_df$Team
gsub("LBSU", "Long Beach St", BPI_df$Team) -> BPI_df$Team
gsub("Loyola (CHI)", "Loyola Chicago", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("California Poly", "Cal Poly", BPI_df$Team) -> BPI_df$Team
gsub("California St Bakersfield", "Cal St Bakersfield", BPI_df$Team) -> BPI_df$Team
gsub("California St Fullerton", "Cal St Fullerton", BPI_df$Team) -> BPI_df$Team
gsub("California St Northridge", "Cal St Northridge", BPI_df$Team) -> BPI_df$Team
gsub("Loyola (MD)", "Loyola MD", BPI_df$Team, fixed = TRUE) -> BPI_df$Team
gsub("\\Loyola Mary\\b", "Loyola Marymount", BPI_df$Team) -> BPI_df$Team
gsub("MD-E Shore", "Maryland Eastern Shore", BPI_df$Team) -> BPI_df$Team
gsub("Miami (FL)", "Miami FL", BPI_df$Team, fixed = TRUE) -> BPI_df$Team
gsub("Miami (OH)", "Miami OH", BPI_df$Team, fixed = TRUE) -> BPI_df$Team
gsub("Mid Tennessee", "Middle Tennessee", BPI_df$Team) -> BPI_df$Team
gsub("Miss St", "Mississippi St", BPI_df$Team) -> BPI_df$Team
gsub("Miss Valley St", "Mississippi Valley St", BPI_df$Team) -> BPI_df$Team
gsub("Mt St Mary's", "Mount St Mary's", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("N Arizona", "Northern Arizona", BPI_df$Team) -> BPI_df$Team
gsub("N Colorado", "Northern Colorado", BPI_df$Team) -> BPI_df$Team
gsub("N Illinois", "Northern Illinois", BPI_df$Team) -> BPI_df$Team
gsub("N Kentucky", "Northern Kentucky", BPI_df$Team) -> BPI_df$Team
gsub("NC A&T", "North Carolina A&T", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("NC Central", "North Carolina Central", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("NC St", "North Carolina St", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Ole Miss", "Mississippi", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("OSU", "Ohio St", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Pitt", "Pittsburgh", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("PV A&M", "Prairie View A&M", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("S Carolina St", "South Carolina St", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("S Illinois", "Southern Illinois", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Saint Joe's", "Saint Joseph's", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Sam Houston", "Sam Houston St", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("SE Louisiana", "Southeastern Louisiana", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("SE Missouri St", "Southeast Missouri St", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("SF Austin", "Stephen F Austin", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("SIU ED", "SIU Edwardsville", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("St Francis (BKN)", "St Francis NY", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("St Francis (PA)", "St Francis PA", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("St Peter's", "Saint Peter's", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Tenn Tech", "Tennessee Tech", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Texas A&M-CC", "Texas A&M Corpus Chris", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UConn", "Connecticut", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UCSB", "UC Santa Barbara", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UIC", "Illinois Chicago", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UL Monroe", "Louisiana Monroe", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UMass", "Massachusetts", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UNH", "New Hampshire", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("URI", "Rhode Island", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("USF", "South Florida", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UT Martin", "Tennessee Martin", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UT Rio Grande", "UT Rio Grande Valley", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UT-Arlington", "UT Arlington", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("UVA", "Virginia", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("W Carolina", "Western Carolina", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("W Illinois", "Western Illinois", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("W Kentucky", "Western Kentucky", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("W Michigan", "Western Michigan", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Massachusetts Lowell", "UMass Lowell", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("Wilmington", "UNC Wilmington", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("North Carolina Asheville", "UNC Asheville", BPI_df$Team, fixed=T) -> BPI_df$Team
gsub("North Carolina Greensboro", "UNC Greensboro", BPI_df$Team, fixed=T) -> BPI_df$Team


#For the time being, am not going to merge teh BPI because it only goes back to 2012.

##Merge datasets
#left_join(Master, BPI_df, by = c("Year","Team")) -> Master
#if( dim(Master[is.na(Master$RAW),])[1] == 0 ){
#  print("All teams have BPI Data")
#} else {
#  print("Check this again")
#  print(Master[is.na(Master$RAW),1:5])
#}
### Nebraska Omaha 2012 is not in the BPI_df.  Might cause issues later when using BPI as Pyth rating for training.
#As they have not made the tourney ever, it should not affect the submission output, as its record will never be
#queried.  

#Check if any teams have not merged data sets
#unique(Master$Team[is.na(Master$CONF)])
#anti_join(BPI_df, Master, by = c("Year", "Team")) %>% 
#  arrange(Team) %>% filter((Year != 2016)) %>% select(Team) %>% unique() %>% print()
##Youngstown St is in the BPI_data, but not the Master team list, it seems.  Therefore YS has no games recorded
#in the kaggle DB and thus are excluded from Master list.  If they make the tourney, revisit this asap.

year_vec <- c(2016, 2015, 2014, 2013, 2012, 2011, 2010, 2009, 2008, 2007, 2006, 2005, 2004, 2003)
url_list2 <- c("http://www.sports-reference.com/cbb/pi/shareit/UCoP0", #2016
               "http://www.sports-reference.com/cbb/pi/shareit/rR40X",  #2015
               "http://www.sports-reference.com/cbb/pi/shareit/CCZ4C",   #2014
               "http://www.sports-reference.com/cbb/pi/shareit/uPLVA",   #2013
               "http://www.sports-reference.com/cbb/pi/shareit/Srg50",  #2012
               "http://www.sports-reference.com/cbb/pi/shareit/qgaDq",   #2010-2011
               "http://www.sports-reference.com/cbb/pi/shareit/POLk9", #2009-2012
               "http://www.sports-reference.com/cbb/pi/shareit/FWmYF",  #2008-2009 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/LqDXH",  #2007-2008 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/yokcw",  #2006-2007 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/fAi2s",  #2005-2006 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/8fLJV",  #2004-2005 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/XXRnY", #2003-2004 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/rQqke") #2002 -2003 ; some data missing

url_list3 <- c("http://www.sports-reference.com/cbb/pi/shareit/bWp43",
               "http://www.sports-reference.com/cbb/pi/shareit/Tp4Jb",
               "http://www.sports-reference.com/cbb/pi/shareit/Joluw",
               "http://www.sports-reference.com/cbb/pi/shareit/C3sM8",
               "http://www.sports-reference.com/cbb/pi/shareit/p3a7V",
               "http://www.sports-reference.com/cbb/pi/shareit/NeOl8",  #2010-2011
               "http://www.sports-reference.com/cbb/pi/shareit/kleCQ",  #2009-2010
               "http://www.sports-reference.com/cbb/pi/shareit/9x1CR",  #2008-2009 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/9KHsd",  #2007-2008 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/JdyGq",  #2006-2007; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/rx27H",  #2005-2006 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/VGIHA",  #2004-2005 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/ksh5Z",  #2003-2004 ; some data missing
               "http://www.sports-reference.com/cbb/pi/shareit/3iw9P")  #2002-2003 ; some data missing

rm(df)
SR <- list(NULL)
for(u in 1:length(url_list2)){
  readHTMLTable(url_list2[u], header = T, stringsAsFactors=F) -> df
  df$basic_school_stats -> df
  mutate(df, "Year" = year_vec[u]) -> df
  df -> SR[[u]]
}

SR_basic_stats <- bind_rows(SR[[1]], SR[[2]]) %>% 
  bind_rows(SR[[3]]) %>% 
  bind_rows(SR[[4]]) %>% 
  bind_rows(SR[[5]]) %>% 
  bind_rows(SR[[6]]) %>% 
  bind_rows(SR[[7]]) %>% 
  bind_rows(SR[[8]]) %>% 
  bind_rows(SR[[9]]) %>% 
  bind_rows(SR[[10]]) %>% 
  bind_rows(SR[[11]]) %>% 
  bind_rows(SR[[12]]) %>% 
  bind_rows(SR[[13]]) %>% 
  bind_rows(SR[[14]]) 
  


rm(df)
SR <- list(NULL)
for(u in 1:length(url_list3)){
  readHTMLTable(url_list3[u], header = T, stringsAsFactors=F) -> df
  df$adv_school_stats -> df
  mutate(df, "Year" = year_vec[u]) -> df
  df -> SR[[u]]
}
##Forgot to remove Rank from 2012 data
SR[[5]] %>% select(-(Rk)) -> SR[[5]]
#bind rows together into df
SR_adv_stats <- bind_rows(SR[[1]], SR[[2]]) %>% 
  bind_rows(SR[[3]]) %>% 
  bind_rows(SR[[4]]) %>% 
  bind_rows(SR[[5]]) %>% 
  bind_rows(SR[[6]]) %>% 
  bind_rows(SR[[7]]) %>% 
  bind_rows(SR[[8]]) %>% 
  bind_rows(SR[[9]]) %>% 
  bind_rows(SR[[10]]) %>% 
  bind_rows(SR[[11]]) %>% 
  bind_rows(SR[[12]]) %>% 
  bind_rows(SR[[13]]) %>% 
  bind_rows(SR[[14]]) 


##Merge basic with advanced stats
SR_df <- inner_join(SR_basic_stats, SR_adv_stats, by = c("School", "Year"))
#Clean up
SR_df %>% select(21, 1:20, 22:34) -> SR_df
names(SR_df)
gsub("[%]", "_pc", names(SR_df)) -> names(SR_df)
gsub("[/]", "_by_", names(SR_df)) -> names(SR_df)
names(SR_df)[2] <- "Team"
names(SR_df)

##Modify SR_df for a clean merge with Master
Mas1 <- Master

##Merge Master with Sports_ref data
Mas2 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas2[is.na(Mas2$FT_pc),'Team'])

##Make changes to SR_df and re-run merge and check with new subscripts
gsub("State", "St", SR_df$Team) -> SR_df$Team
gsub("[-]", " ", SR_df$Team) -> SR_df$Team
gsub("[(]", "", SR_df$Team) -> SR_df$Team
gsub("[)]", "", SR_df$Team) -> SR_df$Team
## Have already changed SR_df to remove (), State -> St, so use caution with what SR_df actual is in the environment.

gsub("Albany (NY)", "Albany", SR_df$Team) -> SR_df$Team
##Merge Master with Sports_ref data
Mas3 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas3[is.na(Mas3$FT_pc),'Team'])

#
##Make changes to SR_df and re-run merge and check with new subscripts
gsub("Bowling Green St", "Bowling Green", SR_df$Team) -> SR_df$Team
gsub("Brigham Young", "BYU", SR_df$Team) -> SR_df$Team
gsub("University of California", "California", SR_df$Team) -> SR_df$Team
gsub("Central Connecticut St", "Central Connecticut", SR_df$Team) -> SR_df$Team
gsub("Citadel", "The Citadel", SR_df$Team) -> SR_df$Team
gsub("Detroit Mercy", "Detroit", SR_df$Team) -> SR_df$Team
gsub("Florida International", "FIU", SR_df$Team) -> SR_df$Team
gsub("Grambling", "Grambling St", SR_df$Team) -> SR_df$Team
gsub("Long Island University", "LIU Brooklyn", SR_df$Team) -> SR_df$Team
gsub("Loyola IL", "Loyola Chicago", SR_df$Team) -> SR_df$Team 
gsub("Louisiana St", "LSU", SR_df$Team) -> SR_df$Team
gsub("Missouri Kansas City", "UMKC", SR_df$Team) -> SR_df$Team
##Merge Master with Sports_ref data
Mas4 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas4[is.na(Mas4$FT_pc),'Team'])


##Make changes to SR_df and re-run merge and check with new subscripts

#gsub("Nebraska Omaha", "Nebraska Omaha", SR_df$Team) -> SR_df$Team
gsub("Pennsylvania", "Penn", SR_df$Team) -> SR_df$Team
gsub("Prairie View", "Prairie View A&M", SR_df$Team) -> SR_df$Team
gsub("South Carolina Upstate", "USC Upstate", SR_df$Team) -> SR_df$Team
gsub("Stephen F. Austin", "Stephen F Austin", SR_df$Team) -> SR_df$Team
gsub("Southern Methodist", "SMU", SR_df$Team) -> SR_df$Team
gsub("Southern Mississippi", "Southern Miss", SR_df$Team) -> SR_df$Team
gsub("[.]", "", SR_df$Team) -> SR_df$Team
##Merge Master with Sports_ref data
Mas5 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas5[is.na(Mas5$FT_pc),'Team'])


##Make changes to SR_df and re-run merge and check with new subscripts
gsub("St Francis NY", "St Francis NY", SR_df$Team) -> SR_df$Team 
gsub("Saint Francis PA", "St Francis PA", SR_df$Team) -> SR_df$Team
gsub("St John's NY", "St John's", SR_df$Team) -> SR_df$Team
gsub("Texas A&M Corpus Christi", "Texas A&M Corpus Chris", SR_df$Team) -> SR_df$Team
##Merge Master with Sports_ref data
Mas6 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas6[is.na(Mas6$FT_pc),'Team'])

##Make changes to SR_df and re-run merge and check with new subscripts
gsub("Texas Christian", "TCU", SR_df$Team) -> SR_df$Team 
gsub("Texas Rio Grande Valley", "UT Rio Grande Valley", SR_df$Team) -> SR_df$Team
gsub("Alabama Birmingham", "UAB", SR_df$Team) -> SR_df$Team
gsub("Central Florida", "UCF", SR_df$Team) -> SR_df$Team
##Merge Master with Sports_ref data
Mas7 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas7[is.na(Mas7$FT_pc),'Team'])

##Make changes to SR_df and re-run merge and check with new subscripts
gsub("Maryland Baltimore County", "UMBC", SR_df$Team) -> SR_df$Team
gsub("North Carolina Asheville", "UNC Asheville", SR_df$Team) -> SR_df$Team
gsub("North Carolina Greensboro", "UNC Greensboro", SR_df$Team) -> SR_df$Team
gsub("North Carolina Wilmington", "UNC Wilmington", SR_df$Team) -> SR_df$Team
gsub("Nevada Las Vegas", "UNLV", SR_df$Team) -> SR_df$Team
gsub("Southern California", "USC", SR_df$Team) -> SR_df$Team
gsub("Texas Arlington", "UT Arlington", SR_df$Team) -> SR_df$Team
gsub("Texas San Antonio", "UTSA", SR_df$Team) -> SR_df$Team
gsub("Texas El Paso", "UTEP", SR_df$Team) -> SR_df$Team
gsub("Virginia Commonwealth", "VCU", SR_df$Team) -> SR_df$Team
gsub("Massachusetts Lowell", "UMass Lowell", SR_df$Team) -> SR_df$Team
##Merge Master with Sports_ref data
Mas8 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas8[is.na(Mas8$FT_pc),'Team'])

gsub("Saint Mary's CA", "Saint Mary's", SR_df$Team, fixed = TRUE) -> SR_df$Team
gsub("\\Mount St. Mary's\\b", "Mount St Mary's", SR_df$Team, fixed = TRUE) -> SR_df$Team

##Merge Master with Sports_ref data
Mas9 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas9[is.na(Mas9$FT_pc),'Team'])
names(Mas9)
Mas9[is.na(Mas9$FT_pc), 2:3]


##Make changes to SR_df and re-run merge and check with new subscripts
gsub("Birmingham Southern", "Birmingham So", SR_df$Team) -> SR_df$Team
gsub("Centenary LA", "Centenary", SR_df$Team) -> SR_df$Team
#Lipscomb 2003 did not exist, will be purged from Mas1:
Mas1 %>% filter(!(Year == 2003 & Team == "Lipscomb")) -> Mas1
#Longwood, Northern Colorado, UC Davis did not exist before 2007, Mas1 will be purged:
Mas1 %>% filter(!(Year <= 2007 & Team == "Longwood")) -> Mas1
Mas1 %>% filter(!(Year <= 2007 & Team == "Northern Colorado")) -> Mas1
Mas1 %>% filter(!(Year <= 2007 & Team == "UC Davis")) -> Mas1
#Utah Valley Kennesaw St, North Florida, NJIT did not exist before 2009, Mas1 will be purged:
Mas1 %>% filter(!(Year <= 2009 & Team == "Utah Valley")) -> Mas1
Mas1 %>% filter(!(Year <= 2009 & Team == "Kennesaw St")) -> Mas1
Mas1 %>% filter(!(Year <= 2009 & Team == "North Florida")) -> Mas1
Mas1 %>% filter(!(Year <= 2009 & Team == "NJIT")) -> Mas1

#North Dakota St ; 2008
Mas1 %>% filter(!(Year <= 2008 & Team == "North Dakota St")) -> Mas1
Mas1 %>% filter(!(Year <= 2008 & Team == "South Dakota St")) -> Mas1
#Central Arkansas, Cal St Bakersfield, Florida Gulf Coast, Houston Baptist, North Carolina Central,
#Presbyterian, USC Upstate, Bryant, SIU Edwardsville, North Dakota, Seattle, South Dakota; 2010
Mas1 %>% filter(!(Year <= 2010 & Team == "Central Arkansas")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "Cal St Bakersfield")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "Florida Gulf Coast")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "Houston Baptist")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "North Carolina Central")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "Presbyterian")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "USC Upstate")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "Bryant")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "SIU Edwardsville")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "North Dakota")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "Seattle")) -> Mas1
Mas1 %>% filter(!(Year <= 2010 & Team == "South Dakota")) -> Mas1
##No reference to Winston Salem State in SR_df, will remove from Mas1
Mas1 %>% filter(!(Team == "W Salem St")) -> Mas1
#Nebraska Omaha; 2012
Mas1 %>% filter(!(Year == 2012 & Team == "Nebraska Omaha")) -> Mas1


##Merge Master with Sports_ref data
Mas10 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas10[is.na(Mas10$FT_pc),'Team'])
Mas10[is.na(Mas10$FT_pc), 2:3]
Mas1 %>% filter(!(Year == 2003 & Team == "Birmingham So")) -> Mas1


##Merge Master with Sports_ref data
Mas11 <- left_join(Mas1, SR_df, by=c("Year","Team"))
##List teams that did not merge because names were off in Sports Ref
unique(Mas11[is.na(Mas11$FT_pc),'Team'])



Mas11 -> Master
#Clean up
rm(BPI_df)
rm(df)
rm(EL)
rm(ESPN)
rm(KenPomScrape)
rm(t)
rm(teams)
rm(temp.df)
rm(u)
rm(url_list)
rm(vec)
rm(y)
rm(year_vec)
rm(Mas1)
rm(Mas2)
rm(Mas3)
rm(Mas4)
rm(Mas5)
rm(Mas6)
rm(Mas7)
rm(Mas8)
rm(Mas9)
rm(Mas10)
rm(Mas11)
rm(url_list2)
rm(url_list3)
rm(SR_basic_stats)
rm(SR_adv_stats)
rm(SR)

##Clean up Master
names(Master)
Master %>% select( -(grep("conf", names(Master), ignore.case = TRUE))) -> Master
Master %>% select( -(grep("Rank", names(Master), fixed = TRUE))) -> Master
Master %>% select( -(16:18)) -> Master  # Get rid of redundant G W L
names(Master)
names(Master)[20:21] <- c("Trip_Att", "Trip_pc")
names(Master)[35] <- "Trip_PAr"
names(Master)
apply(Master[,4:dim(Master)[2]], 2, as.numeric) -> Master[,4:dim(Master)[2]]



#check to make sure all teams have KenPom data
if(dim(Master[is.na(Master$Pyth),])[1] == 0 ){
  print("All teams have KenPom Data")
} else {
  print("Check this again")
  
}
#10 teams without KenPom data, will purge from Master:
Master %>% filter(!(is.na(Master$Pyth))) -> Master
#check to make sure all teams have KenPom data
if(dim(Master[is.na(Master$Pyth),])[1] == 0 ){
  print("All teams have KenPom Data")
} else {
  print("Check this again")
}
if( dim(filter(Master, is.na(TOV)))[1] == 0) {
  print("All teams have Sports Resource Data")
} else {
  print("Check SR again")
}
rm(SR_df)

#Is there a way to replace the missing values in:
#pace (32k)
#Off_rating(32k)
#TotReb (30)
#steal_pc (32)
#Block_pc (28)
#Off_Reb_pc(32k)
#Pace modeling
Master %>% filter(!(is.na(Pace))) -> Master.nona
nona.model <- glm(Pace~AdjustT+AdjustO, data=Master.nona)
cor(Master.nona$Pace,nona.model$fitted.values) -> stat_corr
print(paste("GLM using AdjustT and AdjustO predicts Pace at r^2 of:", stat_corr))
#add values to pace that are missing
if(stat_corr > 0.9){
  print("Replacing NAs in Pace...")
  Master$Pace[is.na(Master$Pace)] <- 
    predict(nona.model, newdata= Master[is.na(Master$Pace),])
  print("Done!")
} else {
  print("Correlation too low, revise model")
}

######ORB
Master %>% filter(!(is.na(ORB))) -> Master.nona
nona.model <- glm(ORB~TRB+AdjustO, data=Master.nona)
cor(Master.nona$ORB,nona.model$fitted.values) -> stat_corr
print(paste("GLM using TRB and AdjustO predicts ORB at r^2 of:", stat_corr))
#add values to ORB that are missing
if(stat_corr > 0.8){
  print("Replacing NAs in ORB...")
  Master$ORB[is.na(Master$ORB)] <- 
    predict(nona.model, newdata= Master[is.na(Master$ORB),])
  print("Done!")
} else {
  print("Correlation too low, revise model")
}
####Off_Rating
Master %>% filter(!(is.na(ORtg))) -> Master.nona
nona.model <- glm(ORtg~AdjustO+TS_pc, data=Master.nona)
cor(Master.nona$ORtg,nona.model$fitted.values) -> stat_corr
print(paste("GLM using AdjustO and TS_pc predicts ORtg at r^2 of:", stat_corr))
#add values to ORtg that are missing
if(stat_corr > 0.8){
  print("Replacing NAs in ORtg...")
  Master$ORtg[is.na(Master$ORtg)] <- 
    predict(nona.model, newdata= Master[is.na(Master$ORtg),])
  print("Done!")
} else {
  print("Correlation too low, revise model")
}
####STL_pc
Master %>% filter(!(is.na(STL_pc))) -> Master.nona
nona.model <- glm(STL_pc~AdjustD+STL, data=Master.nona)
cor(Master.nona$STL_pc,nona.model$fitted.values) -> stat_corr
print(paste("GLM using AdjustD and STL predicts STL_pc at r^2 of:", stat_corr))
#add values to STL_pc that are missing
if(stat_corr > 0.8){
  print("Replacing NAs in STL_pc...")
  Master$STL_pc[is.na(Master$STL_pc)] <- 
    predict(nona.model, newdata= Master[is.na(Master$STL_pc),])
  print("Done!")
} else {
  print("Correlation too low, revise model")
}
####ORB_pc
Master %>% filter(!(is.na(ORB_pc))) -> Master.nona
nona.model <- glm(ORB_pc~ORB+TRB, data=Master.nona)
cor(Master.nona$ORB_pc,nona.model$fitted.values) -> stat_corr
print(paste("GLM using ORB and TRB predicts ORB_pc at r^2 of:", stat_corr))
#add values to ORB_pc that are missing
if(stat_corr > 0.8){
  print("Replacing NAs in ORB_pc...")
  Master$ORB_pc[is.na(Master$ORB_pc)] <- 
    predict(nona.model, newdata= Master[is.na(Master$ORB_pc),])
  print("Done!")
} else {
  print("Correlation too low, revise model")
}

##TRB_pc
Master %>% filter(!(is.na(TRB_pc))) -> Master.nona
nona.model <- glm(TRB_pc~ORB+TRB, data=Master.nona)
cor(Master.nona$TRB_pc,nona.model$fitted.values) -> stat_corr
print(paste("GLM using ORB and TRB predicts TRB_pc at r^2 of:", stat_corr))
#add values to TRB_pc that are missing
if(stat_corr > 0.8){
  print("Replacing NAs in TRB_pc...")
  Master$TRB_pc[is.na(Master$TRB_pc)] <- 
    predict(nona.model, newdata= Master[is.na(Master$TRB_pc),])
  print("Done!")
} else {
  print("Correlation too low, replaced anyway due to low number of observations <20")
  Master$TRB_pc[is.na(Master$TRB_pc)] <- 
    predict(nona.model, newdata= Master[is.na(Master$TRB_pc),])
  print("Done!")
}
#BLK_pc

Master %>% filter(!(is.na(BLK_pc))) -> Master.nona
nona.model <- glm(BLK_pc~AdjustD+BLK, data=Master.nona)
cor(Master.nona$BLK_pc,nona.model$fitted.values) -> stat_corr
print(paste("GLM using AdjustD and BLK predicts BLK_pc at r^2 of:", stat_corr))
#add values to BLK_pc that are missing
if(stat_corr > 0.8){
  print("Replacing NAs in BLK_pc...")
  Master$BLK_pc[is.na(Master$BLK_pc)] <- 
    predict(nona.model, newdata= Master[is.na(Master$BLK_pc),])
  print("Done!")
} else {
  print("Correlation too low, revise model")
}


print(paste("Master Database is ready!"))
print(dim(Master))
