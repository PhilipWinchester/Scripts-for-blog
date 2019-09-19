library(dplyr)

# Reads in the dataframe with all the results so far
DF_Original <- read.csv("E0 (1).csv", header = TRUE) %>% mutate_if(is.factor, as.character)

# Picking the columns we are interested in
DF <- DF_Original[c("HomeTeam", "AwayTeam", "FTHG", "FTAG","FTR")]

# Picking the columns we are interested in and only the games played in the last 13 weeks
Last_Fixture <- as.Date(gsub("/", ".", DF_Original$Date[nrow(DF)]), "%d.%m.%Y")
DF_Recent <- DF_Original[c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "Date")] %>% mutate(Date = as.Date(gsub("/", ".", Date), "%d.%m.%Y")) %>%
  filter(Date > (Last_Fixture - 90))

# Finding the number of results we have
Rows_Original <- nrow(DF_Original)
Rows_Recent <- nrow(DF_Recent)

# Finding the position of some columns we will be using
HomePosition <- grep("HomeTeam", colnames(DF))
AwayPosition <- grep("AwayTeam", colnames(DF))
ResultPosition <- grep("FTR", colnames(DF))

# Creating a list with all the teams
Teams <- unique(DF$HomeTeam)

# Function which takes a team, looks at row i in the dateframe and returns the points under the condition that the team exists in the row
Points <- function(Team, i, DF, HomePosition=1,AwayPosition=2, ResultPosition = 5)
{if (DF[i,ResultPosition]=="D") {
  return(1)
} else if (DF[i,ResultPosition]=="H" & DF[i,HomePosition] == Team) {
  return(3)
} else if (DF[i,ResultPosition]=="A" & DF[i,AwayPosition] == Team) {
  return(3)
} else {
  return(0)
}
}

# Finding the form of each Home team before the fixture was played 
HomeForm <- rep(0,Rows_Original)
for (i in Rows_Original:2)
  {count <- 1 
  points <- rep(0,5)
  LTeam <- DF[i,HomePosition]
  j <- (i-1) 
  for(k in j:1)
    {if(DF[k,HomePosition] == LTeam | DF[k,AwayPosition] == LTeam){points[count] <- Points(Team = LTeam, k, DF)
    count <- count +1} 
    if(count == 6){break}
    }
  HomeForm[i] <- sum(points)
  }

# Finding the form of each Away team before the fixture was played 
AwayForm <- rep(0,Rows_Original)
for (i in Rows_Original:2)
  {count <- 1 
  points <- rep(0,5)
  LTeam <- DF[i,AwayPosition]
  j <- (i-1) 
  for(k in j:1)
    {if(DF[k,HomePosition] == LTeam | DF[k,AwayPosition] == LTeam){points[count] <- Points(Team = LTeam, k, DF)
    count <- count +1} 
    if(count == 6){break}
    }
  AwayForm[i] <- sum(points)
  }

# Creating the dataframe which we will input to our Model
ModelDF <- data.frame(Goals= c(DF$FTHG, DF$FTAG),Team = c(DF$HomeTeam, DF$AwayTeam), Opposition =  c(DF$AwayTeam, DF$HomeTeam), HA = c(rep("H",Rows_Original),rep("A",Rows_Original)), 
                      Form = c(HomeForm,AwayForm))

# Creating the dataframe which we will input to our Model with recent fixtures
ModelDF_Recent <- data.frame(Team = c(DF_Recent$HomeTeam, DF_Recent$AwayTeam), Opposition =  c(DF_Recent$AwayTeam, DF_Recent$HomeTeam), HA = c(rep("H",Rows_Recent),rep("A",Rows_Recent)), Goals= c(DF_Recent$FTHG, DF_Recent$FTAG))


# Creating the Model 
attach(ModelDF)
Model <- glm(Goals ~ Team + HA + Opposition + Form, family=poisson(link=log))
detach(ModelDF)

# Creating the Model for recent fixtures
attach(ModelDF_Recent)
Model_Recent <- glm(Goals ~ Team + HA + Opposition, family=poisson(link=log))
summary(Model_Recent) # Printing the summary 
detach(ModelDF_Recent)

library(readxl)
library(hflights)

# Reading in a dataframe with all the season fixtures and ammending some of the team names
Fixtures <- read_excel("Fixture list.xlsx", sheet = "EPL 18-19") %>% rename(HomeTeam = "HOME TEAM", AwayTeam = "AWAY TEAM") %>% select(HomeTeam, AwayTeam) %>%
  mutate(HomeTeam = gsub("chester","",HomeTeam)) %>% mutate(AwayTeam = gsub("chester","",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Leicester City","Leicester",HomeTeam)) %>% mutate(AwayTeam = gsub("Leicester City","Leicester",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Newcastle United","Newcastle",HomeTeam)) %>% mutate(AwayTeam = gsub("Newcastle United","Newcastle",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Tottenham Hotspur","Tottenham",HomeTeam)) %>% mutate(AwayTeam = gsub("Tottenham Hotspur","Tottenham",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Brighton and Hove Albion","Brighton",HomeTeam)) %>% mutate(AwayTeam = gsub("Brighton and Hove Albion","Brighton",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Wolverhampton Wanderers","Wolves",HomeTeam)) %>% mutate(AwayTeam = gsub("Wolverhampton Wanderers","Wolves",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Cardiff City","Cardiff",HomeTeam)) %>% mutate(AwayTeam = gsub("Cardiff City","Cardiff",AwayTeam)) %>%
  mutate(HomeTeam = gsub("Huddersfield Town","Huddersfield",HomeTeam)) %>% mutate(AwayTeam = gsub("Huddersfield Town","Huddersfield",AwayTeam)) %>%
  mutate(HomeTeam = gsub("West Ham United","West Ham",HomeTeam)) %>% mutate(AwayTeam = gsub("West Ham United","West Ham",AwayTeam)) %>%
  transform(Identifier = paste(HomeTeam,AwayTeam, sep="-"))

# Making a data frame which only includes the fixtures which are left to play 
PlayedGames <- rep(0,Rows_Original)
for (i in 1:Rows_Original)
{PlayedGames[i] <- paste(DF_Original$HomeTeam[i],DF_Original$AwayTeam[i], sep="-")}

for (i in 1:Rows_Original)
{Fixtures <- Fixtures[!(Fixtures$Identifier == PlayedGames[i]),]}

# Finding current form for teams
Index <- 0
Form <- rep(0,20)
for(i in Teams)
{Index <- Index +1
count <- 1 
points <- rep(0,5)
for(k in Rows_Original:2)
{if(DF[k,HomePosition] ==i | DF[k,AwayPosition] == i){points[count] <- Points(Team = i, k, DF)
count <- count +1} 
if(count == 6){break}
}
Form[Index] <- sum(points)
}

CurrentForm <- data.frame(Teams, Form)

# Aadding the team form for upcoming fixtures
Fixtures_For_Forms_Model <- Fixtures %>% transform(HomeForm = CurrentForm$Form[match(HomeTeam,CurrentForm$Teams)]) %>%
transform(AwayTeamForm = CurrentForm$Form[match(AwayTeam,CurrentForm$Teams)])

# Function that takes a home team, opositon, forms for each team and returns a matrix with score probabilities
SimulateMatch_With_Form <- function(Team, AwayTeam, HomeForm, AwayForm, Max = 5)
  {HomeLambda <- predict(Model, data.frame(HA = "H", Team = Team, Opposition = AwayTeam, Form = HomeForm), type = "response")
  AwayLambda <- predict(Model, data.frame(HA = "A", Team = AwayTeam, Opposition = Team, Form = AwayForm), type = "response")
  return(dpois(0:Max, HomeLambda) %o% dpois(0:Max, AwayLambda))}

SimulateMatch <- function(Team, AwayTeam, Max = 5)
  {HomeLambda <- predict(Model_Recent, data.frame(HA = "H", Team = Team, Opposition = AwayTeam), type = "response")
  AwayLambda <- predict(Model_Recent, data.frame(HA = "A", Team = AwayTeam, Opposition = Team), type = "response")
  return(dpois(0:Max, HomeLambda) %o% dpois(0:Max, AwayLambda))}

library(scales) # Includes the percent() function
library(skellam) # Includes the skellam distribution

# Function which return the probabilty of a draw
PDraw_With_Form <- function(Team, AwayTeam, HomeForm, AwayForm)
  {HomeLambda <- predict(Model, data.frame(HA = "H", Team = Team, Opposition = AwayTeam, Form = HomeForm), type = "response")
  AwayLambda <- predict(Model, data.frame(HA = "A", Team = AwayTeam, Opposition = Team, Form = AwayForm), type = "response")
  Answer <- dskellam(0,HomeLambda,AwayLambda)
  return(percent(Answer))}

# Function which return the probabilty of a home win
PHomeWin_With_Form <- function(Team, AwayTeam, HomeForm, AwayForm)
  {HomeLambda <- predict(Model, data.frame(HA = "H", Team = Team, Opposition = AwayTeam, Form = HomeForm), type = "response")
  AwayLambda <- predict(Model, data.frame(HA = "A", Team = AwayTeam, Opposition = Team, Form = AwayForm), type = "response")
  Answer <- pskellam(0,HomeLambda,AwayLambda, lower.tail = FALSE)
  return(percent(Answer))}

# Function which return the probabilty of an away win
PAwayWin_With_Form <- function(Team, AwayTeam, HomeForm, AwayForm)
  {HomeLambda <- predict(Model, data.frame(HA = "H", Team = Team, Opposition = AwayTeam, Form = HomeForm), type = "response")
  AwayLambda <- predict(Model, data.frame(HA = "A", Team = AwayTeam, Opposition = Team, Form = AwayForm), type = "response")
  Answer <- pskellam(-1,HomeLambda,AwayLambda)
  return(percent(Answer))}

# Function which return the probabilty of a draw
PDraw <- function(Team, AwayTeam)
  {HomeLambda <- predict(Model_Recent, data.frame(HA = "H", Team = Team, Opposition = AwayTeam), type = "response")
  AwayLambda <- predict(Model_Recent, data.frame(HA = "A", Team = AwayTeam, Opposition = Team), type = "response")
  Answer <- dskellam(0,HomeLambda,AwayLambda)
  return(percent(Answer))}

# Function which return the probabilty of a home win
PHomeWin <- function(Team, AwayTeam)
  {HomeLambda <- predict(Model_Recent, data.frame(HA = "H", Team = Team, Opposition = AwayTeam), type = "response")
  AwayLambda <- predict(Model_Recent, data.frame(HA = "A", Team = AwayTeam, Opposition = Team), type = "response")
  Answer <- pskellam(0,HomeLambda,AwayLambda, lower.tail = FALSE)
  return(percent(Answer))}

# Function which return the probabilty of an away win
PAwayWin <- function(Team, AwayTeam)
  {HomeLambda <- predict(Model_Recent, data.frame(HA = "H", Team = Team, Opposition = AwayTeam), type = "response")
  AwayLambda <- predict(Model_Recent, data.frame(HA = "A", Team = AwayTeam, Opposition = Team), type = "response")
  Answer <- pskellam(-1,HomeLambda,AwayLambda)
  return(percent(Answer))}


# Adding some percentages to the dataframe
Fixtures_For_Forms_Model <- Fixtures_For_Forms_Model %>% transform(HomeWinPercentage = PHomeWin_With_Form(HomeTeam,AwayTeam, HomeForm, AwayTeamForm), 
                                   DrawPercentage = PDraw_With_Form(HomeTeam,AwayTeam, HomeForm, AwayTeamForm), 
                                   AwayWinPercentage = PAwayWin_With_Form(HomeTeam,AwayTeam, HomeForm, AwayTeamForm))

Fixtures <- Fixtures %>% transform(HomeWinPercentage = PHomeWin(HomeTeam,AwayTeam), DrawPercentage = PDraw(HomeTeam,AwayTeam), AwayWinPercentage = PAwayWin(HomeTeam,AwayTeam)) 

Hej <- Fixtures[c("HomeTeam", "AwayTeam", "HomeWinPercentage", "DrawPercentage", "AwayWinPercentage")]



# Prints

