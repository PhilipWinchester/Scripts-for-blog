library(readxl)
#Making DF with all the game data
FPL <- read_excel("Desktop/FPL.xlsx", sheet = "Calculation", range = "A46:AM66")

#Adding number of interesting players 
FPL[["Players"]] <- c(0,3,2,1,1,3,2,3,2,1,1,2,1,0,1,2,1,1,2,1)

#Removing teams with no interesting players
FPL <- FPL[!(FPL$Players==0),]
#Rearranging DF
FPL <- FPL[,c(1,40,2:39)]

#Making vector containing all the player teams
TeamsVector <-c()
for(i in 1:length(FPL$Team)){
  j <- cumsum(FPL$Players)[i]-FPL$Players[i]
  for(k in 1:FPL$Players[i]){
    TeamsVector[j+k] <- FPL$Team[i]
    }
}

#Making matrix containing all five player combinations
PlayerMatrix <- t(combn(TeamsVector,5))

GwStart = 16 #First gameweek we consider
GwEnd = 20 #Last gameweek we consider
n = GwEnd - GwStart + 1 #Number of games we are looking at

#Making DF only including gameweeks we consider
FPL2 <- FPL[c(1,(GwStart+2):(GwEnd+2))]

SumMinusMax <- function(x){
  #Takes numeric vector and returns the cumulative value minus the largest value
  return(sum(x)-max(x))
}

MatrixMaker  <- function(L,DataFrame,n=5){
  #takes a list of teams and a date frame and produces a matrix with their 
  #game difficulties. n is the number of gameweeks we are looking at. 
  v1 <- as.numeric(DataFrame[match(L[1],DataFrame$Team),c(2:(n+1))])
  v2 <- as.numeric(DataFrame[match(L[2],DataFrame$Team),c(2:(n+1))])
  mat <- matrix(c(v1,v2),nrow=length(v1))
  for(i in 3:length(L)){
    vi <- as.numeric(DataFrame[match(L[i],DataFrame$Team),c(2:(n+1))])
    mat <- cbind(mat,vi)
  }
  return(t(mat))
}

Score <- function(M) {
  #Takes a matrix and return the sum of the SumMinusMax values of the columns
  Ans <- 0
  for(i in 1:ncol(M)){
    Ans <- Ans + SumMinusMax(M[,i]) 
  }
  return(Ans)
}

#Filling the vector Scores with the scores for the the set of players in the PlayerMatrix
Scores <- c()
for(i in 1:nrow(PlayerMatrix)){ 
  Scores[i] <- Score(MatrixMaker(PlayerMatrix[i,],FPL2))
  
}

#Add the scores to the PlayerMatrix and a couple of interesting stats
Ans <- cbind(PlayerMatrix, Scores, rank(Scores),rank(Scores)/length(Scores))
#Producing the csv file
write.csv(Ans, file = "Result.csv")