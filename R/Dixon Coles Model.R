# Things to look at: Why we get an error when we set larger epsilon! have checked and the epsilon stuff does work, still not sure why error when large one

# Should write about the mod stuff and about how he had a bunch of wrong stuff in paper

#Every time a run it, i could save the results in an excel spreadsheet maybe?



library(dplyr)
library("ggplot2")

NMod<- function(Vector,n=1){
  # Takes vector and returns n*mod
  NMod <- 0
  for(i in 1:length(Vector)){
    NMod <- NMod + Vector[i]^2
  }
  return((NMod)^(0.5)*n)
}


# Read in dataframe with results from last three EPL and Championship results
Match_Data_Original <- read.csv("Match Data.csv", header = TRUE, stringsAsFactors=FALSE) 

# Todays date
Today <- as.Date(Sys.Date())

# Extracting columns we are interested in, and adding the time column
Match_Data <- Match_Data_Original[c("HomeTeam", "AwayTeam", "FTHG", "FTAG", "Date", "Div")] %>% mutate(t = as.integer(Today-as.Date(gsub("/", ".", Date), "%d.%m.%Y"))) %>% filter(Div == "E0")

# Sorted vector with all the teams
Teams <- sort(unique(Match_Data$HomeTeam))

tau <- Vectorize(function(x, y, lambda, mu, rho){
  #Defining the tau function
  if (x == 0 & y == 0){return(1 - (lambda*mu*rho))
  } else if (x == 0 & y == 1){return(1 + (lambda*rho))
  } else if (x == 1 & y == 0){return(1 + (mu*rho))
  } else if (x == 1 & y == 1){return(1 - rho)
  } else {return(1)}
})

phi <- function(t, epsilon = 0.0019){
  # Define the weight function
  return(exp(-epsilon*t))
}

MatchLL <- function(x,y,ai, aj, bi, bj, gamma, rho, t){
  # A function which calcualtes the log likelihood of some game
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(phi(t)*sum(log(tau(x, y, lambda, mu, rho)) - lambda + x*log(lambda) - mu + y*log(mu)))
}


LL <- function(Match_Data, Parameters){
  # Function which calclulates the LL for all the games
  LL <- 0 
  
  Teams <- sort(unique(Match_Data$HomeTeam))
  
  # Fixing gamma and rho, as these are constant for all games
  gamma <- Parameters[2*length(Teams)+1]
  rho <- Parameters[2*length(Teams)+2]
  
  for(k in 1:nrow(Match_Data)){
    # Finding index for the home and away team
    IndexHome <- match(Match_Data$HomeTeam[k],Teams)
    IndexAway <- match(Match_Data$AwayTeam[k],Teams)
    
    # Finding relevant Parameters and other variables
    ai <- Parameters[IndexHome]
    aj <- Parameters[IndexAway] 
    bi <- Parameters[IndexHome + length(Teams)]
    bj <- Parameters[IndexAway + length(Teams)] 
    t <- Match_Data$t[k]
    x <- Match_Data$FTHG[k]
    y <- Match_Data$FTAG[k]
    
    #Adding the LL from game k to the total
    LL <- LL + MatchLL(x,y,ai, aj, bi, bj, gamma, rho, t)
  }
  
  return(LL)
}


# Functions for alpha derivative are below

GradAlphaHomeZeroZero <- function(ai, aj, bi, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(phi(t)*bj*(-gamma-mu*gamma*rho/(1-lambda*mu*rho)))
}

GradAlphaHomeZeroOne <- function(ai, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  return(phi(t)*bj*(-gamma+gamma*rho/(1+lambda*rho)))
}

GradAlphaHomeNotZero <- function(ai, bj, gamma, x,t){
  return(phi(t)*(x/ai-bj*gamma))
}

GradAlphaHome <- function(ai, aj, bi, bj, gamma, rho,t,x,y){
  # Funtion which determines the addition to the gradient of the home attacking strenth from some game
  if(x == 0 & y == 0){
    return(GradAlphaHomeZeroZero(ai, aj, bi, bj, gamma, rho,t))
  } else if(x == 0 & y == 1){
    return(GradAlphaHomeZeroOne(ai, bj, gamma, rho,t))
  } else{
    return(GradAlphaHomeNotZero(ai, bj, gamma, x,t))
  }
}

GradAlphaAwayZeroZero <- function(ai, aj, bi, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(phi(t)*bi*(-1-lambda*rho/(1-lambda*mu*rho)))
}

GradAlphaAwayOneZero <- function(aj, bi, rho,t){
  mu <- aj*bi
  return(phi(t)*bi*(-1+rho/(1+mu*rho)))
}

GradAlphaAwayNotZero <- function(aj, bi, y,t){
  return(phi(t)*(y/aj-bi))
}

GradAlphaAway <- function(ai, aj, bi, bj, gamma, rho,t,x,y){
  # Funtion which determines the addition to the gradient of the away attacking strenth from some game
  if(x == 0 & y == 0){
    return(GradAlphaAwayZeroZero(ai, aj, bi, bj, gamma, rho,t))
  } else if(x == 1 & y == 0){
    return(GradAlphaAwayOneZero(aj, bi, rho,t))
  } else{
    return(GradAlphaAwayNotZero(aj, bi, y,t))
  }
}

# Functions for beta derivative are below

GradBetaHomeZeroZero <- function(ai, aj, bi, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(phi(t)*aj*(-1-lambda*rho/(1-lambda*mu*rho)))
}

GradBetaHomeOneZero <- function(aj, bi, rho,t){
  mu <- aj*bi
  return(phi(t)*aj*(-1+rho/(1+mu*rho)))
}

GradBetaHomeNotZero <- function(aj, bi, y,t){
  return(phi(t)*(y/bi-aj))
}

GradBetaHome <- function(ai, aj, bi, bj, gamma, rho,t,x,y){
  # Funtion which determines the addition to the gradient of the home defense strenth from some game
  if(x == 0 & y == 0){
    return(GradBetaHomeZeroZero(ai, aj, bi, bj, gamma, rho,t))
  } else if(x == 1 & y == 0){
    return(GradBetaHomeOneZero(aj, bi, rho,t))
  } else{
    return(GradBetaHomeNotZero(aj, bi, y,t))
  }
}

GradBetaAwayZeroZero <- function(ai, aj, bi, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(phi(t)*ai*(-gamma-mu*gamma*rho/(1-lambda*mu*rho)))
}

GradBetaAwayZeroOne <- function(ai, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  return(phi(t)*ai*(-gamma+rho*gamma/(1+lambda*rho)))
}

GradBetaAwayNotZero <- function(ai, bj, gamma,x,t){
  lambda <- ai*bj*gamma
  return(phi(t)*(x/bj-ai*gamma))
}

GradBetaAway <- function(ai, aj, bi, bj, gamma, rho,t,x,y){
  # Funtion which determines the addition to the gradient of the away defense strenth from some game
  if(x == 0 & y == 0){
    return(GradBetaAwayZeroZero(ai, aj, bi, bj, gamma, rho,t))
  } else if(x == 0 & y == 1){
    return(GradBetaAwayZeroOne(ai, bj,gamma, rho,t))
  } else{
    return(GradBetaAwayNotZero(ai, bj, gamma, x,t))
  }
}

# Functions for gamma derivative are below

GradGammaZeroZero <- function(ai, aj, bi, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(phi(t)*ai*bj*(-1-mu*rho/(1-lambda*mu*rho)))
}

GradGammaZeroOne <- function(ai, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  return(phi(t)*ai*bj*(-1+rho/(1+lambda*rho)))
}

GradGammaNotZero <- function(ai, bj, gamma, x,t){
  return(phi(t)*(-ai*bj+x/gamma))
}

GradGamma <- function(ai, aj, bi, bj, gamma, rho,t,x,y){
  # Funtion which determines the addition to the gradient of the gamma param from some game
  if(x == 0 & y == 0){
    return(GradGammaZeroZero(ai, aj, bi, bj, gamma, rho,t))
  } else if(x == 0 & y == 1){
    return(GradGammaZeroOne(ai, bj, gamma, rho,t))
  } else{
    return(GradGammaNotZero(ai, bj, gamma, x,t))
  }
}

# Functions for rho derivative are below

GradRhoZeroZero <- function(ai, aj, bi, bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  mu <- aj*bi
  return(-phi(t)*lambda*mu/(1-lambda*mu*rho))
}

GradRhoZeroOne <- function(ai,bj, gamma, rho,t){
  lambda <- ai*bj*gamma
  return(phi(t)*lambda/(1+lambda*rho))
}

GradRhoOneZero <- function(aj,bi, rho,t){
  mu <- aj*bi
  return(phi(t)*mu/(1+mu*rho))
}

GradRhoOneOne <- function(rho,t){
  return(-phi(t)/(1-rho))
}

GradRho <- function(ai, aj, bi, bj, gamma, rho,t,x,y){
  # Funtion which determines the addition to the gradient of the gamma param from some game
  if(x == 0 & y == 0){
    return(GradRhoZeroZero(ai, aj, bi, bj, gamma, rho,t))
  } else if(x == 0 & y == 1){
    return(GradRhoZeroOne(ai,bj, gamma, rho,t))
  } else if(x == 1 & y == 0){
    return(GradRhoOneZero(aj,bi, rho,t))
  } else if(x == 1 & y == 1){
    return(GradRhoOneOne(rho,t))
  } else{
    return(0)
  }
}

GradAdder <- function(Match_Data, Parameters, GradientVector,i, gamma, rho){
  # Function which takes the df of mathches, the current Parameters and calcualtes the addition to gradient vector for the i'th match
  # Returns the resulting gradient vector
  
  Teams <- sort(unique(Match_Data$HomeTeam))
  # Finding index for the home and away team
  IndexHome <- match(Match_Data$HomeTeam[i],Teams)
  IndexAway <- match(Match_Data$AwayTeam[i],Teams)
  
  # Finding relevant Parameters and other variables
  ai <- Parameters[IndexHome]
  aj <- Parameters[IndexAway] 
  bi <- Parameters[IndexHome + length(Teams)]
  bj <- Parameters[IndexAway + length(Teams)] 
  t <- Match_Data$t[i]
  x <- Match_Data$FTHG[i]
  y <- Match_Data$FTAG[i]
  
  # Adding onto the Gradient vector
  GradientVector[IndexHome] <- GradientVector[IndexHome] + GradAlphaHome(ai, aj, bi, bj, gamma, rho,t,x,y)
  GradientVector[IndexAway] <- GradientVector[IndexAway] + GradAlphaAway(ai, aj, bi, bj, gamma, rho,t,x,y)
  GradientVector[IndexHome + length(Teams)] <- GradientVector[IndexHome + length(Teams)] + GradBetaHome(ai, aj, bi, bj, gamma, rho,t,x,y)
  GradientVector[IndexAway + length(Teams)] <- GradientVector[IndexAway + length(Teams)] + GradBetaAway(ai, aj, bi, bj, gamma, rho,t,x,y)
  GradientVector[2*length(Teams) + 1] <- GradientVector[2*length(Teams) + 1] + GradGamma(ai, aj, bi, bj, gamma, rho,t,x,y)
  GradientVector[2*length(Teams) + 2] <- GradientVector[2*length(Teams) + 2] + GradRho(ai, aj, bi, bj, gamma, rho,t,x,y)
  
  return(GradientVector)
}

GradientVectorFinder <- function(Match_Data, Parameters){
  # Function whcih takes the match data, current Parameters and returns the Gradient Vector
  
  Teams <- sort(unique(Match_Data$HomeTeam))
  
  # Building the gradient vector 
  GradientVector <- rep(0, 2*length(Teams)+2)
  
  # Setting gamma and rho
  gamma <- Parameters[2*length(Teams)+1]
  rho <- Parameters[2*length(Teams)+2]
  
  # Running through all the matches, every i makes an addition to the gradient vector
  for(i in 1:nrow(Match_Data)){
    GradientVector <- GradAdder(Match_Data, Parameters, GradientVector,i, gamma, rho)
  }
  
  return(GradientVector)
}

NormalisingTheGradientVector <- function(GradientVector,n){
  # Function which takes the GradientVector and normalises it such that the average of the alpha gradients is 0.
  Teams <- sort(unique(Match_Data$HomeTeam))
  AlphaGradValues <- GradientVector[1:length(Teams)]
  AverageAlphaGradValues <- mean(AlphaGradValues) # This is the average of paramaters in notes. But in our corrections, we want to add the gradint. Hence, there should be a net 0 efferct on the everage of the alphas from the gradint, as they already add up to one.
  Normaliser <- rep(0, 2*length(Teams)+2)
  
  for(i in 1:length(Teams)){
    Normaliser[i] <- AverageAlphaGradValues
  }
  

  return((GradientVector - Normaliser)/NMod(GradientVector - Normaliser,n)) 


}


# Have tried to not devide by the NMod stuff. not sure if it works. still issues with not beeing able to find a good value for gamma.
# Will have to try to optemise it individually. have done this succesfully, but does not seem to change much. probs is smaller than expected.

Optimise <- function(Match_Data, Max = 100, m = 10){
  # Takes some match data and returns 
  
  Teams <- sort(unique(Match_Data$HomeTeam))
  
  # Setting all Parameters equal to 1 at first
  Parameters <- rep(1,2*length(Teams)+2)
  
  # Setting gamma equal to 1.4 and rho equal to 0
  Parameters[2*length(Teams)+1] <- 1.3
  Parameters[2*length(Teams)+2] <- -0.05

  Mult <- 1
  Step <- m
  
  
  count <- 0
  # Doing itertaitons until we have added just one of the smallets gradient vecor we weant to add
  while(Step <= Max){
    
    count <- count + 1
    print(paste("count is "  ,toString(count)))
    
    # Finding gradient 
    GradientVector <- GradientVectorFinder(Match_Data, Parameters)
    
    # Normalising (Avergage of alhpas is 1), and adjusting the length
    GradientVectorNormalised <- NormalisingTheGradientVector(GradientVector,Step)
    print(paste("step is "  ,toString(Step)))
    
    PresentPoint <- Parameters
    StepToPoint <- Parameters + GradientVectorNormalised
    LLLoop <- 0
    
    # Adding GradientVectorNormalised until we have maxemised the LL
    while(LL(Match_Data, Parameters=StepToPoint) > LL(Match_Data, Parameters=PresentPoint)){
      PresentPoint <- StepToPoint
      StepToPoint <- PresentPoint + GradientVectorNormalised
      LLLoop <- LLLoop + 1
    }
    
    
    print(paste("LLLoop is "  ,toString(LLLoop)))
    
    # If there has only been one itteration (or zero), we increase the step size
    if(LLLoop < 2){
      Mult <- Mult + 1
      Step <- Mult*m
    }
    
    Parameters <- PresentPoint
  }
  
  Alpha <- Parameters[1:length(Teams)]
  Beta <- Parameters[(length(Teams)+1):(length(Teams)*2)]
  Gamma <- Parameters[length(Teams)*2+1]
  Rho <- Parameters[length(Teams)*2+2]
  Results <- data.frame(Teams, Alpha, Beta, Gamma, Rho)
  
  
  return(Results)
  
}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  
  for(i in 2:ncol(x)){
    x[i] <-  round(x[i], digits)
  }
  return(x)
}


# prints
Results <- Optimise(Match_Data, Max = 200) # If we run this, we will se that all params but gamma are in good shape. could have 2nd bit where we optemise thi
Results <- round_df(Results,2)



# Plotting curve that shows different epsilons 
curve(exp(-0.001*x), xlim = c(0, 1000), ylim = c(0,1), n = 1001, col = "green", xlab = "t", ylab = "φ(t)")
curve(exp(-0.002*x), xlim = c(0, 1000), n = 1001, col = "orange", add = TRUE)
curve(exp(-0.003*x), xlim = c(0, 1000), n = 1001, col = "red", add = TRUE)
legend("topright", c("ε = 0.001", "ε = 0.002","ε = 0.003"), col = c("green", "orange", "red"), pch=16)

NMod(c(1,2,4)/NMod(c(1,2,4),50),1)

NormalisingTheGradientVector(c(1,2),2)
# Can do a match probabilty matrxi as the one done by dishee in the coles post 
