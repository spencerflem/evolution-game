#This should be the FINAL working draft of the simulator
#It is seperated from the UI here for simplicity

library(prob)
library(deSolve)

#obiously when the evolutions list is done the stats will be much more varied
#this is about as simple as it gets here

#each sub-list is a creature
#of type (r-value, population, quality, catchesPerStep, chancetobeseen, influence, ispredator, isgrass)
creatures <- matrix(
  c(-0.2, 1, 800, 1, 50, .4, 1, 0, #predator
    -0.2, 1.2, 550, 1, 30, .4, 0, 0, #prey
    0.2, 2, 50, 5, 95, .8, 0, 1), #grass
  nrow = 3, byrow = TRUE
)
#dataframe? dimnames?

populationSizes <- creatures[,2] #NOT RESILIANT TO ADDING NEW CREATURES!

calculateRvalue <- function(creatures) {
  return(creatures[,1])
}

computeChanceToBeSeen <- function(creatures) {
  numCreatures <- nrow(creatures)
  chanceMatrix <- matrix(creatures[,5], nrow = numCreatures, ncol = numCreatures)
  return(chanceMatrix)
}

computeSampleSpace <- function(chanceToBeSeenRow) {
  numCreatures <- length(chanceToBeSeenRow)
  binarySpace <- tosscoin(numCreatures)
  probs <- c()
  for(i in 1:nrow(binarySpace)) {
    probability <- 1
    for(j in 1:numCreatures) {
      if(binarySpace[i,j] == "H") {
        probability <- probability * (chanceToBeSeenRow[j]/100)
      }
      else {
        probability <- probability * ((100 - chanceToBeSeenRow[j])/100)
      }
    }
    probs <- c(probs, probability)
  }
  sampleSpace <- probspace(binarySpace, probs)
  print(sampleSpace)
  return(sampleSpace)
}

computeValues <- function(creatures) {
  numCreatures <- nrow(creatures)
  valueMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      value <- creatures[j,3]*creatures[j,4]
      if(i == j) {
        value <- 0
      }
      if(creatures[i,7] == 0) {
        if(creatures[j,8] == 0) {
          value <- 0
        }
      }
      if(creatures[i,7] == 1) {
        if(creatures[j,8] == 1) {
          value <- 0
        }
      }
      if(creatures[i,8] == 1) {
        value <- -1
      }
      print(value)
      valueMatrix[i,j] <- value
    }
  }
  return(valueMatrix)
}

computeChanceToEat <- function(sampleSpace, values) {
  numCreatures <- length(values)
  eatenRow <- rep(0, numCreatures)
  for(i in 1:nrow(sampleSpace)) {
    highestVal <- 0
    highestIndex <- -1
    for(j in 1:numCreatures) {
      if(sampleSpace[i,j] == "H") {
        if(values[j] > highestVal) {
          highestVal <- values[j]
          highestIndex <- j
        }
      }
    }
    if(highestIndex > -1) {
      probability <- sampleSpace[i,(numCreatures + 1)]
      eatenRow[highestIndex] <- eatenRow[highestIndex] + probability
    }
  }
  return(eatenRow)
}

computeInfluences <- function(creatures) {
  return(creatures[,6])
}

computeInteractionMatrixFragment <- function(chanceToEat, influence) {
  numCreatures <- length(chanceToEat)
  scaledChanceToEat <- sapply(chanceToEat, function(x){influence*x})
  return(scaledChanceToEat)
}

calculateInteractionMatrix <- function(creatures) {
  numCreatures <- nrow(creatures)
  chanceToBeSeen <- computeChanceToBeSeen(creatures)
  values <- computeValues(creatures)
  influences <- computeInfluences(creatures)
  interactionMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  for(i in 1:numCreatures) {
    sampleSpace <- computeSampleSpace(chanceToBeSeen[,i])
    chanceToEat <- computeChanceToEat(sampleSpace, values[,i])
    interactionMatrixFragment <- computeInteractionMatrixFragment(chanceToEat,influences[i])
    interactionMatrix[i,] <- interactionMatrix[i,] - interactionMatrixFragment
    interactionMatrix[,i] <- interactionMatrix[,i] + interactionMatrixFragment
  }
  print(interactionMatrix)
  return(interactionMatrix)
}

stepSim <- function(creatures) {
  
  starting <- creatures[,2]
  r <- calculateRvalue(creatures) #r-value, + if plant, - if creatrue
  a <- calculateInteractionMatrix(creatures)
  
  steps <- 7
  N0 <- starting
  final <- starting
  
  while (steps > 0) {
    steps <- steps - 1
    parms <- list(r,a)
    t=seq(1,1000, by=1)
    lvout <- ode(N0, t, function (t, n, parms) 
    {
      r <- parms[[1]]
      a <- parms[[2]]
      dns.dt <- n * (r + (a %*% n)[,1]) #LOOK INTO THIS LINE: MIGHT BE RIFE FOR CHANGE!
      return(list(c(dns.dt)))
    }, parms)
    parms2 <- list(r,a,lvout)
    lvout2 <- ode(N0, t, function (t, n, parms2) 
    {
      r <- parms2[[1]]
      a <- parms2[[2]]
      n2 <- parms2[[3]]
      n3 <- n2[t,-1]
      dns.dt <- n3 * (r + (a %*% n3)[,1])  * (2^(-0.02 * t))
      return(list(c(dns.dt)))
    }, parms2)
    lastrow <- lvout2[999,-1]
    changes <- lastrow - N0
    N0 <- N0 + changes
    final <- N0
  }
  creatures[,2] <<- final
  populationSizes <<- rbind(populationSizes,final)
}

stepSim(creatures)

matplot(populationSizes, type = "l", log = "y")