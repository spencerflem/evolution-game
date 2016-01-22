library(plyr)
library(prob)

#TODO:
#do calculations by hand
#then check output of each function

baseCreature <- data.frame(popSize = 100, quality = 800, timeToCatch = 5, seenChance = 50, predator = FALSE, grass = FALSE)
grass <- data.frame(name = "grass", popSize = 2000, quality = 50, timeToCatch = 1, seenChance = 95, predator = FALSE, grass = TRUE)

predator <- data.frame(name = "predator", popSize = 100, timeToCatch = 30, seenChance = 20, predator = TRUE, grass = FALSE)
prey <- data.frame(name = "prey", popSize = 350, timeToCatch = 5, seenChance = 60, predator = FALSE, grass = FALSE)

creatures <- list('-1' = list(grass), '1' = list(predator), '2' = list(prey))

sumCreatures <- function(creatures) {
  summedCreatures <- data.frame()
  for (creature in creatures) {
    filledFrame <- data.frame()
    #NAME AND ID!!!!!!!!!!!!!!!!!!!!!!!!
    for(frame in creature) {
      filledFrame <- rbind.fill(filledFrame, frame)
    }
    name <- filledFrame[]
    filledFrame <- filledFrame[,-1]
    colSums <- colSums(filledFrame)
    filledFrame["Totals",] <- colSums
    summedCreatures <- rbind.fill(summedCreatures, tail(filledFrame, 1))
  }
  return(summedCreatures)
}

#MOVE THIS TO ADDING EVOLUTIONS?!?
#STORE AS ONE DATAFRAME, NOT LIST OF THEM?!?

stepSim  <- function(times, subTimes, creatures) {
  summedCreatures <- sumCreatures(creatures)
  populations <- summedCreatures$popSize
  print(populations)
  print("---")
  for(i in 1:times) {
    populations <- subStep(subTimes, summedCreatures)
  }
  print(populations)
}

subStep <- function(subTimes, summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  print("hihi6")
  eatenMatrix <- calculateEatenMatrix(summedCreatures)
  timeToCatch <- computeTimeToCatch(summedCreatures)
  print("em:")
  print(eatenMatrix)
  print("ttc:")
  print(timeToCatch)
  timeShards <- eatenMatrix * timeToCatch
  totalCatchTimes <- rowSums(timeShards)
  catchesPerStep <- sapply(totalCatchTimes, function(x){
    oox <- 1/x
    if(is.infinite(oox)) {
      return(0)
    }
    else {
      return(oox)
    }
  })
  populations <- summedCreatures$popSize
  for(i in 1:subTimes) {
    numEatenMatrix <- eatenMatrix * catchesPerStep * populations #MAY BE MORE DELICATE THAN JUST THIS!!!!!
    print("cps:")
    print(catchesPerStep)
    print("pops:")
    print(populations)
    print("numEatenMatrix")
    numEaten <- colSums(numEatenMatrix)
    print("ne:")
    print(numEaten)
    populations <- populations + numEaten
  }
  print("OUT!")
  return(populations)
}

#NEEDS METHOD OF GROWTH!!! - BASED ON CALORIES NEEDED?

computeTimeToCatch <- function(summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  timeToCatchMatrix <- matrix(summedCreatures$timeToCatch, nrow = numCreatures, ncol = numCreatures)
  return(timeToCatchMatrix)
}

computeChanceToBeSeen <- function(summedCreatures) {
  #apply evos here
  numCreatures <- nrow(summedCreatures)
  print(summedCreatures)
  print(summedCreatures$seenChance)
  chanceMatrix <- matrix(summedCreatures$seenChance, nrow = numCreatures, ncol = numCreatures)
  print("%")
  print(chanceMatrix) #SIDEWAYS?
  return(chanceMatrix)
}

computeSampleSpace <- function(chanceToBeSeenRow) {
  print(chanceToBeSeenRow)
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
  
  #apply evos here
  #this should account for TIME TO CATCH
  
  numCreatures <- nrow(creatures)
  valueMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      value <- creatures[j,3]*creatures[j,4]
      if(i == j) {
        value <- 0
      }
      if(creatures[i,]$predator == 0) {
        if(creatures[j,]$grass == 0) {
          value <- 0
        }
      }
      if(creatures[i,]$predator >= 1) {
        if(creatures[j,]$grass >= 1) {
          value <- 0
        }
      }
      if(creatures[i,]$grass >= 1) {
        value <- -1
      }
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

calculateEatenMatrix <- function(summedCreatures) {
  print("aadadassdsads")
  numCreatures <- nrow(summedCreatures)
  chanceToBeSeen <- computeChanceToBeSeen(summedCreatures)
  print(chanceToBeSeen)
  print("----")
  values <- computeValues(summedCreatures)
  eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  for(i in 1:numCreatures) {
    sampleSpace <- computeSampleSpace(chanceToBeSeen[,i])
    chanceToEat <- computeChanceToEat(sampleSpace, values[,i])
    print('0909090')
    print(chanceToEat)
    print('0909090')
    #append row of matrix
  }
  #ensure 0-1 not 0-100
  return(eatenMatrix)
}

stepSim(1,1,creatures)