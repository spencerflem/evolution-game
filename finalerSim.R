library(plyr)
library(prob)

grass <- data.frame(name = "grass", popSize = 2000, calories = 40, catchesPerStep = 1, seenChance = 95, predator = FALSE, grass = TRUE, caloriesRequired = -1, lifeExpectancy = -1, babyCalories = -1, maxBabies = -1, babySurviveChance = -1)

pred1 <- data.frame(name = "predator", popSize = 100, calories = 800, catchesPerStep = .40, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 400, lifeExpectancy = 30, babyCalories = 90, maxBabies = 2, babySurviveChance = 70)
pred2 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = .10, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 300, lifeExpectancy = 25, babyCalories = 110, maxBabies = 1, babySurviveChance = 85)
pred3 <- data.frame(name = "predator", popSize = 50, calories = 1000, catchesPerStep = .40, seenChance = 10, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 85, maxBabies = 3, babySurviveChance = 75)

prey1 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 1.5, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 75, lifeExpectancy = 10, babyCalories = 25, maxBabies = 8, babySurviveChance = 40)
prey2 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 1.5, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 100, lifeExpectancy = 8, babyCalories = 20, maxBabies = 8, babySurviveChance = 30)
prey3 <- data.frame(name = "prey", popSize = 350, calories = 200, catchesPerStep = 1.9, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 75, lifeExpectancy = 10, babyCalories = 15, maxBabies = 12, babySurviveChance = 40)
prey4 <- data.frame(name = "prey", popSize = 350, calories = 100, catchesPerStep = 1.1, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 50, lifeExpectancy = 12, babyCalories = 17, maxBabies = 14, babySurviveChance = 35)
prey5 <- data.frame(name = "prey", popSize = 350, calories = 400, catchesPerStep = 2.1, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 50, lifeExpectancy = 10, babyCalories = 25, maxBabies = 9, babySurviveChance = 45)

creatures <- list('-1' = list(grass), '2' = list(pred1), '66' = list(pred2), '3' = list(pred3), '99' = list(prey1), '4' = list(prey2), '5' = list(prey3), '6' = list(prey4), '2' = list(prey5))

#TODO: ADD STEPSIZE!!!
#rem maxbabies?

sumCreatures <- function(creatures) {
  summedCreatures <- data.frame()
  for (creature in creatures) {
    filledFrame <- data.frame()
    #NAME AND ID?
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

stepSim  <- function(times, subTimes, stepSize, creatures) {
  summedCreatures <- sumCreatures(creatures)
  print(summedCreatures$popSize)
  for(i in 1:times) {
    summedCreatures$popSize <- subStep(subTimes, stepSize, summedCreatures)
    print("---")
    print(summedCreatures$popSize)
  }
  #TODO
  #OUTPUT SOMEHOW TO CREATURES FRAME
}

subStep <- function(subTimes, stepSize, summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  eatenMatrix <- calculateEatenMatrix(summedCreatures)
  catchesPerStep <- computecatchesPerStep(summedCreatures)
  catchesPerStepMatrix <- matrix(catchesPerStep, nrow = numCreatures, ncol = numCreatures)
  print("CPSM")
  print(catchesPerStepMatrix)
  populations <- summedCreatures$popSize
  print("PM:")
  popMatrix <- matrix(populations, nrow = numCreatures, ncol = numCreatures)
  print(popMatrix)
  for(i in 1:subTimes) {
    print("EM:")
    print(eatenMatrix)
    print("CPS")
    print(catchesPerStep)
    print("NEM:")
    numEatenMatrix <- eatenMatrix * catchesPerStepMatrix * popMatrix
    print(numEatenMatrix)
    growth <- computeGrowth(summedCreatures, numEatenMatrix)
    populations <- populations + growth * stepSize
  }
  populations <- sapply(populations, function(x){ceiling(x)})
  return(populations)
}

computeGrowth <- function(summedCreatures, numEatenMatrix) {
  numCreatures <- nrow(summedCreatures)
  growth <- c()
  numEaten <- colSums(numEatenMatrix)
  caloriesMatrix <- matrix(summedCreatures$calories, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  caloriesEatenMatrix <- numEatenMatrix * caloriesMatrix
  print("CM:")
  print(caloriesMatrix)
  print("CEM:")
  print(caloriesEatenMatrix)
  caloriesGained <- 2 * rowSums(caloriesEatenMatrix)
  print("CG:")
  print(caloriesGained)
  print("CR:")
  print(summedCreatures$caloriesRequired * summedCreatures$popSize)
  for(i in 1:numCreatures) {
    print("---------------------------------------------------------------")
    creature <- summedCreatures[i,]
    growthVal <- 0
    
    underfedLosses <- 0
    babiesSpawned <- 0
    babiesSurvived <- 0
    caloriesDifference <- caloriesGained[i] - creature$popSize * creature$caloriesRequired #ARBY $ why are cals so low?
    print("CD:")
    print(caloriesDifference)
    if(caloriesDifference < 0) {
        underfedLosses <- -1 * caloriesDifference / creature$caloriesRequired
    }
    else {
      babiesSpawned <- caloriesDifference / creature$babyCalories
      maxBabies <- creature$popSize * creature$maxBabies
      if(babiesSpawned > maxBabies) {
        babiesSpawned <- maxBabies 
      }
      babiesSurvived <- babiesSpawned * creature$babySurviveChance / 100
    }
    print("UL:")
    print(underfedLosses)
    print("BS:")
    print(babiesSurvived)
    
    lifeLosses <- 0
    eatenLosses <- numEaten[i]
    lifeExpectancyLosses <- creature$popSize / creature$lifeExpectancy
    if(eatenLosses > lifeExpectancyLosses) {
      lifeLosses <- eatenLosses
    }
    else {
      lifeLosses <- lifeExpectancyLosses
    }
    
    print("ll")
    print(lifeLosses)
    
    if(summedCreatures[i,]$grass >= 1) {
      growthVal <- creature$popSize * 0.3 - numEaten[i]
    }
    else {
      growthVal <- babiesSurvived - lifeLosses - underfedLosses
    }
    growth <- c(growth, growthVal)
    print("-----")
    print("GV:")
    print(growthVal)
  }
  
  print("---------------------------------------------------------------")
  print("GROWTH:")
  print(growth)
  print("---------------------------------------------------------------")
  return(growth)
}

computecatchesPerStep <- function(summedCreatures) {
  #apply evos here
  numCreatures <- nrow(summedCreatures)
  catchesPerStepMatrix <- matrix(summedCreatures$catchesPerStep, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  return(catchesPerStepMatrix)
}

computeChanceToBeSeen <- function(summedCreatures) {
  #apply evos here
  
  #TODO
  #seen chance - 2 parts
  #chanceToBeNear based on popsize
  #chanceToBeSpotted
  
  #FOR EASE SAKE:
  numCreatures <- nrow(summedCreatures)
  chanceToBeSpottedMatrix <- matrix(summedCreatures$seenChance, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  chanceToBeNear <- 1
  chanceToBeNearMatrix <- matrix(chanceToBeNear, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  chanceMatrix <- chanceToBeNearMatrix * chanceToBeSpottedMatrix
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
  return(sampleSpace)
}

computeValues <- function(creatures) {
  
  #apply evos here
  #this should account for TIME TO CATCH
  
  numCreatures <- nrow(creatures)
  #actually summedCreatures
  valueMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      value <- creatures[j,]$calories / creatures[j,]$catchesPerStep
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
  numCreatures <- nrow(summedCreatures)
  chanceToBeSeen <- computeChanceToBeSeen(summedCreatures)
  values <- computeValues(summedCreatures)
  eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  for(i in 1:numCreatures) {
    sampleSpace <- computeSampleSpace(chanceToBeSeen[i,])
    chanceToEat <- computeChanceToEat(sampleSpace, values[i,])
    eatenMatrix[i,] <- chanceToEat
  }
  return(eatenMatrix)
}

stepSim(10,2,0.05,creatures)