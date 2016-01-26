library(plyr)
library(prob)

grass <- data.frame(name = "grass", popSize = 20000, calories = 20, catchesPerStep = 2, seenChance = 95, predator = FALSE, grass = TRUE, caloriesRequired = -1, lifeExpectancy = -1, babyCalories = -1, maxBabies = -1, babySurviveChance = -1, chanceToBeNearMultiplier = 20)

pred1 <- data.frame(name = "predator", popSize = 100, calories = 800, catchesPerStep = 0.4, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 300, lifeExpectancy = 30, babyCalories = 90, maxBabies = 2, babySurviveChance = 70, chanceToBeNearMultiplier = 30)
pred2 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.6, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 25, babyCalories = 110, maxBabies = 1, babySurviveChance = 85, chanceToBeNearMultiplier = 30)
pred3 <- data.frame(name = "predator", popSize = 500, calories = 1000, catchesPerStep = 0.4, seenChance = 10, predator = TRUE, grass = FALSE, caloriesRequired = 100, lifeExpectancy = 30, babyCalories = 85, maxBabies = 3, babySurviveChance = 75, chanceToBeNearMultiplier = 30)

prey1 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 0.7, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 50, lifeExpectancy = 10, babyCalories = 200, maxBabies = 8, babySurviveChance = 40, chanceToBeNearMultiplier = 30)
prey2 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 1.8, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 75, lifeExpectancy = 8, babyCalories = 150, maxBabies = 8, babySurviveChance = 30, chanceToBeNearMultiplier = 30)
prey3 <- data.frame(name = "prey", popSize = 350, calories = 200, catchesPerStep = 0.8, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 50, lifeExpectancy = 10, babyCalories = 100, maxBabies = 12, babySurviveChance = 40, chanceToBeNearMultiplier = 30)
prey4 <- data.frame(name = "prey", popSize = 350, calories = 100, catchesPerStep = 1.0, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 25, lifeExpectancy = 12, babyCalories = 120, maxBabies = 14, babySurviveChance = 35, chanceToBeNearMultiplier = 30)
prey5 <- data.frame(name = "prey", popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 25, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, chanceToBeNearMultiplier = 20)

#creatures <- list('-1' = list(grass), '2' = list(pred1), '66' = list(pred2), '3' = list(pred3), '99' = list(prey1), '4' = list(prey2), '5' = list(prey3), '6' = list(prey4), '2' = list(prey5))
creatures <- list('-1' = list(grass), '66' = list(prey5))

#TODO: ADD STEPSIZE!!!
#rem maxbabies?

populationSizes <- c()

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
  print(summedCreatures$popSize) #IMPORTANT!
  populationSizes <<- summedCreatures$popSize
  for(i in 1:times) {
    summedCreatures$popSize <- subStep(subTimes, stepSize, summedCreatures)
    print(i)
    print("---------------------------------------------------") #IMPORTANT!
    print(summedCreatures$popSize) #IMPORTANT!
    print("---------------------------------------------------") #IMPORTANT!
    populationSizes <<- rbind(populationSizes,summedCreatures$popSize)
    
  }
  #TODO
  #OUTPUT SOMEHOW TO CREATURES FRAME
}

subStep <- function(subTimes, stepSize, summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  eatenMatrix <- calculateEatenMatrix(summedCreatures)
  catchesPerStep <- computecatchesPerStep(summedCreatures)
  catchesPerStepMatrix <- matrix(catchesPerStep, nrow = numCreatures, ncol = numCreatures)
  
  print("CatchesPerStepMtrx")
  print(catchesPerStepMatrix)
  
  populations <- summedCreatures$popSize
  
  print("PopulationMtrx:")
  
  popMatrix <- matrix(populations, nrow = numCreatures, ncol = numCreatures)
  
  print(popMatrix)
  
  for(i in 1:subTimes) {
    
    print("EatenMtrx:")
    print(eatenMatrix)
    print("CatchesPerStep")
    print(catchesPerStep)
    print("NumEatenMtrx:")
    
    numEatenMatrix <- eatenMatrix * catchesPerStepMatrix * popMatrix
    
    print(numEatenMatrix)
    
    growth <- computeGrowth(summedCreatures, numEatenMatrix)
    populations <- populations + growth * stepSize
    
    populations <- sapply(populations, function(x){
      if(x < 0) {
        return(0)
      }
      else {
        return(x)
      }
    })
  }
  populations <- sapply(populations, function(x){floor(x)})
  return(populations)
}

computeGrowth <- function(summedCreatures, numEatenMatrix) {
  numCreatures <- nrow(summedCreatures)
  growth <- c()
  numEaten <- colSums(numEatenMatrix)
  caloriesMatrix <- matrix(summedCreatures$calories, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  caloriesEatenMatrix <- numEatenMatrix * caloriesMatrix
  
  print("CaloriesMtrx:")
  print(caloriesMatrix)
  print("CaloriesEatenMtrx:")
  print(caloriesEatenMatrix)
  
  caloriesGained <- 2 * rowSums(caloriesEatenMatrix) #TODO: ARBY $ why are cals so low?
  
  print("CaloriesGained:")
  print(caloriesGained)
  print("CaloriesRequired:")
  print(summedCreatures$caloriesRequired * summedCreatures$popSize)
  
  #MAKE MATRIX?
  ufl <- c()
  bs <- c()
  ll <- c()
  for(i in 1:numCreatures) {
    
    creature <- summedCreatures[i,]
    growthVal <- 0
    
    underfedLosses <- 0
    babiesSpawned <- 0
    babiesSurvived <- 0
    caloriesDifference <- caloriesGained[i] - creature$popSize * creature$caloriesRequired
    
    print("CD:")
    print(caloriesDifference)
    
    if(caloriesDifference < 0) {
        underfedLosses <- (-1 * caloriesDifference) / creature$caloriesRequired
    }
    else {
      babiesSpawned <- caloriesDifference / creature$babyCalories
      maxBabies <- creature$popSize * creature$maxBabies
      if(babiesSpawned > maxBabies) {
        babiesSpawned <- maxBabies 
      }
      babiesSurvived <- babiesSpawned * creature$babySurviveChance / 100
    }
    
    ufl <- c(ufl, underfedLosses)
    bs <- c(bs, babiesSpawned)
    
    lifeLosses <- 0
    eatenLosses <- numEaten[i]
    lifeExpectancyLosses <- creature$popSize / creature$lifeExpectancy
    if(eatenLosses > lifeExpectancyLosses) {
      lifeLosses <- eatenLosses
    }
    else {
      lifeLosses <- lifeExpectancyLosses
    }
    
    ll <- c(ll, lifeLosses)
    
    if(creature$grass >= 1) {
      growthVal <- creature$popSize * 0.09 - numEaten[i]
    }
    else {
      growthVal <- babiesSurvived - lifeLosses - underfedLosses
    }
    growth <- c(growth, growthVal)
  
  }
  
  
  #USUALLY NOT COMMENTED OUT
  
  print("UnderfedLosses, BabiesSpawned, LifeLosses | GROWTH:")
  print(ufl)
  print(bs)
  print(ll)
  print(growth)
  
  #print("-------")
  
  return(growth)
}

computecatchesPerStep <- function(summedCreatures) {
  #apply evos here
  numCreatures <- nrow(summedCreatures)
  catchesPerStepMatrix <- matrix(summedCreatures$catchesPerStep, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  return(catchesPerStepMatrix)
}

computeChanceToBeSeen <- function(summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  chanceToBeSpottedMatrix <- computeChanceToBeSpotted(summedCreatures)
  chanceToBeNearMatrix <- computeChanceToBeNear(summedCreatures)
  
  print("ChanceToBeNearMtrx:")
  print(chanceToBeNearMatrix)
  print("ChaceToBeSeenMatrix:")
  print(chanceToBeSpottedMatrix)
  
  chanceMatrix <- chanceToBeNearMatrix * chanceToBeSpottedMatrix
  
  print("SeenMatrixRow:")
  print(chanceMatrix[1,])
  
  return(chanceMatrix)
}

computeChanceToBeSpotted <- function(summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  chanceToBeSpottedMatrix <- matrix(summedCreatures$seenChance, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  return(chanceToBeSpottedMatrix)
}

computeChanceToBeNear <- function(summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  chanceToBeNearMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      prob <- pbinom(0, size = summedCreatures[j,]$popSize, prob = 0.000001 * summedCreatures[i,]$chanceToBeNearMultiplier, lower.tail = FALSE)
      chanceToBeNearMatrix[i,j] <- prob
    }
  }
  return(chanceToBeNearMatrix)
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

computeValues <- function(summedCreatures) {
  
  #apply evos here
  #this should account for TIME TO CATCH
  
  numCreatures <- nrow(summedCreatures)
  valueMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      value <- summedCreatures[j,]$calories / summedCreatures[j,]$catchesPerStep
      if(i == j) {
        value <- 0
      }
      if(summedCreatures[i,]$predator == 0) {
        if(summedCreatures[j,]$grass == 0) {
          value <- 0
        }
      }
      if(summedCreatures[i,]$predator >= 1) {
        if(summedCreatures[j,]$grass >= 1) {
          value <- 0
        }
      }
      if(summedCreatures[i,]$grass >= 1) {
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
  
  #PROBLEM?
  
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

stepSim(20,1,2,creatures)
"============================================================================================="
populationSizes
matplot(populationSizes, type = "l", log = "y")
