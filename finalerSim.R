library(plyr)

baseCreature <- data.frame(popsize = 100, quality = 800, timeToCatch = 5, seenchance = 50, predator = FALSE, grass = FALSE)
grass <- data.frame(name = "grass", popsize = 2000, quality = 50, timeToCatch = 1, seenchance = 95, predator = FALSE, grass = TRUE)

creatures <- list('-1' = list(grass, grass))

#HOW TO APPEND:
creatures <- c(creatures, list('999' = list(grass, grass)))

#HOW TO ADD EVOLUTION:
creatures$'999' <- c(creatures$'999', list(grass))

#HOW TO CONVERT TO SUMMED FORM:
filledFrame <- data.frame()
for(frame in creatures$'999') {
  filledFrame <- rbind.fill(filledFrame, frame)
}
summedCreature <- colSums(filledFrame[,-1])

summedCreatures <- function(creatures) {
  summedCreatures <- list()
  for (creature in creatures) {
    filledFrame <- data.frame()
    for(frame in creature) {
      filledFrame <- rbind.fill(filledFrame, frame)
    }
    summedCreature <- colSums(filledFrame[,-1])
    summedCreatures <- list(summedCreatures, summedCreature)
  }
  print("OUT!")
  return(summedCreatures)
}

summedCreatures(creatures)

stepSim  <- function(times, subTimes, creatures) {
  compressedCreatures <- ldply(creatures, data.frame) #LDPLY DOESNT WORK
  populations <- compressedCreatures$popsize
  print(populations)
  print("---")
  for(i in 1:times) {
    eatenChances <- calculateEatenChance(compressedCreatures)
    populations <- subStep(subTimes, eatenChances, populations)
  }
  print(populations)
}

subStep <- function(times, eatenChances, creatures) {
  numCreatures <- nrow(creatures)
  compressedCreatures <- ldply(creatures, data.frame) #DOESNT WORK
  print("hihi6")
  eatenMatrix <- calculateEatenMatrix(creatures)
  timeToCatch <- calculateTimeToCatch
  timeShards <- eatenMatrix * timeToCatch
  totalCatchTimes <- rowSums(timeShards)
  catchesPerStep <- sapply(totalCatchTimes, function(x){return(1/x)})
  populations <- compressedCreatures$popsize
  for(i in 1:times) {
    numEatenMatrix <- eatenMatrix * catchesPerStep * populations #MAY BE MORE DELICATE THAN JUST THIS!!!!!
    numEaten <- colSums(numEatenMatrix)
    populations <- populations + numEaten
  }
  # <<- update creatures
}

#NEEDS METHOD OF GROWTH!!! - BASED ON CALORIES NEEDED?

computeTimeToCatch <- function(creatures) {
  #apply evos here
  compressedCreatures <- ldply(creatures, data.frame) #DOESNT WORK
  return(compressedCreatures$timeToCatch)
}

computeChanceToBeSeen <- function(creatures) {
  #apply evos here
  compressedCreatures <- ldply(creatures, data.frame) #DOESNT WORK
  return(compressedCreatures$seenchance)
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

calculateEatenMatrix <- function(creatures) {
  print("aadadassdsads")
  numCreatures <- nrow(creatures)
  chanceToBeSeen <- computeChanceToBeSeen(creatures)
  values <- computeValues(creatures)
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