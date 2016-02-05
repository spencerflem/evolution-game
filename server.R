library(shiny)
library(shinyjs)
library(plyr)
library(Rgraphviz)


submittedIDs <- c()


addEvolution <- function(ID, evolution) {
  
  popSizes <- populationSizes
  if(ID %in% submittedIDs) {
    print("NOPE")
  }
  else {
    strID <- toString(ID)
    items <- strsplit(evolution, ",")
    amounts <- strsplit(unlist(items), "#")
    dfAmounts <- as.data.frame(amounts, stringsAsFactors = FALSE)
    for(change in dfAmounts) {
      stat <- creatures[strID,][[change[2]]]
      value <- as.numeric(change[1])
      stat <- stat + value
      creatures[strID,][[change[2]]] <<- stat
    }
    submittedIDs <<- c(submittedIDs, ID)
    numNotSubmitted <- getNumNotSubmitted()
    if(numNotSubmitted == 0) {
      submittedIDs <<- c()
      popSizes <- stepSim(2000, .1, creatures)
    }
  }
  return(popSizes)
}

getNumNotSubmitted <- function() {
  count <- 0
  for(ID in sessions) {
    if(ID %in% submittedIDs) {
    }
    else {
      count <- count + 1
    }
  }
  return(count)
}

grass <- data.frame(population = 20000, size = 0, speed = 0, def = 0, camo = 0, sight = 0, spot = 0, plant = 0, meat = 0, flight = -5, pack = 0, grass = TRUE)
baseCreature <- data.frame(population = 300, size = 0, speed = 0, def = 0, camo = 0, sight = 0, spot = 0, plant = 0, meat = 0, flight = -5, pack = 0, grass = FALSE)
#MORE EXCEPTIONS FOR GRASS!

creatures <- cbind(name = 'grass', grass)
rownames(creatures) <- '-1'

updateView <- function() {
  toggle("creature")
  toggle("selectedEvo")
  toggle("confirmed")
  hide("class")
  hide("race")
  hide("name")
  hide("joined")
  hide("heading")
}

populationSizes <- c()

addCreature <- function(ID, class, race, name) {
  newCreature <- cbind(name = name, baseCreature)
  
  if(class == "carnivore") {
    newCreature$population <- newCreature$population - 50
    newCreature$pack <- newCreature$pack + 2
    newCreature$sight <- newCreature$sight + 1
    newCreature$spot <- newCreature$spot + 1
    newCreature$meat <- newCreature$meat + 6
    newCreature$plant <- newCreature$plant - 6
  }
  if(class == "herbivore") {
    newCreature$population <- newCreature$population + 75
    newCreature$camo <- newCreature$camo + 1
    newCreature$def <- newCreature$def + 1
    newCreature$plant <- newCreature$plant + 6
    newCreature$meat <- newCreature$meat - 6
  }
  
  if(race == "big") {
    newCreature$population <- newCreature$population - 100
    newCreature$speed <- newCreature$speed - 2
  }
  if(race == "small") {
    newCreature$population <- newCreature$population + 100
    newCreature$speed <- newCreature$speed + 2
  }

  strID <- toString(ID)
  creatureRowNames <- rownames(creatures)
  creatures <<- rbind(creatures, newCreature)
  creatureRowNames <- c(creatureRowNames, strID)
  rownames(creatures) <<- creatureRowNames
  
  populationSizes <<- creatures$population
  populationSizes <<- rbind(populationSizes, populationSizes)
  
  numCreatures <- nrow(creatures)
  eatenMatrix <<- matrix(0, nrow = numCreatures, ncol = numCreatures)
  
  #FOR RESULTS SECTION:
  lifeLossesMatrix <<- matrix(0, ncol = numCreatures)
  babiesSpawnedMatrix <<- matrix(0, ncol = numCreatures)
  underfedLossesMatrix <<- matrix(0, ncol = numCreatures)
  growthMatrix <<- matrix(0, ncol = numCreatures)
  
  
  updateView()
}

sessions <- c()

generateID <- function() {
  return(floor(runif(1,0,100000)))
}

addSession <- function() {
  newSession <- generateID()
  while(newSession %in% sessions) {
    newSession <- generateID()
  }
  sessions <<- c(sessions, newSession)
  return(newSession)
}

#########SIM BEGINS HERE

#setup
numCreatures <- nrow(creatures)
eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)

#FOR RESULTS SECTION:
lifeLossesMatrix <- matrix(0, ncol = numCreatures)
babiesSpawnedMatrix <- matrix(0, ncol = numCreatures)
underfedLossesMatrix <- matrix(0, ncol = numCreatures)
growthMatrix <- matrix(0, ncol = numCreatures)


graphResults <- function() {
  
  folder <- paste0("C://Users//me//Desktop//", folderName)
  
  popSizeFile <- paste0(folder, "//populations.svg")
  babiesSpawnedFile <- paste0(folder, "//babies.svg")
  underfedLossesFile <- paste0(folder, "//underfed.svg")
  growthFile <- paste0(folder, "//growth.svg")
  lifeLossesFile <- paste0(folder, "//lifelosses.svg")
  
  svg(file=popSizeFile)
    matplot(populationSizes[-1,], type = "l", log = "y")
  dev.off()
  
  svg(file=babiesSpawnedFile)
    matplot(babiesSpawnedMatrix[-1,], type = "l")
  dev.off()
  
  svg(file=underfedLossesFile)
    matplot(underfedLossesMatrix[-1,], type = "l")
  dev.off()
  
  svg(file=growthFile)
    matplot(growthMatrix[-1,], type = "l")
  dev.off()
  
  svg(file=lifeLossesFile)
    matplot(lifeLossesMatrix[-1,], type = "l")
  dev.off()
}




stepSim  <- function(times, stepSize, creatures) {
  
  creaturesCopy <- creatures
  
  #print("---------------------------------------------------")
  #print(creaturesCopy$population) 
  #print("---------------------------------------------------")
  
  for(i in 1:times) {
    creaturesCopy$population <- subStep(stepSize, creaturesCopy)
    print(i)
    #print("---------------------------------------------------")
    #print(creaturesCopy$population)
    #print("---------------------------------------------------")
    populationSizes <<- rbind(populationSizes,creaturesCopy$population)
    
  }
  
  creatures$population <<- creaturesCopy$population
  
  graphResults()
  
  return(populationSizes)
}

subStep <- function(stepSize, creaturesCopy) {
  growthRow <- calculateGrowthRow(creaturesCopy)
  populationsOriginal <- creaturesCopy$population
  populationsRow <- populationsOriginal + growthRow * stepSize
  populationsRow <- sapply(populationsRow, function(x){
    if(x < 0) {
      return(0)
    }
    else {
      return(x)
    }
  })
  populationsRow <- sapply(populationsRow, function(x){floor(x)})
  return(populationsRow)
}

calculateNumEatenMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  eatenMatrix <<- calculateEatenMatrixFast(creaturesCopy)
  catchesPerStepMatrix <- computecatchesPerStepMatrix(creaturesCopy)
  populations <- creaturesCopy$population
  popMatrix <- matrix(populations, nrow = numCreatures, ncol = numCreatures)
  numEatenMatrix <- eatenMatrix * catchesPerStepMatrix * popMatrix
}

calculateGrowthRow <- function(creaturesCopy) {
  
  numEatenMatrix <- calculateNumEatenMatrix(creaturesCopy)
  
  numCreatures <- nrow(creaturesCopy)
  growth <- c()
  
  numEaten <- colSums(numEatenMatrix)
  
  caloriesMatrix<- computeCaloriesMatrix(creaturesCopy)
  
  caloriesEatenMatrix <- numEatenMatrix * caloriesMatrix
  caloriesGained <- 2 * rowSums(caloriesEatenMatrix)
  
  ufl <- c()
  bs <- c()
  ll <- c()
  
  caloriesRequiredRow <- computeCaloriesRequiredRow(creaturesCopy)
  babyCaloriesRow <- computeBabyCaloriesRow(creaturesCopy)
  maxBabiesRow <- computeMaxBabiesRow(creaturesCopy)
  lifeExpectancyRow <- computeLifeExpectancyRow(creaturesCopy)
  
  for(i in 1:numCreatures) {
    
    creature <- creaturesCopy[i,]
    growthVal <- 0
    
    underfedLosses <- 0
    babiesSpawned <- 0
    
    caloriesDifference <- caloriesGained[i] - creature$population * caloriesRequiredRow[i]
    
    if(caloriesDifference < 0) {
      underfedLosses <- (-1 * caloriesDifference) / caloriesRequiredRow[i]
    }
    else {
      babiesSpawned <- caloriesDifference / babyCaloriesRow[i]
      maxBabies <- creature$population * maxBabiesRow[i]
      if(babiesSpawned > maxBabies) {
        babiesSpawned <- maxBabies 
      }
    }
    
    ufl <- c(ufl, underfedLosses)
    bs <- c(bs, babiesSpawned)
    
    lifeLosses <- 0
    eatenLosses <- numEaten[i]
    
    lifeExpectancyLosses <- creature$population / lifeExpectancyRow[i]
    
    if(eatenLosses > lifeExpectancyLosses) {
      lifeLosses <- eatenLosses
    }
    else {
      lifeLosses <- lifeExpectancyLosses
    }
    
    ll <- c(ll, lifeLosses)
    
    if(creature$grass >= 1) {
      growthVal <- creature$population * 0.09 - numEaten[i]
    }
    else {
      growthVal <- babiesSpawned - lifeLosses - underfedLosses
    }
    growth <- c(growth, growthVal)
    
  }
  
  #print("UnderfedLosses, BabiesSpawned, LifeLosses | GROWTH:")
  #print(ufl)
  #print(bs)
  #print(ll)
  #print(growth)
  
  underfedLossesMatrix <<- rbind(underfedLossesMatrix, ufl)
  babiesSpawnedMatrix <<- rbind(babiesSpawnedMatrix, bs)
  lifeLossesMatrix <<- rbind(lifeLossesMatrix, ll)
  growthMatrix <<- rbind(growthMatrix, growth)
  
  #print("-------")
  
  return(growth)
}

calculateChanceToBeSeenMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  chanceToBeSpottedMatrix <- computeChanceToBeSpottedMatrix(creaturesCopy)
  chanceToBeNearMatrix <- calculateChanceToBeNearMatrix(creaturesCopy)
  chanceMatrix <- chanceToBeNearMatrix * chanceToBeSpottedMatrix
  return(chanceMatrix)
}

calculateChanceToBeNearMatrix <- function(creaturesCopy) {
  nearChanceCenterRow <- computeNearChanceCenterRow(creaturesCopy)
  nearChanceSlopeRow <- computeNearChanceSlopeRow(creaturesCopy)
  numCreatures <- nrow(creaturesCopy)
  chanceToBeNearMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      prob <- (1/(1+exp(-(creaturesCopy[j,]$population-nearChanceCenterRow[j])/nearChanceSlopeRow[j])))
      chanceToBeNearMatrix[i,j] <- prob
    }
  }
  return(chanceToBeNearMatrix)
}

calculateEatenMatrixFast <- function(creaturesCopy) {
  valuesMatrix <- calculateValuesMatrix(creaturesCopy)
  chanceMatrix <- calculateChanceToBeSeenMatrix(creaturesCopy)
  numCreatures <- nrow(creaturesCopy)
  eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      prob <- 0
      value <- valuesMatrix[i,j]
      if(value > 0) {
        prob <- (chanceMatrix[i,j]/100)
        for(k in 1:numCreatures) {
          if(valuesMatrix[i,k] > value) {
            prob <- prob * (1 - (chanceMatrix[i,k]/100) )
          }
        }
      }
      eatenMatrix[i,j] = prob
    }
  }
  return(eatenMatrix)
}

sig <- function(x) {
  y <- ((2/(1+exp(-(x/2.3))))-1)
  return(y)
}

xpo <- function(x) {
  y <- 1.13^(x)
  return(y)
}

computeChanceToBeSpottedMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  chanceToBeSpottedMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      baseChance <- 50
      sizeMod <- 10 * sig(creaturesCopy[j,]$size)
      sightMod <- 20 * sig(creaturesCopy[i,]$sight)
      effectiveCamo <- creaturesCopy[j,]$camo - creaturesCopy[i,]$spot
      camoMod <- -1 * 20 * sig(effectiveCamo)
      spotChance <- baseChance + sizeMod + sightMod + camoMod
      chanceToBeSpottedMatrix[i,j] <- spotChance #/100?
    }
  }
  return(chanceToBeSpottedMatrix)
}

computecatchesPerStepMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  catchesPerStepMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      adjustedCatchChance <- 0
      if(creaturesCopy[j,]$grass) {
        adjustedCatchChance <- 1
      }
      else {
        baseChance <- 0
        effectiveSize <- creaturesCopy[i,]$size - creaturesCopy[j,]$size + creaturesCopy[i,]$pack
        sizeMod <- 10 * sig(creaturesCopy[j,]$size)
        packMod <- -2 * sig(creaturesCopy[i,]$pack)
        effectiveFlight <- creaturesCopy[i,]$flight - creaturesCopy[j,]$flight
        flightMod <- 5 * sig(effectiveFlight)
        effectiveSpeed <- creaturesCopy[i,]$speed - creaturesCopy[j,]$speed
        speedMod <- 5 * sig(effectiveSpeed)
        defMod <- -1 * 1 * sig(creaturesCopy[j,]$def)
        catchChance <- baseChance + sizeMod + flightMod + speedMod + defMod + packMod
        adjustedCatchChance <-  xpo(catchChance)
      }
      catchesPerStepMatrix[i,j] <- adjustedCatchChance
    }
  }
  return(catchesPerStepMatrix)
}

computeNearChanceCenterRow <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  nearChancesCenterRow <- c()
  for(i in 1:numCreatures) {
    center <- populationSizes[1,i] * .9
    nearChancesCenterRow <- c(nearChancesCenterRow, center)
  }
  return(nearChancesCenterRow)
}

computeNearChanceSlopeRow <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  nearChancesSlopeRow <- c()
  for(i in 1:numCreatures) {
    slope <- populationSizes[1,i] * .1
    nearChancesSlopeRow <- c(nearChancesSlopeRow, slope)
  }
  return(nearChancesSlopeRow)
}

computeLifeExpectancyRow <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  lifeExpectancyRow <- c()
  for(i in 1:numCreatures) {
    baseExpectancy <- 20
    sizeMod <- 15 * sig(creaturesCopy[i,]$size)
    lifeExpectancy <- baseExpectancy + sizeMod
    lifeExpectancyRow <- c(lifeExpectancyRow, lifeExpectancy)
  }
  return(lifeExpectancyRow)
}

computeMaxBabiesRow <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  maxBabiesRow <- c()
  for(i in 1:numCreatures) {
    baseMax <- 5
    sizeMod <- 3 * sig(creaturesCopy[i,]$size)
    maxBabies <- baseMax + sizeMod
    maxBabiesRow <- c(maxBabiesRow, maxBabies)
  }
  return(maxBabiesRow)
}

computeBabyCaloriesRow <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  babyCaloriesRow <- c()
  caloriesRequiredRow <- computeCaloriesRequiredRow(creaturesCopy)
  for(i in 1:numCreatures) {
    babyCalories <- 0.6 * caloriesRequiredRow[i]
    babyCaloriesRow <- c(babyCaloriesRow, babyCalories)
  }
  return(babyCaloriesRow)
}

computeCaloriesMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  caloriesRequiredRow <- computeCaloriesRequiredRow(creaturesCopy)
  caloriesMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      baseMultiplier <- 1
      if(creaturesCopy[j,]$grass == TRUE) {
        typeMod <- 1 * sig(creaturesCopy[i,]$plant)
      }
      else {
        typeMod <- 1 * sig(creaturesCopy[i,]$meat)
      }
      caloriesMultiplier <- baseMultiplier + typeMod
      calories <- caloriesRequiredRow[j] * caloriesMultiplier
      caloriesMatrix[i,j] <- calories
    }
  }
  return(caloriesMatrix)
}

calculateValuesMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  valueMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  
  catchesPerStepMatrix <- computecatchesPerStepMatrix(creaturesCopy)
  caloriesMatrix <- computeCaloriesMatrix(creaturesCopy)
  
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      value <- caloriesMatrix[i,j] * catchesPerStepMatrix[i,j]
      if(i == j) {
        value <- 0
      }
      if(creaturesCopy[i,]$grass >= 1) {
        value <- -1
      }
      valueMatrix[i,j] <- value
    }
  }
  return(valueMatrix)
}

computeCaloriesRequiredRow <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  caloriesRequiredRow <- c()
  for(i in 1:numCreatures) {
    multiplier <- xpo(creaturesCopy[i,]$size)
    speedCals <- 40 * xpo(creaturesCopy[i,]$speed)
    flightCals <- 40 * xpo(creaturesCopy[i,]$flight) * (sig(creaturesCopy[i,]$size) + 1)
    sightCals <- 20 * xpo(creaturesCopy[i,]$sight)
    spotCals <- 20 * xpo(creaturesCopy[i,]$spot)
    caloriesRequired <- multiplier * (speedCals + flightCals + sightCals + spotCals)
    caloriesRequiredRow <- c(caloriesRequiredRow, caloriesRequired)
  }
  return(caloriesRequiredRow)
}

############ SIM ENDS HERE

shinyServer(function(input, output, session) {
  
  updateNumericInput(session, "ID", "realID", value = addSession())
  
  disable("confirmed")
  hide("creature")
  hide("selectedEvo")
  hide("confirmed")
  hide("ID")
  
  output$populations <- renderPlot({
    matplot(creaturesReact$popSizes, type = "l", log = "y")
  })
  
  #only popsizes is reactive TODO Priority 6
  
  creaturesReact <- reactiveValues()
  creaturesReact$popSizes <- c(0,6,7)
  
  observeEvent(input$selectedEvo, {enable("confirmed")})
  
  observeEvent(input$confirmed, {
    creaturesReact$popSizes <- addEvolution(input$ID, input$selectedEvo)
  }, ignoreNULL = TRUE) #used to be false...
  
  observeEvent(input$joined, {
    addCreature(input$ID, input$class, input$race, input$name)})
  
  output$evos <- renderMenu({
    sidebarMenu(
      menuItem("body",tabName = "body",
               menuSubItem("larger (+1 size)",tabName = "+1#size"),
               menuSubItem("smaller (-1 size)",tabName = "-1#size")),
      menuItem("legs",tabName = "legs",
               menuSubItem("speedier (+1 speed)",tabName = "+1#speed"),
               menuSubItem("more efficent (-1 speed)",tabName = "-1#speed")),
      menuItem("hide",tabName = "hide",
               menuSubItem("defensive (+1 def, -1 camo)",tabName = "+1#def,-1#camo"),
               menuSubItem("camouflaged (+1 camo -1 def)", tabName = "+1#camo,-1#def")),
      menuItem("eyes",tabName = "eyes",
               menuSubItem("longer ranged (+2 sight)",tabName = "+2#sight"),
               menuSubItem("camo spotting (+2 spot)",tabName = "+2#spot"),
               menuSubItem("more efficent (-1 sight, -1 spot)",tabName="-1#sight,-1#spot")),
      menuItem("stomach", tabName = "stomach",
               menuSubItem("digests plant (+1 plant, -1 meat)", tabName = "+1#plant,-1#meat"),
               menuSubItem("digests meat (+1 meat, -1 plant)", tabName = "+1#meat,-1#plant")),
      menuItem("wings", tabName = "wings",
               menuSubItem("more winged (+1 flight)", tabName = "+1#flight"), #includes word wings
               menuSubItem("less winged (-1 flight)", tabName = "-1#flight")),
      menuItem("behavior", tabName = "behavior",
               menuSubItem("hunts in a pack (+1 pack)", tabName = "+1#pack"), #better working and better stat?
               menuSubItem("hunts independantly (-1 pack)", tabName = "-1#pack"))
    )})
  
})

#folderName <- "folder to store graphs"
