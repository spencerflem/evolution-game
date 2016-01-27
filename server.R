library(shiny)
library(shinyjs)
library(plyr)

#if you die, new creature comes back in corectly
#creature joining doesnt creash game TODO: priority 9

submittedIDs <- c()

#HOW TO GET CATEGORY FROM EVOLUTION

addEvolution <- function(ID, evolution) {
  
  print("EVO")
  print(evolution)
  
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
      popSizes <- stepSim(1000, 1, .1, creatures)
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

grass <- data.frame(name = "grass", popSize = 20000, calories = 25, catchesPerStep = 2, seenChance = 95, predator = FALSE, grass = TRUE, caloriesRequired = -1, lifeExpectancy = -1, babyCalories = -1, maxBabies = -1, babySurviveChance = -1, nearChanceCenter = 18000, nearChanceSlope = 2000)

pred1 <- data.frame(name = "predator", popSize = 100, calories = 800, catchesPerStep = 0.4, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 120, maxBabies = 2, babySurviveChance = 70, nearChanceCenter = 600, nearChanceSlope = 1000)
pred2 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.6, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 150, lifeExpectancy = 25, babyCalories = 110, maxBabies = 1, babySurviveChance = 85, nearChanceCenter = 600, nearChanceSlope = 1000)
pred3 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.4, seenChance = 10, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 85, maxBabies = 3, babySurviveChance = 75, nearChanceCenter = 600, nearChanceSlope = 1000)

prey1 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 0.7, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 30, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 40, nearChanceCenter = 310, nearChanceSlope = 35)
prey2 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 1.8, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 55, lifeExpectancy = 8, babyCalories = 15, maxBabies = 8, babySurviveChance = 30, nearChanceCenter = 310, nearChanceSlope = 35)
prey3 <- data.frame(name = "prey", popSize = 350, calories = 200, catchesPerStep = 0.8, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 40, lifeExpectancy = 10, babyCalories = 10, maxBabies = 12, babySurviveChance = 40, nearChanceCenter = 310, nearChanceSlope = 35)
prey4 <- data.frame(name = "prey", popSize = 350, calories = 100, catchesPerStep = 1.0, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 12, babyCalories = 12, maxBabies = 14, babySurviveChance = 35, nearChanceCenter = 310, nearChanceSlope = 35)
prey5 <- data.frame(name = "prey", popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, nearChanceCenter = 310, nearChanceSlope = 35)

grass <- data.frame(population = 20000, size = 0, speed = 0, def = 0, camo = 0, sight = 0, spot = 0, plant = 0, meat = 0, flight = -5, pack = 0, grass = TRUE)
baseCreature <- data.frame(population = 200, size = 0, speed = 0, def = 0, camo = 0, sight = 0, spot = 0, plant = 0, meat = 0, flight = -5, pack = 0, grass = FALSE)
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

addCreature <- function(ID, class, race, name) {
  newCreature <- cbind(name = name, baseCreature) #TODO: VERY IMPORTANT Flesh this line out. (priority 6)
  strID <- toString(ID)
  print("NC")
  print(newCreature)
  creatureRowNames <- rownames(creatures)
  creatures <<- rbind(creatures, newCreature)
  creatureRowNames <- c(creatureRowNames, strID)
  rownames(creatures) <<- creatureRowNames
  
  populationSizes <- creatures$population
  populationSizes <- rbind(populationSizes, populationSizes)
  
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

#First Time Setup
numCreatures <- nrow(creatures)
eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)

stepSim  <- function(times, subTimes, stepSize, creatures) {
  
  creaturesCopy <- creatures
  
  print("---------------------------------------------------")
  print(creaturesCopy$population) 
  print("---------------------------------------------------")
  
  for(i in 1:times) {
    creaturesCopy$population <- subStep(subTimes, stepSize, creaturesCopy)
    print(i)
    print("---------------------------------------------------")
    print(creaturesCopy$population)
    print("---------------------------------------------------")
    populationSizes <<- rbind(populationSizes,creaturesCopy$population)
    
  }
  
  creatures$population <<- creaturesCopy$population
  
  return(populationSizes)
}

subStep <- function(subTimes, stepSize, creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  print("IN")
  eatenMatrix <<- calculateEatenMatrixFast(creaturesCopy)
  print("INNER")
  catchesPerStep <- computecatchesPerStepMatrix(creaturesCopy)
  print("INNERER")
  catchesPerStepMatrix <- matrix(catchesPerStep, nrow = numCreatures, ncol = numCreatures)
  print("INNERIST")
  populations <- creaturesCopy$population
  print("INNERISTIST")
  popMatrix <- matrix(populations, nrow = numCreatures, ncol = numCreatures)
  
  print("CatchesPerStepMtrx")
  print(catchesPerStepMatrix)
  print("PopulationMtrx:")
  print(popMatrix)
  
  for(i in 1:subTimes) {
    
    numEatenMatrix <- eatenMatrix * catchesPerStepMatrix * popMatrix
    
    print("EatenMtrx:")
    print(eatenMatrix)
    print("CatchesPerStep")
    print(catchesPerStep)
    print("NumEatenMtrx:")
    print(numEatenMatrix)
    
    growth <- calculateGrowthRow(creaturesCopy, numEatenMatrix)
    
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

calculateGrowthRow <- function(creaturesCopy, numEatenMatrix) {
  numCreatures <- nrow(creaturesCopy)
  growth <- c()
  
  numEaten <- colSums(numEatenMatrix)
  
  caloriesMatrix<- computeCaloriesMatrix(creaturesCopy)
  
  caloriesEatenMatrix <- numEatenMatrix * caloriesMatrix
  caloriesGained <- 2 * rowSums(caloriesEatenMatrix)
  
  print("CaloriesMtrx:")
  print(caloriesMatrix)
  print("CaloriesEatenMtrx:")
  print(caloriesEatenMatrix)
  print("CaloriesGained:")
  print(caloriesGained)
  
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
    
    print("CD:")
    print(caloriesDifference)
    
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
    
    print("EL?LL")
    print(eatenLosses)
    print(lifeExpectancyLosses)
    
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
  
  
  #USUALLY NOT COMMENTED OUT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  print("UnderfedLosses, BabiesSpawned, LifeLosses | GROWTH:")
  print(ufl)
  print(bs)
  print(ll)
  print(growth)
  
  print("-------")
  
  return(growth)
}

calculateChanceToBeSeenMatrix <- function(creaturesCopy) {
  numCreatures <- nrow(creaturesCopy)
  print("CTBS1")
  chanceToBeSpottedMatrix <- computeChanceToBeSpottedMatrix(creaturesCopy)
  print("CTBS2")
  chanceToBeNearMatrix <- calculateChanceToBeNearMatrix(creaturesCopy)
  print("CTBS3")
  chanceMatrix <- chanceToBeNearMatrix * chanceToBeSpottedMatrix
  
  print("ChanceToBeNearMtrx:")
  print(chanceToBeNearMatrix)
  print("ChaceToBeSeenMatrix:")
  print(chanceToBeSpottedMatrix)
  print("SeenMatrixRow:")
  print(chanceMatrix[1,])
  
  return(chanceMatrix)
}

calculateChanceToBeNearMatrix <- function(creaturesCopy) {
  
  nearChanceCenterRow <- computeNearChanceCenterRow(creaturesCopy)
  print("OK")
  nearChanceSlopeRow <- computeNearChanceSlopeRow(creaturesCopy)
  print("KO")
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
  print("INA")
  valuesMatrix <- calculateValuesMatrix(creaturesCopy)
  print("IOI")
  chanceMatrix <- calculateChanceToBeSeenMatrix(creaturesCopy)
  print("OPL")
  numCreatures <- nrow(creaturesCopy)
  print("SDE")
  eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  print("TUA")
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      prob <- 0
      value <- valuesMatrix[i,j]
      print("TREEA")
      if(value > 0) {
        prob <- (chanceMatrix[i,j]/100)
        for(k in 1:numCreatures) {
          if(valuesMatrix[i,k] > value) {
            prob <- prob * (1 - (chanceMatrix[i,k]/100) )
          }
        }
      }
      print("FURA")
      eatenMatrix[i,j] = prob
    }
  }
  return(eatenMatrix)
}

sig <- function(x) {
  y <- ((2/(1+exp(-(i/2.3))))-1)
  return(y)
}

xpo <- function(x) {
  y <- 1.13^(x)
  return(y)
}

#untested
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
  #IS IT BAD THAT THIS IS CHANGABLE?
  numCreatures <- nrow(creaturesCopy)
  nearChancesCenterRow <- c()
  for(i in 1:numCreatures) {
    print(populationSizes)
    center <- populationSizes[1,i] * .9
    nearChancesCenterRow <- c(nearChancesCenterRow, center)
  }
  return(nearChancesCenterRow)
}

computeNearChanceSlopeRow <- function(creaturesCopy) {
  #IS IT BAD THAT THIS IS CHANGABLE? YES IT IS!
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
      baseMultiplier <- 3
      if(creaturesCopy[j,]$grass == TRUE) {
        typeMod <- 3 * sig(creaturesCopy[i,]$plant)
      }
      else {
        typeMod <- 3 * sig(creaturesCopy[i,]$meat)
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
      value <- caloriesMatrix[i,j] / catchesPerStepMatrix[i,j]
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
    flightCals <- 40 * xpo(creaturesCopy[i,]$flight)
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
  
  output$foodWeb <- renderPlot({
    foodWeb<-new("graphAM", adjMat=eatenMatrix, edgemode="directed")
    plot(foodWeb)
  })
  
  output$populations <- renderPlot({
    matplot(creaturesReact$popSizes, type = "l", log = "y")
  })
  
  #only popsizes is reactive TODO Priority 6
  
  creaturesReact <- reactiveValues()
  creaturesReact$popSizes <- c(0,6,7)
  
  output$yourCreature <- renderText(testText$test) #TODO: priority 7 (only in sidebar? advanced stats)
  
  output$otherCreatures <- renderDataTable(creatures) #TODO priority 8 (view basic stats of all creaures)
  
  observeEvent(input$selectedEvo, {enable("confirmed")})
  
  observeEvent(input$confirmed, {
    creaturesReact$popSizes <- addEvolution(input$ID, input$selectedEvo)
  }, ignoreNULL = TRUE) #used to be false...
  
  observeEvent(input$joined, {
    addCreature(input$ID, input$class, input$race, input$name)})
  
  #nut cracking means TREES!!!
  #HERD MENTALITY? <- downside?
  #MORE ENDURING LEGS?
  #SHOW OFF POISONNESSNESS ALA BRIGHT SNAKES
  #MIND/BRAIN EVOS?
  #BURROWING?
  #MOUTH
  #BABYMAKING !
  #Scavenger (eats those died to starve/attrition)?
  
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