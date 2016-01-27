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
      stat <- creatures[[strID]][[change[2]]]
      value <- as.numeric(change[1])
      stat <- stat + value
      creatures[[strID]][[change[2]]] <<- stat
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

baseCreature <- data.frame(population = 300, size = 5, speed = 5, def = 5, camo = 5, sight = 5, spot = 5, plant = 5, meat = 5, resist = 5, poison = 5, flight = 5, pack = 5, grass = FALSE)

creatures <- list('-1' = list(baseCreature))
#soon to be data.frame TODO: PRIORITY 1

#old style
#baseCreature <- data.frame(popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, nearChanceCenter = 310, nearChanceSlope = 35)

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
  newCreature <- data.frame(name = name, baseCreature) #TODO: VERY IMPORTANT Flesh this line out. (priority 6)
  strID <- toString(ID)
  creatures[[strID]] <<- newCreature
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
summedCreatures <- sumCreatures(creatures)
numCreatures <- nrow(summedCreatures)
populationSizes <- sumCreatures(creatures)$popSize
eatenMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)

sumCreatures <- function(creatures) {
  summedCreatures <- data.frame()
  for (creature in creatures) {
    print("creature:")
    print(creature)
    filledFrame <- data.frame()
    #NAME AND ID?
    for(frame in creature) {
      filledFrame <- rbind.fill(filledFrame, frame)
    }
    print("filled frame:")
    print(filledFrame)
    #name <- filledFrame[] #first element, name
    
    
    filledFrame <- filledFrame[,-1]
    
    #filledFrame$name <- NA ???
    #filledFrame$category <- NA ???
    #why bother?
    
    colSums <- colSums(filledFrame)
    filledFrame["Totals",] <- colSums(filledFrame, na.rm = TRUE)
    print("inc totals:")
    print(filledFrame)
    summedCreatures <- rbind.fill(summedCreatures, tail(filledFrame, 1))
  }
  return(summedCreatures)
}

stepSim  <- function(times, subTimes, stepSize, creatures) {
  summedCreatures <- sumCreatures(creatures)
  
  print(summedCreatures)
  print("---------------------------------------------------") #IMPORTANT!
  print(summedCreatures$popSize) #IMPORTANT!
  print("---------------------------------------------------") #IMPORTANT!
  
  #populationSizes <<- summedCreatures$popSize
  
  for(i in 1:times) {
    summedCreatures$popSize <- subStep(subTimes, stepSize, summedCreatures)
    print(i) #IMPORTANT
    print("---------------------------------------------------") #IMPORTANT!
    print(summedCreatures$popSize) #IMPORTANT!
    print("---------------------------------------------------") #IMPORTANT!
    populationSizes <<- rbind(populationSizes,summedCreatures$popSize)
    
  }
  
  for(i in 1:numCreatures) {
    creatures[[i]][[1]]$popSize <<- summedCreatures[i,]$popSize
  }
  return(populationSizes)
}

subStep <- function(subTimes, stepSize, summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  eatenMatrix <<- calculateEatenMatrixFast(summedCreatures)
  catchesPerStep <- computecatchesPerStep(summedCreatures)
  catchesPerStepMatrix <- matrix(catchesPerStep, nrow = numCreatures, ncol = numCreatures)
  populations <- summedCreatures$popSize
  popMatrix <- matrix(populations, nrow = numCreatures, ncol = numCreatures)
  
  #print("CatchesPerStepMtrx")
  #print(catchesPerStepMatrix)
  #print("PopulationMtrx:")
  #print(popMatrix)
  
  for(i in 1:subTimes) {
    
    numEatenMatrix <- eatenMatrix * catchesPerStepMatrix * popMatrix
    
    #print("EatenMtrx:")
    #print(eatenMatrix)
    #print("CatchesPerStep")
    #print(catchesPerStep)
    #print("NumEatenMtrx:")
    #print(numEatenMatrix)
    
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
  caloriesGained <- 2 * rowSums(caloriesEatenMatrix)
  
  #print("CaloriesMtrx:")
  #print(caloriesMatrix)
  #print("CaloriesEatenMtrx:")
  #print(caloriesEatenMatrix)
  #print("CaloriesGained:")
  #print(caloriesGained)
  #print("CaloriesRequired:")
  #print(summedCreatures$caloriesRequired * summedCreatures$popSize)
  
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
    
    #print("CD:")
    #print(caloriesDifference)
    
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
  
  
  #USUALLY NOT COMMENTED OUT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #print("UnderfedLosses, BabiesSpawned, LifeLosses | GROWTH:")
  #print(ufl)
  #print(bs)
  #print(ll)
  #print(growth)
  
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
  chanceMatrix <- chanceToBeNearMatrix * chanceToBeSpottedMatrix
  
  #print("ChanceToBeNearMtrx:")
  #print(chanceToBeNearMatrix)
  #print("ChaceToBeSeenMatrix:")
  #print(chanceToBeSpottedMatrix)
  #print("SeenMatrixRow:")
  #print(chanceMatrix[1,])
  
  return(chanceMatrix)
}

computeChanceToBeSpotted <- function(summedCreatures) {
  #apply evos here
  numCreatures <- nrow(summedCreatures)
  chanceToBeSpottedMatrix <- matrix(summedCreatures$seenChance, nrow = numCreatures, ncol = numCreatures, byrow = TRUE)
  return(chanceToBeSpottedMatrix)
}

computeChanceToBeNear <- function(summedCreatures) {
  numCreatures <- nrow(summedCreatures)
  chanceToBeNearMatrix <- matrix(0, nrow = numCreatures, ncol = numCreatures)
  for(i in 1:numCreatures) {
    for(j in 1:numCreatures) {
      #OLD MODEL LISTED BELOW. CAUSED CREATURES TO COMMIT MURDER SUICIDE. DO NOT USE.
      #prob <- pbinom(0, size = summedCreatures[j,]$popSize, prob = 0.0001 * summedCreatures[i,]$chanceToBeNearMultiplier, lower.tail = FALSE)
      prob <- (1/(1+exp(-(summedCreatures[j,]$popSize-summedCreatures[j,]$nearChanceCenter)/summedCreatures[j,]$nearChanceSlope)))
      chanceToBeNearMatrix[i,j] <- prob
    }
  }
  return(chanceToBeNearMatrix)
}

calculateEatenMatrixFast <- function(summedCreatures) {
  valuesMatrix <- computeValues(summedCreatures)
  chanceMatrix <- computeChanceToBeSeen(summedCreatures)
  numCreatures <- nrow(summedCreatures)
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
  
  #TODO: MAKE GRAPHS REACTIVE!!!!!!!!!!!!!!!! <- SUPER DUPER IMPORTANT! TODO (priority 4)
  
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
  
  #TODO: ONLY SHOW SELECTABLE EVOS: USE allowedEvos(input$ID) somehow perhaps createMenu() TODO!: priority 3
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
               menuSubItem("digests plant (+1 plant, -1 else)", tabName = "+1#plant,-1#meat,-1#resist"),
               menuSubItem("digests meat (+1 meat, -1 else)", tabName = "+1#meat,-1#plant,-1#resist"),
               menuSubItem("resists poison (+1 resist, -1 else)", tabName = "+1#resist,-1#plant,-1#meat")),
      menuItem("glands", tabName = "glands",
               menuSubItem("deadlier (+1 poison)", tabName = "+1#poison"),
               menuSubItem("more efficent (-1 poison)", tabName = "-1#poison")),
      menuItem("wings", tabName = "wings",
               menuSubItem("more winged (+1 flight)", tabName = "+1#flight"), #includes word wings
               menuSubItem("less winged (-1 flight)", tabName = "-1#flight")),
      menuItem("behavior", tabName = "behavior",
               menuSubItem("hunts in a pack (+1 pack)", tabName = "+1#pack"), #better working and better stat?
               menuSubItem("hunts independantly (-1 pack)", tabName = "-1#pack"))
    )})
  
})

