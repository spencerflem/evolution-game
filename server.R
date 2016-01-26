library(shiny)
library(Rgraphviz)
library(ape)
library(deSolve)
library(shinyjs)
library(prob)
library(plyr)

evolutions <- list(
  "good arms" = data.frame(cat = "arms", quality = 200, stringsAsFactors = FALSE),
  "bad arms" = data.frame(cat = "arms", quality = 401, stringsAsFactors = FALSE),
  "average legs" = data.frame(cat = "legs", humility = 320, stringsAsFactors = FALSE)
)

allowedEvos <- function(ID) {
  #TODO
}

submittedIDs <- c()

addEvolution <- function(ID, evolution)
{
  if(ID %in% submittedIDs) {
    print("NOPE")
  }
  else {
    if(TRUE) { #evolution %in% allowedEvos(ID)
      strID <- toString(ID)
      creatures[[strID]] <<- c(creatures[[strID]], list(evolutions[[evolution]]))
      submittedIDs <<- c(submittedIDs, ID)
      numNotSubmitted <- getNumNotSubmitted()
      if(numNotSubmitted == 0) {
        stepSim()
        submittedIDs <<- c()
      }
    }
  }
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

baseCreature <- data.frame(popsize = 100, quality = 800, timeToCatch = 5, seenchance = 50, predator = FALSE, grass = FALSE)
grass <- data.frame(name = "grass", popsize = 2000, quality = 50, timeToCatch = 1, seenchance = 95, predator = FALSE, grass = TRUE)

creatures <- list('-1' = list(grass, grass))

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
  newCreature <- data.frame(name = name, baseCreature)
  strID <- toString(ID)
  creatures[[strID]] <<- list(newCreature)
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

populationSizes <- creatures[,2] #TODO: NOT RESILIANT TO ADDING NEW CREATURES! ALSO WRONG NOW!

#########SIM BEGINS HERE TODO: CHANGE EVERYTHING (specifically using new creature datatype)
grass <- data.frame(name = "grass", popSize = 20000, calories = 25, catchesPerStep = 2, seenChance = 95, predator = FALSE, grass = TRUE, caloriesRequired = -1, lifeExpectancy = -1, babyCalories = -1, maxBabies = -1, babySurviveChance = -1, nearChanceCenter = 18000, nearChanceSlope = 2000)

pred1 <- data.frame(name = "predator", popSize = 100, calories = 800, catchesPerStep = 0.4, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 120, maxBabies = 2, babySurviveChance = 70, nearChanceCenter = 600, nearChanceSlope = 1000)
pred2 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.6, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 150, lifeExpectancy = 25, babyCalories = 110, maxBabies = 1, babySurviveChance = 85, nearChanceCenter = 600, nearChanceSlope = 1000)
pred3 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.4, seenChance = 10, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 85, maxBabies = 3, babySurviveChance = 75, nearChanceCenter = 600, nearChanceSlope = 1000)

prey1 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 0.7, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 30, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 40, nearChanceCenter = 310, nearChanceSlope = 35)
prey2 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 1.8, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 55, lifeExpectancy = 8, babyCalories = 15, maxBabies = 8, babySurviveChance = 30, nearChanceCenter = 310, nearChanceSlope = 35)
prey3 <- data.frame(name = "prey", popSize = 350, calories = 200, catchesPerStep = 0.8, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 40, lifeExpectancy = 10, babyCalories = 10, maxBabies = 12, babySurviveChance = 40, nearChanceCenter = 310, nearChanceSlope = 35)
prey4 <- data.frame(name = "prey", popSize = 350, calories = 100, catchesPerStep = 1.0, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 12, babyCalories = 12, maxBabies = 14, babySurviveChance = 35, nearChanceCenter = 310, nearChanceSlope = 35)
prey5 <- data.frame(name = "prey", popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, nearChanceCenter = 310, nearChanceSlope = 35)

#creatures <- list('-1' = list(grass), '2' = list(pred1), '66' = list(pred2), '3' = list(pred3), '99' = list(prey1), '4' = list(prey2), '5' = list(prey3), '6' = list(prey4), '2' = list(prey5))
creatures <- list('-1' = list(grass), '66' = list(prey1), '6666' = list(prey2), '7676' = list(prey3), '988ds' = list(prey4), '765675' = list(prey5), '425452ds' = list(pred1), '78sdasdf' = list(pred2), '54455445' = list(pred3))

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
  #eatenMatrixSLOW <- calculateEatenMatrix(summedCreatures)
  eatenMatrix <- calculateEatenMatrixFast(summedCreatures)
  
  catchesPerStep <- computecatchesPerStep(summedCreatures)
  catchesPerStepMatrix <- matrix(catchesPerStep, nrow = numCreatures, ncol = numCreatures)
  
  #print("CatchesPerStepMtrx")
  #print(catchesPerStepMatrix)
  
  populations <- summedCreatures$popSize
  
  #print("PopulationMtrx:")
  
  popMatrix <- matrix(populations, nrow = numCreatures, ncol = numCreatures)
  
  #print(popMatrix)
  
  for(i in 1:subTimes) {
    
    #print("EatenMtrx:")
    #print(eatenMatrix)
    #print("CatchesPerStep")
    #print(catchesPerStep)
    #print("NumEatenMtrx:")
    
    numEatenMatrix <- eatenMatrix * catchesPerStepMatrix * popMatrix
    
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
  
  #print("CaloriesMtrx:")
  #print(caloriesMatrix)
  #print("CaloriesEatenMtrx:")
  #print(caloriesEatenMatrix)
  
  caloriesGained <- 2 * rowSums(caloriesEatenMatrix) #TODO: ARBY $ why are cals so low?
  
  #print("CaloriesGained:")
  #print(caloriesGained)
  #print("CaloriesRequired:")
  #print(summedCreatures$caloriesRequired * summedCreatures$popSize)
  
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
  
  #print("ChanceToBeNearMtrx:")
  #print(chanceToBeNearMatrix)
  #print("ChaceToBeSeenMatrix:")
  #print(chanceToBeSpottedMatrix)
  
  chanceMatrix <- chanceToBeNearMatrix * chanceToBeSpottedMatrix
  
  #print("SeenMatrixRow:")
  #print(chanceMatrix[1,])
  
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
    V <- letters[1:10]
    M <- 1:4
    set.seed(input$bins)
    g1 <- randomGraph(V, M, 0.2)
    plot(g1)
  })
  
  output$evoTree <- renderPlot({
    tree <- rtree(n = 20)
    plot(tree, edge.width = 2)
  })
  
  output$populations <- renderPlot({
    toplot <- matrix(c(1:5,11:15),nrow = 2, ncol = 5)
    matplot(toplot, type = "l", log = "y")
  })
  
  #TODO: MAKE GRAPHS REACTIVE!!!!!
  
  output$yourCreature <- renderText("getEvolutions()")
  
  output$otherCreatures <- renderPrint("addedCreature()")
  
  observeEvent(input$selectedEvo, {enable("confirmed")})
  
  observeEvent(input$confirmed, {addEvolution(input$ID, input$selectedEvo)}, ignoreNULL = FALSE)
  
  observeEvent(input$joined, {addCreature(input$ID, input$class, input$race, input$name)})
  
  #TODO: ONLY SHOW SELECTABLE EVOS: USE allowedEvos(input$ID) somehow perhaps createMenu()
  output$evos <- renderMenu({
    sidebarMenu(
      menuItem("arms",tabName = "Arms",
               menuSubItem("good arms",tabName = "good arms"),
               menuSubItem("bad arms",tabName = "bad arms")),
      menuItem("legs",tabName = "Legs",
               menuSubItem("good legs",tabName = "good legs"),
               menuSubItem("bad legs",tabName = "bad legs")),
      menuItem("body",tabName = "Body",
               menuSubItem("good body",tabName = "good body"),
               menuSubItem("bad body",tabName = "bad body")),
      menuItem("brain",tabName = "Brain",
               menuSubItem("good brain",tabName = "good brain"),
               menuSubItem("bad brain",tabName = "bad brain"))
    )})
  
})






stepSim(75,1,0.2,creatures)
"============================================================================================="
populationSizes
matplot(populationSizes, type = "l", log = "y")

