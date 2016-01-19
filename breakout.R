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
  return(interactionMatrix)
}

stepSim <- function(creatures) {
  
  print("STEPPIN!")
  
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


addSession()
addSession()

addCreature(999, "la", "lo", "le")
addCreature(888, "la", "lo", "les")

addEvolution(888, "good arms")

addEvolution(999, "good arms")
addEvolution(999, "good legs")
