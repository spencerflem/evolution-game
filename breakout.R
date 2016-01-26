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

grass <- data.frame(name = "grass", popSize = 20000, calories = 25, catchesPerStep = 2, seenChance = 95, predator = FALSE, grass = TRUE, caloriesRequired = -1, lifeExpectancy = -1, babyCalories = -1, maxBabies = -1, babySurviveChance = -1, nearChanceCenter = 18000, nearChanceSlope = 2000)

pred1 <- data.frame(name = "predator", popSize = 100, calories = 800, catchesPerStep = 0.4, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 120, maxBabies = 2, babySurviveChance = 70, nearChanceCenter = 600, nearChanceSlope = 1000)
pred2 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.6, seenChance = 20, predator = TRUE, grass = FALSE, caloriesRequired = 150, lifeExpectancy = 25, babyCalories = 110, maxBabies = 1, babySurviveChance = 85, nearChanceCenter = 600, nearChanceSlope = 1000)
pred3 <- data.frame(name = "predator", popSize = 100, calories = 1000, catchesPerStep = 0.4, seenChance = 10, predator = TRUE, grass = FALSE, caloriesRequired = 200, lifeExpectancy = 30, babyCalories = 85, maxBabies = 3, babySurviveChance = 75, nearChanceCenter = 600, nearChanceSlope = 1000)

prey1 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 0.7, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 30, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 40, nearChanceCenter = 310, nearChanceSlope = 35)
prey2 <- data.frame(name = "prey", popSize = 350, calories = 300, catchesPerStep = 1.8, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 55, lifeExpectancy = 8, babyCalories = 15, maxBabies = 8, babySurviveChance = 30, nearChanceCenter = 310, nearChanceSlope = 35)
prey3 <- data.frame(name = "prey", popSize = 350, calories = 200, catchesPerStep = 0.8, seenChance = 60, predator = FALSE, grass = FALSE, caloriesRequired = 40, lifeExpectancy = 10, babyCalories = 10, maxBabies = 12, babySurviveChance = 40, nearChanceCenter = 310, nearChanceSlope = 35)
prey4 <- data.frame(name = "prey", popSize = 350, calories = 100, catchesPerStep = 1.0, seenChance = 30, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 12, babyCalories = 12, maxBabies = 14, babySurviveChance = 35, nearChanceCenter = 310, nearChanceSlope = 35)
prey5 <- data.frame(name = "prey", popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, nearChanceCenter = 310, nearChanceSlope = 35)

creatures <- list('-1' = list(grass, prey1, grass), '-8' = list(prey2, pred1, grass))

baseCreature <- data.frame(name = "prey", popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, nearChanceCenter = 310, nearChanceSlope = 35)


addCreature <- function(ID, class, race, name) {
  newCreature <- data.frame(name = name, baseCreature) #TODO: VERY IMPORTANT Flesh this line out.
  strID <- toString(ID)
  creatures[[strID]] <<- list(newCreature)
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


stepSim <- function() {
  print("STEPPIN' IT UP!")
}

addSession()
addSession()
addCreature(sessions[1], 'dog', 'cat', 'predo')
addCreature(sessions[2], 'mouse', 'small', 'preysley')
addEvolution(sessions[1], 'good arms')
addEvolution(sessions[2], 'bad arms')
