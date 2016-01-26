tc <- list('8' = list('base' = data.frame(popSize = 350, calories = 400, catchesPerStep = 1.5, seenChance = 70, predator = FALSE, grass = FALSE, caloriesRequired = 20, lifeExpectancy = 10, babyCalories = 20, maxBabies = 8, babySurviveChance = 45, nearChanceCenter = 310, nearChanceSlope = 35),
                      'arms' = data.frame(ID = 1, caloriesRequired = 20, stringsAsFactors = FALSE)))


evolutions <- list( #ADD EVOS (priority 5)
    "good arms" = data.frame(caloriesRequired = 20, stringsAsFactors = FALSE),
    "bad arms" = data.frame(caloriesRequired = -10, stringsAsFactors = FALSE),
    "average legs" = data.frame(humility = 320, stringsAsFactors = FALSE)
)

mylist <- list()
mylist_ <- list()
for(i in 1:5) {
  for(j in 1:5) {
    mylist[[j]] <- i*j
  }
  mylist_[[i]] <- mylist
}

allEvos <- laply(evolutions, function(x) laply(x, identity))
