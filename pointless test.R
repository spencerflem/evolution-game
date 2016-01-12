creatures <- matrix(
  c(-0.2, 1, 800, 1, 50, .4, 1, 0, #predator
    -0.2, 1.2, 550, 1, 30, .4, 0, 0, #prey
    0.2, 2, 50, 5, 95, .8, 0, 1), #grass
  nrow = 3, byrow = TRUE
)

library(plyr)

#TRUE/FALSE?
base <- data.frame(rval = -0.2, popsize = 1, quality = 800, catchper = 1, seenchance = 50, influence = .4, predator = TRUE, grass = FALSE, row.names = 2398)
evo1 <- data.frame(quality = 900, row.names = 2398)
evo2 <- data.frame(catchper = 2, seenchance = 5)
evos <- list(base, evo1, evo2)
combs <- rbind.fill(evos)
tots <- colSums(combs, na.rm = TRUE)
tots

#TEST LIST INSIDE MATRIX
#IF SO:
#creatures <- matrix
#ID, LIST OF evos (first evo is BASE CREATRUE)

testm <- matrix(
  c(2309, 3400, list(c("a","b")), list(c("b","c"))),
  ncol = 2
)
testm


evolutions <- list(
    data.frame(cat = "arms", evo = "good arms", quality = 200),
    data.frame(cat = "arms", evo = "bad arms", quality = 401),
    data.frame(cat = "legs", evo = "average legs", quality = 320)
)

baseCreature <- data.frame(rval = -0.2, popsize = 1, quality = 800, catchper = 1, seenchance = 50, influence = .4, predator = FALSE, grass = FALSE)
grass <- data.frame(name = "grass", rval = 0.2, popsize = 2, quality = 50, catchper = 5, seenchance = 95, influence = .8, predator = FALSE, grass = TRUE)

creatures <- matrix(
  c(-1, list(c(grass))), ncol = 2
)


tlist <- list("a","b")
tlist <- c(tlist, "c")
tlist


library(plyr)

computeEvoFrame <- function(evolutions) {
  evoFrame <- data.frame()
  for(evo in evolutions) {
    evoFrame <- rbind.fill(evoFrame, evo)
  }
  print(evoFrame)
  return(evoFrame)
}

evolutions <- list(
  data.frame(cat = "arms", evo = "good arms", quality = 200),
  data.frame(cat = "arms", evo = "bad arms", quality = 401),
  data.frame(cat = "legs", evo = "average legs", humility = 320)
)

evolutionsFrame <- computeEvoFrame(evolutions)
evolutionsFrame









computeEvoFrame <- function(evolutions) {
  evoFrame <- data.frame()
  rowNames <- c()
  for(evo in evolutions) {
    rowNames <- c(rowNames, evo$evo)
    evo$cat <- NULL
    evo$evo <- NULL
    evoFrame <- rbind.fill(evoFrame, evo)
  }
  row.names(evoFrame) <- rowNames
  print(evoFrame)
  return(evoFrame)
}

evolutions <- list(
  data.frame(cat = "arms", evo = "good arms", quality = 200, stringsAsFactors = FALSE),
  data.frame(cat = "arms", evo = "bad arms", quality = 401, stringsAsFactors = FALSE),
  data.frame(cat = "legs", evo = "average legs", humility = 320, stringsAsFactors = FALSE)
)

evolutionsFrame <- computeEvoFrame(evolutions)
