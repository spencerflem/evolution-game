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

