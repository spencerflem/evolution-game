#CREATURES ARRAY
##each has current population
##each has lists of evoltions
####evoltions calculate stats
####stats calculate interactions
##each has name and way of displaying what it evolved from
####how?


#each sub-list is a creature
#of type (r-value, population, quality, chancetobeseen, ispredator, isgrass)

creatures <- matrix(
  c(-0.2, 1, 800, 1, 50, 1, 0, #predator
    -0.2, 1.2, 550, 1, 30, 0, 0, #prey
    0.2, 2, 50, 5, 95, 0, 1), #grass
  nrow = 3, ncol = 7, byrow = TRUE
)
#dataframe? dimnames?

r <- creatures[,1] #r-value, + if plant, - if creatrue
starting <- creatures[,2] #initial population size
a <- calculateInteractionMatrix(creatures) #interaction matrix

steps <- 7
N0 <- starting
final <- starting

while (steps > 0) {
  steps <- steps - 1
  parms <- list(r,a)
  t=seq(1,1000, by=1)
  library(deSolve)
  lvout <- ode(N0, t, function (t, n, parms) 
  {
    r <- parms[[1]]
    a <- parms[[2]]
    dns.dt <- n * (r + (a %*% n)[,1])
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

toplot <- matrix(nrow = 2, ncol = 3)
toplot[1,] <- starting
toplot[2,] <- final
matplot(toplot, type = "l", log = "y")

calculateInteractionMatrix <- function(creatures) {
  samplespace <- computeSampleSpace(creatures)
  return(matrix(c(0,0.2,0,-0.2,0,0.2,0,-0.2,0), nrow=3, ncol=3, byrow=TRUE))
}

computeSampleSpace <- function(creatures) {
  ncreatures <- nrow(creatures)
  ssones <- rep(1,2^ncreatures)
  ssdims <- rep(2,ncreatures)
  samplespace <- array(ssones, dim = ssdims)
  for(i in 1:ncreatures) {
    index <- rep(c(1,2),ncreatures)
    index[i] <- 1
    index[i+1] <- 1
    #[] or [[]]
    samplespace[index]
  }
}
