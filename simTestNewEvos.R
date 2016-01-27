baseCreature <- data.frame(name = "lolol", population = 300, size = 5, speed = 5, def = 5, camo = 5, sight = 5, spot = 5, plant = 5, meat = 5, resist = 5, poison = 5, flight = 5, pack = 5, grass = FALSE)
creatures <- baseCreature
creatures <- rbind(creatures, baseCreature)
creaturesROWNAMES <- IDs
creatures
matplot(c(3,4), type = "l", log = "y")


baseCreature <- data.frame(population = 300, size = 5, speed = 5, def = 5, camo = 5, sight = 5, spot = 5, plant = 5, meat = 5, resist = 5, poison = 5, flight = 5, pack = 5, grass = FALSE)
creatures <- baseCreature
rownames(creatures) <- '-1'

crown <- rownames(creatures)
creatures <- rbind(creatures, baseCreature)
crown <- c(crown, '88')
rownames(creatures) <- crown

creatures

newC <- cbind(name = "HOI", baseCreature)
newC
baseCreature


sig <- function(x) {
  y <- ((2/(1+exp(-(i/2.3))))-1)
  return(y)
}

prob <- matrix()
for(i in -15:15) {
  calc <- 2/(1+exp(-(i/2.3)))-1
  if(i == -15) {
    prob <- calc
  }
  else {
    prob <- rbind(prob, calc)
  }
}
prob
matplot(prob, type = 'l')




xpo <- function(x) {
  y <- 1.13^(x)
  return(y)
}





prob <- matrix()
for(i in -5:5) {
  calc <- 1.13^(i)
  if(i == -15) {
    prob <- calc
  }
  else {
    prob <- rbind(prob, calc)
  }
}
prob
matplot(prob, type = 'l')