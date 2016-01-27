baseCreature <- data.frame(name = "lolol", population = 300, size = 5, speed = 5, def = 5, camo = 5, sight = 5, spot = 5, plant = 5, meat = 5, resist = 5, poison = 5, flight = 5, pack = 5, grass = FALSE)
creatures <- baseCreature
creatures <- rbind(creatures, baseCreature)
creaturesROWNAMES <- IDs
creatures
matplot(c(3,4), type = "l", log = "y")
