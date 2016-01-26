pbinom(0, size = 2,    prob = 0.0001 * MULTIPLIER, lower.tail = FALSE)
pbinom(0, size = 20,   prob = 0.002, lower.tail = FALSE)
pbinom(0, size = 200,  prob = 0.002, lower.tail = FALSE)
pbinom(0, size = 2000, prob = 0.002, lower.tail = FALSE)

0.0001 * 200

20000000
100000

pbinom(0, size = 2000000, prob = 0.0000001 * 30, lower.tail = FALSE) * .95




#S-CURVE
values2 <- c()
for(i in 0:2000) {
values2 <- c(values2, (1/(1+exp(-(i-275)/400))))
}
matplot(values2, type = "l")




pbinom(0, size = 350, prob = 0.0001 * 10, lower.tail = FALSE)