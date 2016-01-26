pbinom(0, size = 2,    prob = 0.0001 * MULTIPLIER, lower.tail = FALSE)
pbinom(0, size = 20,   prob = 0.002, lower.tail = FALSE)
pbinom(0, size = 200,  prob = 0.002, lower.tail = FALSE)
pbinom(0, size = 2000, prob = 0.002, lower.tail = FALSE)

0.0001 * 200

20000000
100000

pbinom(0, size = 2000000, prob = 0.0000001 * 30, lower.tail = FALSE) * .95


for(i in 1:10000) {
pbinom(0, size = 162041, prob = 0.000001 * 6, lower.tail = FALSE) * .90
}