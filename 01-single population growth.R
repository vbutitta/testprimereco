library("primer")


# CH 1 #
N0     <- 1
lambda <- 2
time   <- 0:10

Nt <- N0*lambda^time
Nt

# different starting populations
N0     <- c(10, 20, 30)
lambda <- 2
time   <- 0:4

Nt.s <- sapply(N0, function(n) n * lambda^time)
Nt.s

matplot(time, Nt.s, pch = 1:3)
matplot(time, Nt.s, log="y", pch = 1:3)

# different pop growth rate
N0      <- 100
time    <- 0:3
lambdas <- c(0.5, 1, 1.5)

N.all <- sapply(lambdas, function(x) N0 * x^time)

matplot(time, N.all, xlab= "years", ylab = "N", pch=1:3)
abline(h=N0, lty=3)

# instantaneous growth rate
r   <- c(-0.03, -0.02, 0, 0.02, 0.03)
N0  <- 2
t   <- 1:100

cont.mat <- sapply(r, function(ri) N0 * exp(ri * t))

layout(matrix(1:2, nrow = 1))
matplot(t, cont.mat, type="l", col=1)  # geom scale
matplot(t, cont.mat, type="l", log="y") #log scale

# Sparrows training dataset
data(sparrows)
attach(sparrows)

plot(Count ~ Year, type='b')

# calculate growth rate per year
obs.R <- Count[-1]/Count[-length(Count)]
plot(obs.R ~ Year[-length(Count)])
abline(h=1, lty=3)

# simulate possible projections of populations
years <- 50 # years to simulate

set.seed(3)
sim.Rs <- sample(x = obs.R, size = years, replace = TRUE) # create sample Rs from obs.R

output <- numeric(years + 1)
output[1] <- Count[Year == max(Year)] # start simulation with the count value from the final year "max(year)" of the obs data

for(t in 1:years) output[t + 1] <- {
  output[t] * sim.Rs[t]
}

plot(0:years, output, type="l")

# simulate multiple times
sims = 10
sim.RM <- matrix(sample(obs.R, sims * years, replace = TRUE), nrow = years, ncol = sims)

output[1] <- Count[Year == max(Year)]
outmat <- sapply(1:sims, function(i) {
  for (t in 1:years) output[t + 1] <- output[t] * sim.RM[t,i] 
  output
})

matplot(0:years, outmat, type="l", log="y")




