library(primer)
library(lattice)
# Chapter 3
#3.5

rd.v <- seq(1.3,  2.8, by =0.3)
t <- 15
Ns <- data.frame(sapply(rd.v, function(r) dlogistic(rd = r, t = t)))
matplot(0:t, Ns, type = "l", col = 1)

# limit cycles
tmp <- data.frame(rd = as.factor(rd.v), t(Ns))
Ns2 <- reshape(tmp, varying = list(2:ncol(tmp)), idvar = "rd", v.names = "N", direction = "long")
print(xyplot(N~time | rd, data = Ns2, type = "l", layout = c(3,2,1), col=1))

# new line for git test