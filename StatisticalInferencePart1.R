# setwd("F:/01. Data Science/06. Statistical Inference/04. Week 4/01. Course Project")

# PART 1: A simulation exercise

# Set the given constants in R variables
set.seed(100)
lambda <- 0.2
n <- 40
simulations <- 1000

# Replicate to prepare simulations and calculate their mean
simulationsExp <- replicate (simulations, rexp(n, lambda))
meansExp <- apply (simulationsExp, 2, mean)

# Plot histogram
hist(meansExp, breaks = 40, xlim = c(2, 9), main = "Simulation Means", col = "blue")

# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
sampleMean <- mean(meansExp)
theoreticalMean <- 1 / lambda

print(paste("Sample Mean of simulated data is", sampleMean, " while Theoretical Mean of same data is", theoreticalMean))
print(paste("Difference between these two is just", (theoreticalMean - sampleMean), "It indicates that Sample Mean and Theoretical Means are preety close."))

# Show above in a plot
hist (meansExp, breaks = 40, xlim = c (2, 9), main = "Theoretical Mean vs. Actual Sample Mean", xlab = "Simulation Means", col="blue")

# plot a vertical red line at the mean of the sample means
abline (v = sampleMean, lwd = "2", col = "red")
abline (v = theoreticalMean, lwd = "2", col = "yellow")
text (x = 7, y = 55, "<- Sample Mean (4.99)", col = "red", lwd = 4)
text (x = 7, y = 65, "<- Theoretical Mean (5)", col = "yellow", lwd = 4)

# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
theoreticalVar <- round (((1/lambda)/sqrt(n))^2 ,4)
simulatedVar <- round (sd(meansExp)^2 ,4)

print(paste("Simulated variance of this data is", simulatedVar, " while Theoretical variance of same data is", theoreticalVar))
print(paste("Difference between these two is just", (theoreticalVar - simulatedVar), "It indicates that Sample Variance and Theoretical Variance are preety close."))


# 3. Show that the distribution is approximately normal.
#General Plot with ditribution curve drawn
hist (meansExp, prob = TRUE, col = "blue", main="Simulation Means of Exponential Function", breaks = 40, xlim = c (2, 9), xlab = "Simulation Means")
lines (density (meansExp), lwd = 4, col = "red")

# Normal distribution line creation
x <- seq (min (meansExp), max (meansExp), length =2*n)
y <- dnorm (x, mean = 1 / lambda, sd = sqrt (((1/lambda)/sqrt(n))^2))
lines(x, y, pch = 22, col = "yellow", lwd = 3, lty = 2)

print("Sample curve is red line and yellow dotted line is normal distribution. Both these are very close as one can see in the plot above.")










