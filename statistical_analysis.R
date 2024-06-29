library("datasets")
library("dplyr")
library(help = "datasets")
library(HH)

?USArrests

data(USArrests)
head(USArrests)
summary(USArrests)

# Explore associations between variables through a scatterplot matrix
pairs(USArrests)

selected_murder <- select(USArrests, Murder, UrbanPop)
selected_assault <- select(USArrests, Assault, UrbanPop)
selected_rape <- select(USArrests, Rape, UrbanPop)

X1 <- selected_murder
X2 <- selected_assault
X3 <- selected_rape
Y <- select(USArrests, UrbanPop)

head(Y)
summary(Y)

head(X1)
summary(X1)
mean_murder <- aggregate(Murder ~ UrbanPop, data = USArrests, FUN = mean)

head(X2)
summary(X2)
mean_assault <- aggregate(Assault ~ UrbanPop, data = USArrests, FUN = mean)

head(X3)
summary(X3)
mean_rape <- aggregate(Rape ~ UrbanPop, data = USArrests, FUN = mean)

plot(X1$UrbanPop, X1$Murder,
     xlab = "Urban Population",
     ylab = "Murder Rate",
     main = "Murder Rate vs. Urban Population",
     col = "blue",
     ylim = c(0, max(X1$Murder) + 5),
     xlim = c(0, max(X1$UrbanPop) +6))

lines(mean_murder$UrbanPop, mean_murder$Murder, col = "red", lwd = 2)

plot(X2$UrbanPop, X2$Assault, 
     ylab = "Assault Rate",
     xlab = "Urban Population",
     main = "Scatterplot: Assault Rate vs. Urban Population",
     col = "blue",
     ylim = c(0, max(X2$Assault) + 50),
     xlim = c(0, max(X2$UrbanPop) +6))
lines(mean_assault$UrbanPop, mean_assault$Assault, col = "red", lwd = 2)

plot(X3$UrbanPop, X3$Rape, 
     ylab = "Rape Rate",
     xlab = "Urban Population",
     main = "Scatterplot: Rape Rate vs. Urban Population",
     col = "blue",
     ylim = c(0, max(X3$Rape) +5 ),
     xlim = c(0, max(X3$UrbanPop) +6))
lines(mean_rape$UrbanPop, mean_rape$Rape, col = "red", lwd = 2)

# These histograms typically peak at the sample median and then decrease symmetrically on both sides
# of this point in a bell-shaped fashion. These data sets are considered normal and their histograms are called normal histograms.

# If the histogram of a data set is close to a normal histogram, then we say that the data set is approximately normal.

par(mfrow = c(2, 2))

mediana_pop <- median(Y$UrbanPop)

dataset <- USArrests

# Extract state names and UrbanPop data

hist(Y$UrbanPop,
     main = "Distribution of Urban Population",
     xlab = "Urban Population",
     ylab = "Frequency",
     col="blue")
abline(v = median_pop, col = "red")



abline(v = mediana_pop, col = "red")

median_murder <- median(X1$Murder)
hist(X1$Murder,
     main = "Distribution of Murder",
     xlab = "Murder",
     ylab = "Frequency",
     col="blue")
abline(v = median_murder, col = "red")

median_assault <- median(X2$Assault)
hist(X2$Assault,
     main = "Distribution of Assault",
     xlab = "Assault",
     ylab = "Frequency",
     col="blue")
abline(v = median_assault, col = "red")

median_rape <- median(X3$Rape,)
hist(X3$Rape,
     main = "Distribution of Rape",
     xlab = "Rape",
     ylab = "Frequency",
     col="blue")
abline(v = median_rape, col = "red")

############################################################################
# Probability density distribution (DDP) curve
#A symmetrical curve may suggest a normal distribution,
#while an asymmetrical curve may indicate a non-normal distribution.

# Calculates the density of urban population
dens <- density(USArrests$UrbanPop)

# Create the urban population histogram
hist(USArrests$UrbanPop, breaks = 7, xlim = range(dens$x),
     ylim = c(0, max(dens$y)), probability = TRUE, col = "red",
     main = "Distribution of Urban Population", xlab = "Urban Population", ylab = "Density")

# Add the density function over the histogram
lines(dens, col = "blue")

# Calculates the density of murder
dens_murder <- density(X1$Murder)

# Create the murder histogram
hist(X1$Murder, breaks = 10, xlim = range(dens_murder$x),
     ylim = c(0, max(dens_murder$y)), probability = TRUE, col = "red",
     main = "Distribution of Murder", xlab = "Murder", ylab = "Density")

# Add the density function over the histogram
lines(dens_murder, col = "blue")

# Calculates the density of urban population
dens_assault <- density(X2$Assault)

# Create the assault histogram
hist(X2$Assault, breaks = 10, xlim = range(dens_assault$x),
     ylim = c(0, max(dens_assault$y)), probability = TRUE, col = "red",
     main = "Distribution of Assault", xlab = "Assault", ylab = "Density")

# Add the density function over the histogram
lines(dens_assault, col = "blue")

# Calculates the density of urban population
dens_rape <- density(X3$Rape)

# Create the rape histogram
hist(X3$Rape, breaks = 10, xlim = range(dens_rape$x),
     ylim = c(0, max(dens_rape$y)), probability = TRUE, col = "red",
     main = "Distribution of Rape", xlab = "Rape", ylab = "Density")

# Add the density function over the histogram
lines(dens_rape, col = "blue")


# Calculate the sample mean
sample_mean_pop <- mean(Y$UrbanPop)
sample_mean_murder <- mean(X1$Murder)
sample_mean_assault <- mean(X2$Assault)
sample_mean_rape <- mean(X3$Rape)

# Calculate the sample standard deviation
sample_standard_deviation_pop <- sd(Y$UrbanPop)
sample_standard_deviation_murder <- sd(X1$Murder)
sample_standard_deviation_assault <- sd(X2$Assault)
sample_standard_deviation_rape <- sd(X3$Rape)

# Sample size
n <- length(Y$UrbanPop)
n

#Choosing a 95% confidence interval
alpha = 0.05

# two values of probabilities of interest (alpha/2, 1 – alpha/2), for the bilateral case
# for calculating the confidence interval

# Calculating the critical values (z):
z <- qnorm(c(alpha/2,1-alpha/2))
z
#Standard error of the mean:
standard_error_mean <- sqrt(sample_standard_deviation_pop^2/n)
standard_error_mean
#Confidence interval
ci <- sample_mean_pop + z*standard_error_mean
ci

# Defining the limits of the confidence interval and the critical value
lower_limit <- ci[1]
upper_limit <- ci[2]

###############################################################################
#Central limit theorem
#This theorem tells us that, if X is a population with mean μ and variance σ2, 
#then the sampling distribution of the mean of a random sample of size n 
#approximates a normal distribution with mean μ and variance σ2/n when n → ∞.
 
#when you take several samples from a population and calculate the mean of each of these 
#samples, you get a series of mean sample values. 
#The distribution of these sample means is called the sampling distribution of the mean.

# this is valid, provided n is sufficiently large (n>30)


# Defining the sample size and the number of samples
sample_size <- 45
num_samples <- 10000

# Creating a vector to store the sample means
sample_means <- numeric(num_samples)

# Making random sampling and calculating the sample means
for (i in 1:num_samples) {
  sample <- sample(USArrests$UrbanPop, sample_size, replace = TRUE)
  sample_means[i] <- mean(sample)
}

hist(sample_means, breaks = 30, freq = FALSE, main = "Histogram of Sample Means Urban Population",
     xlab = "Sample Mean", ylab = "Density")

# Adding a normal density curve to the plot
normal_curve <- density(sample_means)
lines(normal_curve, col = "red", lwd = 2)

############################################################################
# Defining the sample size and the number of samples
sample_size <- 45
num_samples <- 10000
# Creating a vector to store the sample means
sample_means <- numeric(num_samples)

# Making random sampling and calculating the sample