#step 2 A

# Defines the function to calculate the Taylor series
taylor_ln <- function(x, terms) {
  series <- numeric(terms)
  absolute_error <- numeric(terms)
  relative_error <- numeric(terms)
  
  for (n in 1:terms) {
    term <- ((-1)^(n-1))/n * (x - 1)^n
    series[n] <- sum(term)
    absolute_error[n] <- abs(series[n] - log(x))
    relative_error[n] <- absolute_error[n] / abs(log(x))
  }
  
  # Preparing the output table
  output <- data.frame(Term = 1:terms,
                       ln_x = series,
                       Absolute_error = absolute_error,
                       Relative_error = relative_error)
  return(output)
}


# Gets input from the user
x <- as.numeric(readline("Please enter the value of x: "))
terms <- 10  # The number of terms to compute

# Computing Taylor series and display results
result <- taylor_ln(x, terms)
print(result)

#step 2 B

# Defines the function to calculate the Taylor series
taylor_ln <- function(x, terms) {
  series <- numeric(terms)
  
  for (n in 1:terms) {
    term <- ((-1)^(n-1))/n * (x - 1)^n
    series[n] <- sum(term)
  }
  
  return(series)
}

# Gets input from the user
x <- as.numeric(readline("Please enter the value of x: "))
max_terms <- 100  # Maximum number of terms to compute

# Computing Taylor series for different number of terms
series_values <- sapply(1:max_terms, function(terms) {
  taylor_ln(x, terms)[terms]
})

# Plots the value of the series as a function of the number of terms
plot(1:max_terms, series_values, type = "l", 
     xlab = "Number of Terms", ylab = "Series Value",
     main = "Taylor Series of ln(x) around x = 1")



# Step 4 A

library(ggplot2)

# Data from the file
time <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200)
distance <- c(429, 4049, 13411, 31909, 75552, 145054, 171450, 255921, 350983, 550032, 659327, 950342, 1098533, 1456987, 1850443, 2047942, 2455985, 3187294, 3759842, 4987231)

# Converting distance from meters to kilometers
distance_km <- distance / 1000

# Plot
ggplot(data = NULL, aes(x = time, y = distance_km)) +
  geom_line() +
  labs(x = "Time (sec)", y = "Distance (km)", title = "Distance vs. Time")

# Function to compute first derivative using CDD method
first_derivative_cdd <- function(xVec, yVec) {
  n <- length(xVec)
  h <- diff(xVec)
  delta <- diff(yVec) / h
  firstDev <- numeric(n)
  firstDev[1] <- (yVec[2] - yVec[1]) / (xVec[2] - xVec[1])
  firstDev[n] <- (yVec[n] - yVec[n - 1]) / (xVec[n] - xVec[n - 1])
  for (i in 2:(n - 1)) {
    firstDev[i] <- ((yVec[i] - yVec[i - 1]) / (xVec[i] - xVec[i - 1]) + (yVec[i + 1] - yVec[i]) / (xVec[i + 1] - xVec[i])) / 2
  }
  return(firstDev)
}

# Compute the velocity
velocity <- first_derivative_cdd(time, distance_km)

# Plot velocity
ggplot(data = NULL, aes(x = time, y = velocity)) +
  geom_line() +
  labs(x = "Time (sec)", y = "Velocity (km/sec)", title = "Velocity vs. Time")

# Function to compute second derivative using CDD method
second_derivative_cdd <- function(xVec, yVec) {
  n <- length(xVec)
  h <- diff(xVec)
  delta <- diff(yVec) / h
  secondDev <- numeric(n)
  secondDev[1] <- (delta[2] - delta[1]) / (xVec[2] - xVec[1])
  secondDev[n] <- (delta[n] - delta[n - 1]) / (xVec[n] - xVec[n - 1])
  for (i in 2:(n - 1)) {
    secondDev[i] <- (delta[i + 1] - delta[i - 1]) / (xVec[i + 1] - xVec[i - 1])
  }
  return(secondDev)
}

# Computes acceleration
acceleration <- second_derivative_cdd(time, velocity)

# Ploting the acceleration
ggplot(data = NULL, aes(x = time, y = acceleration)) +
  geom_line() +
  labs(x = "Time (sec)", y = "Acceleration (km/sec^2)", title = "Acceleration vs. Time")


