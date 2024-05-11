
eulers_method <- function(h, theta0, t_end) {
  times <- seq(0, t_end, by = h)
  theta <- numeric(length(times))
  theta[1] <- theta0
  
  for (i in 1:(length(times)-1)) {
    theta[i+1] <- theta[i] + h * (cos(4*times[i]) - 2*theta[i])
  }
  
  return(data.frame(Time = times, EstimatedTemp = theta))
}


rk4_method <- function(h, theta0, t_end) {
  times <- seq(0, t_end, by = h)
  theta <- numeric(length(times))
  theta[1] <- theta0
  
  for (i in 1:(length(times)-1)) {
    k1 <- h * (cos(4*times[i]) - 2*theta[i])
    k2 <- h * (cos(4*(times[i]+h/2)) - 2*(theta[i]+k1/2))
    k3 <- h * (cos(4*(times[i]+h/2)) - 2*(theta[i]+k2/2))
    k4 <- h * (cos(4*(times[i]+h)) - 2*(theta[i]+k3))
    theta[i+1] <- theta[i] + (k1 + 2*k2 + 2*k3 + k4) / 6
  }
  
  return(data.frame(Time = times, EstimatedTemp = theta))
}


exact_solution <- function(t) {
  return(0.1 * cos(4*t) + 0.2 * sin(4*t) + 2.9 * exp(-2*t))
}


calculate_relative_error <- function(results) {
  exact_values <- sapply(results$Time, exact_solution)
  relative_errors <- abs((exact_values - results$EstimatedTemp) / exact_values) * 100
  results$ExactTemp <- exact_values
  results$PercentageError <- relative_errors
  return(results)
}


choose_method <- function() {
  cat("Choose the method for solving the ODE:\n1. Euler’s Method\n2. Runge-Kutta 4th Order Method\n")
  method <- as.integer(readline(prompt=">> "))
  cat("Choose step size “h” (0.8, 0.2, 0.05)\n")
  h <- as.numeric(readline(prompt=">> "))
  theta0 <- 3
  t_end <- 2
  
  if (method == 1) {
    results <- eulers_method(h, theta0, t_end)
  } else if (method == 2) {
    results <- rk4_method(h, theta0, t_end)
  } else {
    cat("Invalid method choice.\n")
    return(NULL)
  }
  
  results_with_error <- calculate_relative_error(results)
  print(results_with_error)
}

choose_method()
