
library(readxl)
library(ggplot2)
library(minpack.lm)

setwd("C:/Level_4_Rian_Olson/R code/Assignment2_Rian_Olson")


read_rocket_data <- function(filename) {
  file_path <- file.path(".", filename)
  if(file.exists(file_path)) {
    rocket_data <- read_excel(file_path)
    return(rocket_data)
  } else {
    cat("File does not exist, please enter the name of the file to open:")
    filename <- readline()
    return(read_rocket_data(filename))
  }
}


power_fit <- function(data) {
  fit <- nls(d_meter ~ a * t_sec^b, data = data, start = list(a = 1, b = 1))
  return(fit)
}

exp_fit <- function(data) {
  fit <- nls(d_meter ~ a * exp(b * t_sec), data = data, start = list(a = 1, b = 1))
  return(fit)
}



calculate_Sr <- function(fit, data) {
  residuals <- residuals(fit)
  Sr <- sum(residuals^2)
  return(Sr)
}


best_fit_analysis <- function(data) {
  power_model <- power_fit(data)
  exp_model <- exp_fit(data)
  
  Sr_power <- calculate_Sr(power_model, data)
  Sr_exp <- calculate_Sr(exp_model, data)
  
  if(Sr_power < Sr_exp) {
    best_fit <- "Power"
  } else {
    best_fit <- "Exponential"
  }
  
  return(list(power_model = power_model, exp_model = exp_model, best_fit = best_fit))
}


extrapolate <- function(best_fit_model, time) {
  if(best_fit_model == "Power") {
    distance <- coef(best_fit$power_model)[1] * time ^ coef(best_fit$power_model)[2]
  } else {
    distance <- coef(best_fit$exp_model)[1] * exp(coef(best_fit$exp_model)[2] * time)
  }
  return(distance)
}


generate_plot <- function(data, best_fit_model) {
  p <- ggplot(data, aes(x = Time, y = Distance)) +
    geom_point() +
    stat_function(fun = function(x) coef(best_fit$power_model)[1] * x ^ coef(best_fit$power_model)[2], color = "blue") +
    stat_function(fun = function(x) coef(best_fit$exp_model)[1] * exp(coef(best_fit$exp_model)[2] * x), color = "red") +
    labs(title = "Rocket Distance vs Time",
         x = "Time (sec)",
         y = "Distance (meter)") +
    theme_minimal()
  return(p)
}


bestFitFun <- function() {
  cat("MENU\n")
  cat("1. Best Fit\n")
  cat("2. Quit\n")
  
  choice <- as.integer(readline("Enter your choice: "))
  
  if(choice == 1) {
    filename <- readline("Please enter the name of the file to open: ")
    rocket_data <- read_rocket_data(filename)
    names(rocket_data) <- c("t_sec", "d_meter")
    cat("Rocket Data:\n")
    print(rocket_data)
    
    best_fit <- best_fit_analysis(rocket_data)
    cat("\nPower Model:\n")
    print(summary(best_fit$power_model))
    cat("Sr = ", calculate_Sr(best_fit$power_model, rocket_data), "\n")
    
    cat("\nExponential Model:\n")
    print(summary(best_fit$exp_model))
    cat("Sr = ", calculate_Sr(best_fit$exp_model, rocket_data), "\n")
    
    cat("\nThe best fit model is ", best_fit$best_fit, " model.\n\n")
    
    cat("MENU\n")
    cat("1. Extrapolation\n")
    cat("2. Main Menu\n")
    
    choice <- as.integer(readline("Enter your choice: "))
    if(choice == 1) {
      time <- as.integer(readline("Please enter the time to extrapolate to: "))
      distance <- extrapolate(best_fit$best_fit, time)
      cat("Estimated travelled distance: ", distance, " meters\n")
    } else if(choice == 2) {
      bestFitFun()
    }
    
    plot <- generate_plot(rocket_data, best_fit)
    ggsave("best_fit.pdf", plot)
  } else if(choice == 2) {
    cat("Exiting program...\n")
    return(NULL)
  } else {
    cat("Invalid choice. Please try again.\n")
    bestFitFun()
  }
}


bestFitFun()

