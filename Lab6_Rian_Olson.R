#Lab 6

ode_function <- function(t, y) {
  return(-y * cos(t))
}


euler_method <- function(f, y0, t_start, t_end, h) {
  t_values <- seq(t_start, t_end, by=h)
  y_values <- numeric(length(t_values))
  y_values[1] <- y0
  for (i in 2:length(t_values)) {
    y_values[i] <- y_values[i-1] + f(t_values[i-1], y_values[i-1]) * h
  }
  return(list(t = t_values, y = y_values))
}


y0 <- 1.241

t_start <- 0
t_end <- 6

hs <- c(0.5, 0.25, 0.1)


plot(NULL, xlim=c(t_start, t_end), ylim=c(min(0.5 * exp(sin(seq(t_start, t_end, length.out = 1000)))), y0), 
     xlab="Time t", ylab="Displacement y", main="Displacement over Time")
colors <- c("red", "blue", "green")


for (i in seq_along(hs)) {
  result <- euler_method(ode_function, y0, t_start, t_end, hs[i])
  lines(result$t, result$y, col=colors[i], type="l", lwd=2, lty=i)
}


t_analytical <- seq(t_start, t_end, length.out = 1000)
y_analytical <- 0.5 * exp(sin(t_analytical))
lines(t_analytical, y_analytical, col="black", lwd=2, lty=2)


legend("topright", legend=c(paste("Euler's Method h=", hs), "Analytical Solution"), 
       col=c(colors, "black"), lwd=2, lty=1:2)


absolute_errors_05 <- abs(result_h_05$y - y_analytical_05)
relative_errors_05 <- absolute_errors_05 / abs(y_analytical_05)

print(absolute_errors_05)
print(relative_errors_05)

