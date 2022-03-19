##### Methods of Integration

# f is the function whose integral we want to integrate
f <- function(x){ return(x) }
# step size
step <- 2^seq(from = 10, to = 20, by = 1)

# integration methods
euler <- function(x_n, dt, f) {
  return(x_n + dt*f(x_n))
}

midpoint <- function(x_n, dt, f) {
  k <- x_n + dt/2*f(x_n)
  return(x_n + dt*f(k))
}

rungekutta <- function(x_n, dt, f) {
  k_1 <- f(x_n)
  k_2 <- f(x_n + dt*k_1/2)
  k_3 <- f(x_n + dt*k_2/2)
  k_4 <- f(x_n + dt*k_3)
  return(x_n + dt/6*(k_1+2*k_2+2*k_3+k_4))
}

euler_results <- vector(length = length(step))
midpoint_results <- vector(length = length(step))
rungekutta_results <- vector(length = length(step))

for (i in 1:length(step)){
  invdt = step[i]
  euler_result <- 1
  midpoint_result <- 1
  rungekutta_result <- 1
  
  for (j in 1:invdt){
    euler_result <- euler(euler_result, 1/invdt, f)
    midpoint_result <- midpoint(midpoint_result, 1/invdt, f)
    rungekutta_result <- rungekutta(rungekutta_result, 1/invdt, f)
  }
  
  euler_results[i] <- euler_result
  midpoint_results[i] <- midpoint_result
  rungekutta_results[i] <- rungekutta_result
}

# numerical error estimates based on the maximum value the second derviative 
# our function e^t takes in the interval [0,1], which is e
# reference: https://math.libretexts.org/Courses/Mount_Royal_University/MATH_2200%3A_Calculus_for_Scientists_II/2%3A_Techniques_of_Integration/2.5%3A_Numerical_Integration_-_Midpoint%2C_Trapezoid%2C_Simpson%27s_rule
# reference: https://en.wikipedia.org/wiki/Euler_method#Global_truncation_error
euler_errbounds <- exp(1)*(exp(1)-1)/2/(step)
midpoint_errbounds <- exp(1)/24/(step^2)
rungekutta_errbounds <- exp(1)/12/(step^2)

euler_tab <- data.frame(step, euler_results, euler_errbounds, (exp(1)-euler_results))
colnames(euler_tab) <- c("1/step size", "Estimate", "Error bound", "Actual error")
print(euler_tab)
midpoint_tab <- data.frame(step, midpoint_results, midpoint_errbounds, (exp(1)-midpoint_results))
colnames(midpoint_tab) <- c("1/step size", "Estimate", "Error bound", "Actual error")
print(midpoint_tab)
rungekutta_tab <- data.frame(step, rungekutta_results, rungekutta_errbounds, (exp(1)-rungekutta_results))
colnames(rungekutta_tab) <- c("1/step size", "Estimate", "Error bound", "Actual error")
print(rungekutta_tab)

