library(nleqslv)

# Constants
PR1 <- 1.3
m <- 0.40
RR <- 5
e <- 0.2
PR2 <- 1.05

# Calculate 'a' using the first equation
a <- (PR1 * m) / (1 + PR1)

# Define the system of equations
system_of_equations <- function(vars) {
  c <- vars[1]
  d <- vars[2]
  
  # Second equation
  eq1 <- c / (m - a) - RR * (e + 2 * d) / (1 + a - 2 * m)
  
  # Third equation
  eq2 <- (a + 2 * c + d) / (m + c + d + e) - PR2 * (m - a - c + e) / (1 - m - c - d - e)
  
  return(c(eq1, eq2))
}

# Initial guesses for 'c' and 'd'
initial_guesses <- c(0.1, 0.1)

# Use nleqslv to solve the system of equations
solution <- nleqslv(x = initial_guesses, fn = system_of_equations)

# Display the solution
if (solution$termcd == 1) {
  cat("The solution is:\n")
  cat(sprintf("c = %f\n", solution$x[1]))
  cat(sprintf("d = %f\n", solution$x[2]))
} else {
  cat("The solver did not converge to a solution. Please check the equations and initial guesses.\n")
}
