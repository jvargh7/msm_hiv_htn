# Install if necessary
# install.packages("pwr")

# Load the package
library(pwr)

# Example parameters
PR1 <- 1.31
n <- 160 # Initial guess for sample size
m <- 0.496 # Calculate 'a' using the first equation
a <- (PR1 * m) / (1 + PR1)
p_control = m - a
p_treatment = a

lambda_control <- p_control*n # Mean count in the control group
lambda_treatment <- p_treatment*n # Mean count in the treatment group
alpha <- 0.05 # Significance level
power <- 0.8 # Desired power

# Calculate effect size
log_irr <- log(lambda_treatment) - log(lambda_control)

# Use pwr.p.test for a rough estimation (note: this is more illustrative than exact for Poisson regression)
p_test <- pwr.p.test(h = log_irr, sig.level = alpha, n = 147,alternative = "two.sided")

print(p_test)
exp(log_irr)

# Risk Ratio
n_new = p_test$n
N_m_minus_a = 2*n_new*(m-a) # number of individuals at risk with partner diagnosed
N_rest = 2*n_new*(1+a-2*m)

r_control = 5/1000 # Solveig: 1.5/1000 vs 16.4/1000
r_treatment = 10/1000
risk_control = N_rest*r_control
risk_treatment = N_m_minus_a*r_treatment


log_RR <- log(risk_treatment) - log(risk_control)
# log_RR <- log(r_treatment) - log(r_control)
pRR_test <- pwr.p.test(h = log(1.28),sig.level = alpha, n = (N_m_minus_a + N_rest),alternative = "two.sided")
# pRR_test <- pwr.p.test(n = , sig.level = alpha, power = power, alternative = "two.sided")
pRR_test

p_test$n*1.10*1.10*1.10



p_test <- pwr.p.test(power = 0.80,sig.level = alpha, n = round(pRR_test$n),alternative = "two.sided")
p_test

exp(p_test$h)
