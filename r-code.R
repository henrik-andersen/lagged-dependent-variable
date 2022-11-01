
# | Rehabilitating the Lagged Dependent Variable with Structural Equation Modeling 
# | Henrik Kenneth Andersen & Jochen Mayerl 
# | Supplementary materials: Examples code  
# | 10/2022 

rm(list = ls())

# Generate data -----------------------------------------------------------

# Set seed 
set.seed(45678)

# Load packages 
library(lavaan)
library(dplyr)

# Set large sample size 
n <- 1000L

rho   = 0.3 # Autoregressive effect, Yt -> Yt+1
gamma = 0.6 # Effect U -> D
theta = 0.5 # Effect U -> Y1
beta  = 0.4 # Causal effect, D -> Y2

# Time-invariant unobserved heterogeneity 
V = rnorm(n, 0, 1)

# Simulate initial realization of outcome
Y0 = 1 * V + rnorm(n, 0, 1)

# Time-varying confounder 
U = rnorm(n, 0, 1)

# Causal variable 
D = gamma * U + rnorm(n, 0, 1)

# Remaining realizations of outcome 
Y1 = theta * U + rho * Y0 + 1 * V + rnorm(n, 0, 1)
Y2 = beta  * D + rho * Y1 + 1 * V + rnorm(n, 0, 1)

# Put into dataframe
df = data.frame(Y0, Y1, Y2, D, V, U)

# Save dataframe for Mplus 
write.table(df, file = "df.csv", sep = ",", dec = ".", row.names = FALSE, col.names = FALSE)

# Model 1: All variables observed  ----------------------------------------

m1 = "
  Y2 ~ beta*D + rho*Y1 + V 
  Y1 ~ theta*U + V
  D  ~ gamma*U 
"
sem(model = m1, data = df, estimator = "ML") %>%
  summary()


# Model 2: Time-invariant variable V unobserved ---------------------------

m2 = "
  Y2 ~ beta*D + rho*Y1 
  Y1 ~ theta*U 
  D  ~ gamma*U 
"
sem(model = m2, data = df, estimator = "ML") %>%
  summary()


# Model 3:  Latent time-invariant variable V  -----------------------------

m3 = "
  # Individual effects to account for V
  alpha =~ 1*Y1 + 1*Y2
  # Regressions 
  Y2 ~ beta*D + rho*Y1
  Y1 ~ rho*Y0
  # Allow initial outcome to correlate with unit effects
  alpha ~~ Y0
  # Account for U, common cause of Y1 and D 
  D ~~ Y1
"
sem(model = m3, data = df, estimator = "ML") %>%
  summary()


# Repeated samples --------------------------------------------------------

set.seed(9876)

sim_func = function(rho = 0.3, beta = 0.4, gamma = 0.6, theta = 0.5) {
  
  # Set large sample size
  n = 1000L
  
  # Time-invariant unobserved heterogeneity 
  V = rnorm(n, 0, 1)
  
  # Simulate initial realization of outcome
  Y0 = 1 * V + rnorm(n, 0, 1)
  
  # Time-varying confounder 
  U = rnorm(n, 0, 1)
  
  # Causal variable 
  D = gamma * U + rnorm(n, 0, 1)
  
  # Remaining realizations of outcome 
  Y1 = theta * U + rho * Y0 + 1 * V + rnorm(n, 0, 1)
  Y2 = beta  * D + rho * Y1 + 1 * V + rnorm(n, 0, 1)
  
  # Put into dataframe
  df = data.frame(Y0, Y1, Y2, D, V, U)
  
  # Fit the model 
  mx = "
    # Individual effects to account for V
    alpha =~ 1*Y1 + 1*Y2
    # Regressions 
    Y2 ~ beta*D + rho*Y1
    Y1 ~ rho*Y0
    # Allow initial outcome to correlate with unit effects
    alpha ~~ Y0
    # Account for U, common cause of Y1 and D 
    D ~~ Y1
  "
  mx.fit = sem(model = mx, data = df, estimator = "ML")
  
  # Get estimate of beta
  est = lavInspect(mx.fit, "list") %>%
    filter(label == "beta") %>%
    select(est) %>%
    as.numeric()
  
  # Return estimate of beta
  return(est)
}

res = replicate(n = 10000L, expr = sim_func())

mean(res); sd(res)
hist(res, main = NULL, xlab = NULL, breaks = 30)

