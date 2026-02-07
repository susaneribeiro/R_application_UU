## Replication of paper
##"Using Fit Statistic Differences to Determine
## the Optimal Number of Factors to Retain in an
## Exploratory Factor Analysis"

#📌 Monte Carlo simulation of parallel analysis accuracy under varying interfactor correlations

# Correlation matrix 

library(lavaan)
library(psych)

#generate model 
run_pa <- function(rho, N = 300, reps = 500) {
  model <- paste0("
  F1 =~ 0.35*x1 + 0.35*x2 + 0.35*x3 + 0.35*x4
  F2 =~ 0.35*x5 + 0.35*x6 + 0.35*x7 + 0.35*x8
  F1 ~~ ", rho, "*F2
  ")
  
  
  #generate data with Monte Carlo = replicate simulated data
  results <- replicate(reps, {
    dat <- simulateData(model, sample.nobs = N)
    pa  <- suppressMessages(fa.parallel(dat, plot = FALSE))
    pa$nfact
  })
  
  c(
    rho = rho,
    acc = mean(results == 2),
    under = mean(results == 1),
    over = mean(results == 3)
  )
}

rhos <- c(0.1, 0.3, 0.5, 0.7)
out <- t(sapply(rhos, run_pa))
out

df_res <- as.data.frame(out)

df_res 
library(ggplot2)

ggplot(df_res, aes(x = rho, y = acc)) +
  geom_line() +
  geom_point() +
  ylim(0, 1) +
  labs(
    x = "Interfactor correlation (ρ)",
    y = "Accuracy",
    title = "Parallel Analysis accuracy (factor loadings = 0.35)"
  )
