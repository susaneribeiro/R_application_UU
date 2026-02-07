## Replication of paper
##"Using Fit Statistic Differences to Determine
## the Optimal Number of Factors to Retain in an
## Exploratory Factor Analysis"

library(lavaan)

model <- '
F1 =~ 0.6*x1 + 0.6*x2 + 0.6*x3 + 0.6*x4
F2 =~ 0.6*x5 + 0.6*x6 + 0.6*x7 + 0.6*x8
F1 ~~ 0.3*F2
'

dat <- simulateData(model, sample.nobs = 300)
head(dat)

library(psych)

fa.parallel(dat)     # parallel analysis
fa(dat, nfactors=2)  # EFA com 2 fatores
fa(dat, nfactors=3)  # testa errado de propósito

# Monte Carlo Loop 
results <- replicate(500, {
  dat <- simulateData(model, sample.nobs = 300)
  pa <- suppressMessages(fa.parallel(dat, plot = FALSE))
  pa$nfact
})
mean(results == 2) 

table(results)
str(results)
results


efa2 <- fa(
  r = dat,
  nfactors = 2,
  fm = "ml",        # ou "pa" (principal axis). "ml" combina com dados normais
  rotate = "oblimin" # oblíqua (permite correlação entre fatores)
)

print(efa2, sort = TRUE, cutoff = 0.30)
efa2$Phi   # correlação entre fatores (porque rotação é oblíqua)
efa2
efa2$RMSEA
efa2$rms

efa3 <- fa(dat, nfactors = 3, fm = "ml", rotate = "oblimin")
print(efa3, sort = TRUE, cutoff = 0.30)

