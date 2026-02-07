install.packages("kableExtra")
library(lavaan)
library(ggplot2)
library(dplyr)
library(haven)
library(lavaan)
library(semTools)
library(psych)
library(kableExtra) 


EFA <- read.table("Schoolplezier.dat", header = TRUE, sep = "\t")
head(EFA)

efa1 <- fa(EFA[, 5:12], nfactors = 1, fm = "ml")
efa1 
efa1$CFI
#Chi squared: function STATISTIC ou "empirical Chi squared"
efa1$STATISTIC
efa1$dof
efa1$PVAL
efa1$TLI

#model1_efa <- list(efa1)
#compare_efa_fit(modellen_efa)

fa.parallel(EFA[,5:12], fm = "ml", fa = "fa")
# Parallel analysis suggests that the number of factors = 1

efa2 <- fa(EFA[,5:12], nfactors = 2, rotate = "oblimin", fm = "ml")
efa2
efa3 <- fa(EFA[,5:12], nfactors = 3, rotate = "oblimin", fm = "ml")

models_efa <- list(efa1, efa2, efa3)
#compare_efa_fit(models_efa)

efa2Orth <- fa(EFA[,5:12], nfactors = 2, rotate = "varimax", fm = "ml")
efa2Orth 

# Assumpties checken
dat <- HolzingerSwineford1939
HS.model <- ' visual =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed =~ x7 + x8 + x9 '
fitHS <- cfa(HS.model, data=dat, missing = "ML")

summary(fitHS)
sum(is.na(dat))
head(dat)

boxplot(dat$x1)

ggplot(dat, aes(y = x1)) +
  geom_boxplot(fill = "lightgray") +
  theme_minimal()


