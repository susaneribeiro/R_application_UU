# Dataset: Andy Field SPSS Anxiety Questionnaire (SAQ)
# PART 1 OF 
# tutorial: https://stats.oarc.ucla.edu/r/seminars/rcfa/#s1a 
# PART 2: UNCORRELATED FACTORS 
# commenting section cmd + shift + c 
# run until line cmd + option + B 
# run all cmd + shift + enter 
# Statistics makes me cry
# 1 My friends will think I’m stupid for not being able to cope with SPSS
# 2 Standard deviations excite me
# 3 I dream that Pearson is attacking me with correlation coefficients
# 4 I don’t understand statistics
# 5 I have little experience with computers
# 6 All computers hate me
# 7 I have never been good at mathematics
library(foreign) 
library(lavaan)
library(haven)
dat <- read.spss("saq.sav", to.data.frame = TRUE, use.value.labels = FALSE )
# use.value.labels = FALSE converts categorical variables 
# to numeric values rather than factor\
# to.data.frame = TRUE converts to data frame 
head(dat)
summary(dat)

round(cor(dat[, 1:8]), 2)

# Let's use 3:5 items = 3 items 
# known values = p(p+1)/2 = 6 

round(cov(dat[,3:5]), 2)

# we are looking for 10 parameters: 
# 3 lambdas 
# var 
# 6 residues (it would be 9, but the matrix is simmetrical)

# df = N unique parameters - N fixed parameters 
# df negative, known < free (under-identified, bad)
# df = 0, known = free (just identified or saturated, neither bad nor good)
# df positive, known > free (over-identified, good)

# Initially: df = 10 - 6 = -4 
# we fix: lambda1 = 1 
# we set 3 unique residual COV = 0 
# Finally: df = 6 - 6 = 0 (just identified or saturated)

# Identification of a three-item one factor CFA
# marker method: lmabda1 = 1 
# variance standardization method var = 1

# ~ predict, used for regression of observed outcome to observed predictors
# =~ indicator, used for latent variable to observed indicator in factor analysis measurement models
# ~~ covariance
# ~1 intercept or mean (e.g., q01 ~ 1 estimates the mean of variable q01)
# 1* fixes parameter or loading to one
# NA* frees parameter or loading (useful to override default marker method)
# a* labels the parameter ‘a’, used for model constraints

#we use these columns as items for one factor:
m1a <- 'f =~ q03 + q04 + q05'
#onefac3items_a <- cfa(m1a, data = dat)
#summary(onefac3items_a)

# lavaan automatically does marker method: lambda1 = 1 
# put NA* in front of parameter to free it: 
m1b <- 'f =~ NA*q03 + q04 + q05
        f ~~ 1*f' #creates variance standardization var =1 
onefac3items_b <- cfa(m1b, data = dat)
summary(onefac3items_b)

# f ~~ 1*f <-> std.lv = TRUE (standardize latent variable variance)
#onefac3items_a <- cfa(m1a, data = dat, std.lv = TRUE)
#summary(onefac3items_a)

onefac3items_a <- cfa(m1a, data = dat)
summary(onefac3items_a, fit.measures = TRUE, standardized = TRUE)

standardizedsolution(onefac3items_a) #it does not show fit indices

#To convert from Std.lv (which standardizes the X or the latent variable) 
#to Std.all we need to divide by the implied standard deviation 
# of each corresponding item. 

###############################################################################
# --------------------- one factor, two items, equal lambda ------------------#
# this way we can use 2 items for one factor: 
m2b <- 'f1 =~ a*q04 + a*q05'
onefac2items_b <- cfa(m2b, data = dat, std.lv = TRUE)
summary(onefac3items_b)

#now let's use eight itmes for one factor 
m3a <- 'f =~ q01 + q02 + q03 + q04 + q05 + q06 + q07 + q08'
onefact3items_8 <- cfa(m3a, data = dat, std.lv = TRUE)
summary(onefact3items_8, fit.measures = TRUE, standardized = TRUE)

###############################################################################
# --------------------- NUMBER OF MODEL PARAMETERS ---------------------------#
# missing 8 lambdas, 8 residual var (VarErr11, VarErr22...), Var_factor 
# if we fix var_Factor = 1: 
#N_free = total - fixed = 17 - 1 = 16 NUMBER OF MODEL PARAMETERS 

###############################################################################
# --------------------- DEGREES OF FREEDOM -----------------------------------#
#df = known parameters - free parameters 
# com 8 items, temos known parameters p(p+1)/2 = 8*9/2 = 36 
# df = 36 - 16 = 20 

# CONTINUE IN PART 2: UNCORRELATED FACTORS
