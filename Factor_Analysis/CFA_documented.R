# Confirmatory Factor Analysis (CFA)
# Construct: School enjoyment / School satisfaction
#
# Items:
# item1: Ik ben blij, dat ik op deze school zit.
# item2: Ik heb zin om naar school te gaan.
# item3: Ik vind dat wij op school leuke dingen doen.
# item4: Ik vind het leuk op school.
# item5: Ik krijg leuk les op school.
# item6: Ik heb plezier in werken op school.
# item7: Ik zit liever op een andere school.          (negatively worded)
# item8: De lessen op school vind ik vervelend.        (negatively worded)
#
# Response scale:
# 1 = dat is zo
# 2 = soms wel
# 3 = soms niet
# 4 = dat is niet zo
#
# 1 = meisje, 2 = jongen 
#
# Higher scores indicate lower levels of school enjoyment.
# Negatively worded items may require reverse coding prior to analysis.


library(lavaan)
library(ggplot2)
library(dplyr)
library(haven)
library(semTools)
library(psych)



D <- read.table(
  "Schoolplezier.dat",
  header = TRUE,
  sep = "\t"
)


# ------------------------------------------------------------
# Recoding items to ensure consistent directionality
# ------------------------------------------------------------
#
# The original response scale is coded as follows:
# 1 = dat is zo
# 2 = soms wel
# 3 = soms niet
# 4 = dat is niet zo
#
# In the original coding, lower scores indicate more positive school enjoyment.
# For the CFA and subsequent analyses, items are recoded so that:
# higher scores indicate more school enjoyment,
# and lower scores indicate less school enjoyment.


# ------------------------------------------------------------
# Option 1: Explicit recoding using case_match()
# ------------------------------------------------------------
#
# This approach explicitly maps each original response category
# to its reversed value. It is very transparent and easy to verify.

# D$item1 <- case_match(D$item1, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)
# D$item2 <- case_match(D$item2, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)
# D$item3 <- case_match(D$item3, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)
# D$item4 <- case_match(D$item4, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)
# D$item5 <- case_match(D$item5, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)
# D$item6 <- case_match(D$item6, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)


# ------------------------------------------------------------
# Option 2: Mathematical reverse coding
# ------------------------------------------------------------
#
# For a 4-point Likert scale, reverse coding can also be done by:
# new_score = (maximum + 1) - original_score
#
# Since the maximum value is 4, the transformation becomes:
# new_score = 5 - original_score
#
# This approach is more concise but relies on the assumption
# that the scale runs consecutively from 1 to 4.

# D$item1 <- 5 - D$item1
# D$item2 <- 5 - D$item2
# D$item3 <- 5 - D$item3
# D$item4 <- 5 - D$item4
# D$item5 <- 5 - D$item5
# D$item6 <- 5 - D$item6

# Simplifying in one step, we create the matrix with the recoded items: 
item_rec <- 5 - D[,5:10] 

# ------------------------------------------------------------
# Quick sanity check: compare a recoded column with the original item
# ------------------------------------------------------------
#
# This binds the first recoded column next to the original item1 so that
# you can visually confirm that the recoding behaves as expected.

cbind(item_rec[,1], D$item1)

# ------------------------------------------------------------
# Preserve item names
# ------------------------------------------------------------
#
# After creating item_rec from a slice of D, we explicitly copy the original
# column names so the items remain identifiable in later analyses.

colnames(item_rec) <- colnames(D[, 5:10])

# ------------------------------------------------------------
# Build a new recoded dataset (Drec)
# ------------------------------------------------------------
#
# Drec combines:
# - the first four columns of D (e.g., ID/demographics or other variables)
# - the recoded item block (item_rec)
# - the remaining item columns 11:12 (kept as in D)
#
# The result is a data frame where the school enjoyment items are aligned
# in the intended direction (higher = more enjoyment).

Drec <- cbind(D[, 1:4], item_rec, D[,11:12])
#head(Drec)

# ------------------------------------------------------------
# Interpretation after recoding
# ------------------------------------------------------------
#
# After recoding, all eight school enjoyment items are coded
# in the same direction:
#
# - High scores indicate high levels of school enjoyment
# - Low scores indicate low levels of school enjoyment
#
# If multiple items are assumed to measure the same underlying construct,
# they are expected to show positive statistical associations.
#
# Children with high school enjoyment are expected to score high on all items,
# whereas children with low school enjoyment are expected to score low on all items.
#
# If the associations between items are very weak, it is unlikely that
# they measure the same latent construct.


# ------------------------------------------------------------
# Inspecting correlations between item scores
# ------------------------------------------------------------
#
# Before constructing a test score or fitting a factor model,
# it is useful to inspect the correlations between all item scores.
#
# Correlations between all item scores:
corr <- cor(Drec[, 5:12], use = "pairwise")

round(cor(Drec[, 5:12], use = "pairwise"), 3)

# ------------------------------------------------------------
# Inspecting extreme correlations in the correlation matrix
# ------------------------------------------------------------
#
# To better understand the correlation matrix, we identify the
# weakest and strongest associations between item pairs.
#
# The diagonal of a correlation matrix contains correlations of
# variables with themselves (value = 1). These are not informative
# for this purpose and are therefore set to NA.

diag(corr) <- NA


# ------------------------------------------------------------
# Minimum correlation
# ------------------------------------------------------------
#
# Identify the smallest (most negative or weakest) correlation
# between any two items, excluding the diagonal.

min(corr, na.rm = TRUE)
max(corr, na.rm = TRUE)

# Locate the indices (row and column) of the minimum correlation
# arr.ind = TRUE returns the matrix coordinates instead of a single index.

idmin <- which(corr == min(corr, na.rm = TRUE), arr.ind = TRUE)
idmin


# ------------------------------------------------------------
# Maximum correlation
# ------------------------------------------------------------
#
# Identify the strongest correlation between any two items,
# excluding the diagonal.

idmax <- which(corr == max(corr, na.rm = TRUE), arr.ind = TRUE)
idmax


###########################################################################
# Confirmatory Factor Analysis (CFA) using the lavaan package
#
# In general, three main steps can be distinguished when conducting an analysis
# with the lavaan package:
#
# 1. Specify the model using lavaan syntax
# 2. Fit the specified model to the data
# 3. Inspect and interpret the results
#
# Note: In this script we use Drec, a recoded version of the dataset in which
# the school enjoyment items are aligned in the same direction
# (higher scores indicate higher school enjoyment).


# ------------------------------------------------------------
# Step 1: Model specification
# ------------------------------------------------------------
#
# A two-factor confirmatory factor model is specified using lavaan syntax.
# Each latent factor is defined by a set of observed items.

model1 <- "

f1 =~ item1 + item2 + item4 + item7

f2 =~ item3 + item5 + item6 + item8
"


# ------------------------------------------------------------
# Step 2: Fit the model to the data
# ------------------------------------------------------------
#
# The specified model is now fitted to the data using the cfa() function
# from the lavaan package.
#
# The cfa() function performs a confirmatory factor analysis.
# Although the function has many arguments, only two are used here:
#
# - model: the object containing the model specification (model1)
# - data:  the data frame containing the observed variables (Drec)

fit <- cfa(model = model1, data = Drec)


# NOTE:
# It is important to use the exact object names when fitting the model.
# For example, using an incorrect model name (e.g., model10 instead of model1)
# will result in an error or unintended behavior.

# fit <- cfa(model = model10, data = Drec)   # incorrect
# fit <- cfa(model = model1,  data = Drec)   # correct


# If the model is fitted successfully, the object "fit" will appear
# in the Environment panel.


# ------------------------------------------------------------
# Step 3: Request and inspect the results
# ------------------------------------------------------------
#
# Model results are obtained using the summary() function.
# By setting fit.measures = TRUE, overall model fit indices are also requested.

summary(fit, fit.measures = TRUE)

# ------------------------------------------------------------
# Commonly used fit index guidelines (rule-of-thumb cutoffs)
# ------------------------------------------------------------
#
# CFI (Comparative Fit Index)
#   >= .95  -> good fit
#   >= .90  -> acceptable fit
#
# TLI (Tucker–Lewis Index)
#   >= .95  -> good fit
#   >= .90  -> acceptable fit
#
# RMSEA (Root Mean Square Error of Approximation)
#   <= .05        -> good fit
#   .05 – .08     -> acceptable fit
#   >= .10        -> poor fit
#
# SRMR (Standardized Root Mean Square Residual)
#   <= .08  -> good fit
#
# Note:
# According to commonly used guidelines, RMSEA values of .05 or lower indicate
# a good fit, while values above .10 indicate poor fit.
# In this analysis, the RMSEA suggests approximately acceptable fit, whereas
# the SRMR indicates good fit.


# ------------------------------------------------------------
# Interpretation / conclusion (example write-up)
# ------------------------------------------------------------
#
# We fitted a standard confirmatory two-factor model to the data using the lavaan package.
# Based on the chi-square goodness-of-fit test, χ²(19) = 38.619, p = 0.005, we reject
# the null hypothesis at α = 0.05 and conclude that the model does not fit the data well.
#
# The relative fit indices, CFI = 0.925 and TLI = 0.889, suggest moderate fit (neither
# very good nor very poor). The absolute fit index RMSEA = 0.071 also indicates acceptable,
# but not excellent, model fit. In contrast, the SRMR = 0.052 indicates good fit.
#
# Given the significant chi-square test in combination with a sample size that is not
# extremely large (N = 205), and fit indices that do not consistently indicate good fit,
# we conclude that the standard confirmatory two-factor model does not fit the data
# sufficiently well. Further analyses of the underlying structure of these items are
# therefore warranted.


# ------------------------------------------------------------
# Model identification and estimation in CFA (lavaan)
# ------------------------------------------------------------
#
# In CFA, model identification can be achieved in different ways.
# Two common approaches in lavaan are:
#
# 1. Unit loading identification (default in lavaan)
# 2. Unit variance identification (std.lv = TRUE)
#
# Below, both approaches are applied to the same model and dataset (Drec)
# to illustrate their use and interpretation.


# ------------------------------------------------------------
# Unit loading identification (default in lavaan)
# ------------------------------------------------------------
#
# By default, lavaan identifies the model by fixing one factor loading
# per latent variable to 1. This sets the scale of the latent factor
# in terms of one of its observed indicators.

fit1 <- cfa(model1, data = Drec)

# Request model fit indices, standardized estimates, and R-squared values
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Optional reliability analyses
# reliability(fit1)   # (commented out in this script)

# Composite reliability (CR) for each latent factor
compRelSEM(fit1)

# Average Variance Extracted (AVE) for each latent factor
AVE(fit1)


# ------------------------------------------------------------
# Unit variance identification (std.lv = TRUE)
# ------------------------------------------------------------
#
# In unit variance identification, the variance of each latent factor
# is fixed to 1. As a result, all factor loadings are freely estimated.
#
# This approach leads to standardized latent variables and facilitates
# interpretation of factor loadings in standardized units.

fit2 <- cfa(model1, data = Drec, std.lv = TRUE)

# Again, request fit indices, standardized estimates, and R-squared values
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Average Variance Extracted (AVE) under unit variance identification
AVE(fit2)


# ------------------------------------------------------------
# Modification indices
# ------------------------------------------------------------
#
# Modification indices indicate how much the overall model fit would improve
# if a fixed parameter were freely estimated.
#
# They are typically used as diagnostic tools to identify potential
# model misspecifications, such as cross-loadings or correlated residuals.
#
# Modification indices should be interpreted cautiously and always in light
# of theoretical considerations.

modificationindices(fit1)


# ------------------------------------------------------------
# Total variance explained (beyond item-level R² / communalities)
# ------------------------------------------------------------
#
# In addition to the explained variance of individual items (R² / communalities),
# it is also useful to examine the proportion of variance explained at the factor level.
# This indicates how much of the variance in the indicators is captured by each factor.
#
# In R, factor-level explained variance can be requested using semTools.
# The function reliability() provides several reliability-related indices.
#
# In the output, the row labeled 'avevar' contains the total proportion of
# explained variance per factor in the model.

#reliability(fit1) # This proposed function from grasple did NOT work

# ------------------------------------------------------------
# Reliability and AVE (updated semTools workflow)
# ------------------------------------------------------------

# The recommended replacement is:
# - compRelSEM(): model-based reliability (and optional alpha, depending on settings)
# - AVE(): Average Variance Extracted (reported separately because AVE is not a reliability coefficient)

compRelSEM(fit1)
AVE(fit1)


# ------------------------------------------------------------
# Manual calculation: overall proportion of variance explained (all factors)
# ------------------------------------------------------------
#
# Computing a single overall proportion of explained variance for the entire model
# (all factors combined) is not directly provided as a simple one-line output in many
# standard summaries. A straightforward manual approach is:
#
# 1) Take the explained variance per factor (avevar).
# 2) Multiply each factor's avevar by the number of items loading on that factor.
# 3) Sum these products across factors.
# 4) Divide by the total number of items in the model.
#
# Example (three factors):
#   factor 1 (3 items) = 0.43
#   factor 2 (3 items) = 0.52
#   factor 3 (4 items) = 0.45
#
# Overall proportion of explained variance:
#   ((0.43 * 3) + (0.52 * 3) + (0.45 * 4)) / 10 = 0.465

# ------------------------------------------------------------
# Evaluating model improvements: cross-loadings and correlated residuals
# ------------------------------------------------------------
#
# Modification indices may suggest potential improvements to model fit.
# However, improvements should always be evaluated critically, taking into
# account both the magnitude of the improvement and its theoretical plausibility.
#
# Adding parameters solely based on statistical criteria may lead to overfitting
# and poor generalizability.
#
# Typical reasons for correlated residuals include:
# - Overlapping item content
# - Similar item wording or formulation
# - Method effects (e.g., parallel items, perspective effects such as "I" vs "we")
#
# Correlated residuals should only be added when they are theoretically and
# empirically justifiable.


# ------------------------------------------------------------
# Cross-loading and correlated residual model specification
# ------------------------------------------------------------
#
# In this extended model:
# - item6 is allowed to load on both factors (cross-loading)
# - a correlated residual is specified between item6 and item2
#
# This specification is based on modification indices and theoretical inspection
# of item content and wording.

modelcross <- "
# specifying factors
f1 =~ item1 + item2 + item4 + item7 + item6
f2 =~ item3 + item5 + item6 + item8

# add residual correlation / correlated residual
item6 ~~ item2
"

# Fit the extended model using unit variance identification
fitcross <- cfa(modelcross, data = Drec, std.lv = TRUE)


# ------------------------------------------------------------
# Inspecting model fit and parameter estimates
# ------------------------------------------------------------
#
# Request overall model fit indices, standardized estimates, and R-squared values
# to evaluate whether the extended model improves fit in a meaningful way.

summary(fitcross, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Average Variance Extracted (AVE) for each factor
AVE(fitcross)


# ------------------------------------------------------------
# Exploratory comparison using EFA
# ------------------------------------------------------------
#
# As an exploratory complement, a two-factor EFA is estimated to inspect
# the underlying structure of the items and compare it with the CFA solution.
#
# This can help assess whether cross-loadings or correlated residuals observed
# in CFA are also reflected in an exploratory framework.

fa.diagram(fa(Drec[, 5:12], nfactors = 2))


# ------------------------------------------------------------
# Item-level explained variance (communalities)
# ------------------------------------------------------------
#
# Communalities indicate how much variance of each item is explained by the factors.
# Low communalities may suggest that an item is poorly represented by the factor model.

fa(D[, 5:12], nfactors = 2)$communality   # variance explained per item


# ------------------------------------------------------------
# Total variance explained by the factor solution
# ------------------------------------------------------------
#
# The proportion of variance explained by the factors is reported below.
# This provides an indication of how well the factor solution captures
# the overall variance in the data.

fa(D[, 5:12], nfactors = 2)$Vaccounted     # proportion of variance explained
