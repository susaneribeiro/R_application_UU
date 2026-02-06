#Linear regression 
library(dplyr)
library(ggplot2)
library(car)        # VIF
library(lmtest)     # bptest, dwtest
library(performance) # check_model
library(sjPlot)     # tab_model
library(broom)      # tidy output
 

dataset <- read_csv("Exam_Score_Prediction.csv")
dataset$study_method <- factor(dataset$study_method)
levels(dataset$study_method) 

table(dataset$study_method) # to understand the dataset 
dataset %>% count(study_method) # alternative way with pipe 

# setting intercept to "self-study"
dataset$study_method <- relevel(dataset$study_method, ref = "self-study")

fit <- lm(exam_score ~ study_method, data = dataset)

summary(fit)
tab_model(fit, show.ci = TRUE, show.se = TRUE, show.stat = TRUE)



##Assumptions
# Linearity + homoscedasticity (visual)
res <- residuals(fit)

plot(fitted(fit), res,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(0, 0)

#Homoscedasticity (formal)
bptest(fit)

# Normality of residuals
qqnorm(res)
qqline(res)
# Multicollinearity
vif(fit)

# Influential points
plot(fit, which = 4)  # Cook's distance

