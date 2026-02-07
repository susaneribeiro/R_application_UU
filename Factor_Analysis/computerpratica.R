#### Practicum A&E: "Inleiding R en Rstudio" ####


library(lavaan)
library(ggplot2)
library(dplyr)
library(haven)
library(semTools)
library(psych)

### 1 = meisje, 2 = jongen 

D <- read.table(
  "Schoolplezier.dat",
  header = TRUE,
  sep = "\t"
)


#write.csv(D, "dados.csv", row.names = FALSE)

summary(D)
head(D)
dim(D)
D[D$kindnr == 100,]

#D[order(D$kindnr),]
#D[order(D$sekse, D$kindnr), ] = ordena primeiro por sexo, depois por kinnumber

#D[order(D$sekse,D$kindnr), ]


#D[order(D$leeftijd),]

summary(D$leeftijd)

#Quantos NA existem
sum(as.character(D$item2) == "1", na.rm = TRUE)
sum(D$item2 == "1", na.rm = TRUE)
sum(is.na(D$sekse))
dim(D)
apply(D[, 5:12], 2, sd, na.rm = TRUE)
order(apply(D[, 5:12], 2, mean, na.rm = TRUE))

D[D$kindnr == 50, ]
D$groep[D$kindnr == 50]
D[D$kindnr == 50, c("groep", "sekse")]

table(D$sekse, useNA = "always")

apply(D[, 5:12], 2, table, useNA = "always")
apply(D[, 5:12], 2, mean, na.rm = TRUE)
order(apply(D[, 5:12], 2, mean, na.rm = TRUE))

apply(D[, 5:12], 2, sd, na.rm = TRUE)

order(apply(D[, 5:12], 2, sd, na.rm = TRUE))
order(apply(D[, 5:12], 2, mean, na.rm = TRUE))

mean(D$leeftijd, na.rm = TRUE)
sd(D$leeftijd, na.rm = TRUE)

sum(D$item5 == 2, na.rm=TRUE)

sum(is.na(D$item1))
colSums(is.na(D[,5:12]))


summary(D$leeftijd)
table(D$sekse)
sd(D$leeftijd, na.rm =T)
apply(D[, 5:12], 2, sd, na.rm = TRUE)
order(apply(D[,5:12], 2, mean, na.rm = TRUE))
sum(D$item5 == 2, na.rm = TRUE)
table(D$item5)


apply(D[, 5:12], 2, sd,na.rm = TRUE)

order(apply(D[, 5:12], 2, sd, na.rm = TRUE))

table(D$item5)
sum(D$item5 == 2, na.rm = TRUE)

it1 <- case_match(D$item1, 1 ~ 4, 2 ~ 3, 3 ~ 2, 4 ~ 1)
it2 <- (4 + 1) - D$item2
?case_match

item_rec <- 5 - D[,5:10] 

cbind(item_rec[,1], D$item1)
colnames(item_rec) <- colnames(D[, 5:10])

Drec <- cbind(D[, 1:4], item_rec, D[,11:12])
#head(Drec)

#understanding how to get means, sum of NA, sd, from each column 
colSums(is.na(Drec[,5:12]))
colMeans(Drec[ ,5:12], na.rm = TRUE)
order(colMeans(Drec[ , 5:12], na.rm = TRUE))

sds <- apply(Drec[, 5:12], 2, sd, na.rm = TRUE)
sds
order(sds)

round(cor(Drec[, 5:12], use = "pairwise"), 3)

cor(D$item1, D$item2, use = "pairwise")

corr <- cor(Drec[, 5:12], use = "pairwise")
diag(corr) <- NA

min(corr, na.rm = TRUE)
max(corr, na.rm = TRUE)
idx <- which(corr == min(corr, na.rm = TRUE), arr.ind = TRUE)
idx

idmax <- which(corr == max(corr, na.rm = TRUE), arr.ind = TRUE)
idmax

model1 <- "
f1 =~ item1 + item2 + item4 + item7
f2 =~ item3 + item5 + item6 + item8
"
##Unit loading identification, default in lavaan 
fit1 <- cfa(model1, data = Drec)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_cross, fit.measures = TRUE, standardized = TRUE)
#reliability(fit1)
compRelSEM(fit1)
AVE(fit1)

#unit variance identification: std.lv = TRUE  
fit2 <- cfa(model1, data = Drec, std.lv = TRUE)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
AVE(fit2)

modificationindices(fit1) 

#crossloading modification - item6
modelcross <- "
#specifying factors
f1 =~ item1 + item2 + item4 + item7  + item6
f2 =~ item3 + item5 + item6 + item8
#add residual correlation / residual variance
item6 ~~ item2
"
fit_cross <- cfa(modelcross, data = Drec, std.lv = TRUE)


summary(fit_cross, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
AVE(fit_cross)
fa.diagram(fa(Drec[, 5:12], nfactors = 2))


fa(D[, 5:12], nfactors = 2)$communality #variance explained per item
fa(D[, 5:12], nfactors = 2)$Vaccounted #proportion of variance explained
