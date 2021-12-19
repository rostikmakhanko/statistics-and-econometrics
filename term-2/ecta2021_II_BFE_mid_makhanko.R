# On my honor, as a KSE student, I will neither give nor receive any unauthorized help on this assignment.

library(wooldridge)
data(discrim)
discrim <- select(discrim, c(psoda, pfries, pentree, wagest, nmgrs, emp, chain, density, crmrte, prpblck, prppov, prpncar, nstores, income, hseval, lpsoda, lpfries, lhseval, lincome, ldensity, state ))
discrim<-discrim[complete.cases(discrim), ]
rnd<-round(runif(6, 1, 356))
discrim<-discrim[-c(rnd), ]
rm(rnd)
attach(discrim)  
?discrim

library(stargazer)

# 1
number_of_observations <- nrow(discrim)
print(number_of_observations)

# 2
# prpblck does not have units of measurement, because it is proportion
# income is measured in dollars
prpblck_average <- mean(discrim$prpblck)
income_average <- mean(discrim$income)
prpblck_standard_deviation <- sd(discrim$prpblck)
income_standard_deviation <- sd(discrim$income)
cat(prpblck_average, prpblck_standard_deviation)
cat(income_average, income_standard_deviation)

# 3
# Income is normally distributed on range from 0 to 90k.
# Also, we can see that there are some extremely wealthy households with income more than 120k.
hist(discrim$income)

# 4
soda_model <- lm(data=discrim, psoda ~ prpblck + income)
summary(soda_model)
stargazer(soda_model, type="text")

# 5
soda_model_on_prpblck <- lm(data=discrim, psoda ~ prpblck)
summary(soda_model_on_prpblck)
stargazer(soda_model_on_prpblck, type="text")

# 6
soda_model_with_logs <- lm(data=discrim, log(psoda) ~ prpblck + log(income))
summary(soda_model_with_logs)
stargazer(soda_model_with_logs, type="text")

# 7
# R-squared has not change much, being equal to around 0.07 for both models.

# 8
# Adding prppov to the regression will lead to prpblack coefficient decrease near two times.
# Because psoda correlates with prppov and prpblack than with prpblack only.
soda_model_with_logs_and_prppov <- lm(data=discrim, log(psoda) ~ prpblck + log(income) + prppov)
summary(soda_model_with_logs_and_prppov)
stargazer(soda_model_with_logs_and_prppov, type="text")

# 9
# Log income and prpblack correlates the most.
cor(log(income), prpblck)
cor(log(income), prppov)
cor(prppov, prpblck)