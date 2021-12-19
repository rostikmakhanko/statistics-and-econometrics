# On my honor, as a KSE student, I, Rostyslav Makhanko, will neither give nor receive any unauthorized help on this assignment.

library(wooldridge)
library(dplyr)
library(sandwich)
library(stargazer)
data(wage2)
?wage2

# 1
number_of_observations <- nrow(wage2)
print(number_of_observations)
number_of_variables <- ncol(wage2)
print(number_of_variables)

# 2
# There are three variables that have some NAs in observations:
# brthord, meduc, and feduc.
# The most "problematic" is feduc (father's education) variable, it has 194 NAs.
colSums(is.na(wage2))

# 3
# It is clear to see that married people's wage is more than for unmarried,
# and for black is less than for not black.
# The difference in average wages for married and unmarried is almost the same for
# all and black. Married earn around 150 more on average.
mean((wage2 %>% filter(married == 0))$wage) # 798.44
mean((wage2 %>% filter(married == 1))$wage) # 977.0479
mean((wage2 %>% filter(black == 0))$wage) # 990.6479
mean((wage2 %>% filter(black == 1))$wage) # 735.8417
mean((wage2 %>% filter(black == 1, married == 0))$wage) # 600.1111
mean((wage2 %>% filter(black == 1, married == 1))$wage) # 759.7941

# 4
# The coefficient of educ equals 0.074864 meaning that one additional
# year of education will increase the wage approximately by 7.5%
lwage_model_1 <- lm(data=wage2, lwage ~ educ + exper + tenure)
summary(lwage_model_1)

# 5
# I used a t-test and checked the p-value to reject the necessity to include the quadratic term.
lwage_model_1_quadratic_exper <- lm(data=wage2, lwage ~ educ + exper + exper^2 + tenure)
summary(lwage_model_1)

# 6
wage2$pareduc <- wage2$meduc + wage2$feduc
wage2_without_pareduc_na <- (wage2 %>% filter(is.na(pareduc) == FALSE))
lwage_model_2 <- lm(data=wage2_without_pareduc_na, lwage ~ educ + exper + tenure + educ*pareduc)
summary(lwage_model_2)

# 7
lwage_model_3 <- lm(data=wage2_without_pareduc_na, lwage ~ educ + exper + tenure)
summary(lwage_model_3)

# 8
wage2$educ_category = case_when(wage2$educ < 12 ~ 1,
                                wage2$educ >= 12 & wage2$educ < 16 ~ 2,
                                wage2$educ >= 16 & wage2$educ < 20 ~ 3,
                                wage2$educ > 20 ~ 4)
lwage_model_4 <- lm(data=wage2, lwage ~ educ_category + exper + tenure)
summary(lwage_model_4)

# 9
lwage_model_5 <- lm.beta(lwage_model_1)
summary(lwage_model_5)

# 10
# The person with the highest wage based on this regression is:
# - married,
# - not black,
# - lives in the city,
# - not from the south,
# - has as many as possible years of education and work experience.
# The person with the lowest wage based on this regression is:
# - not married,
# - black,
# - lives in the rural area,
# - from the south,
# - hasn't studied,
# - did not and does not work.
lwage_model_6 <- lm(data=wage2, lwage ~ educ + exper + tenure + married + black + urban + south)
summary(lwage_model_6)

# 11
# Coefficient of educ:black equals -0.022624, so indeed returns on education
# depend on race, and on average for black people additional year of education
# will result in decrease in the wage.
# Also, returns to education may depend on the region (south variable), because
# in different regions types of jobs may be different and some of them not
# necessarily require a lot of education.
lwage_model_7 <- lm(data=wage2, lwage ~ (educ * black) + exper + tenure + married + black + urban + south)
summary(lwage_model_7)

# 12

# 13

# 14
# Breusch-Pagan test says that p-value = 0.001259 < 0.05, so we can reject H0
# about homoscedasticity and say that there is a homoscedasticity of residuals.
bptest(lwage_model_6)

# 15

# 16
resettest(lwage ~ educ + exper + tenure + married + black + urban + south, power=2, type="regressor", data = wage2)

# 17

# 18

# 19
install.packages('L1pack')
library(L1pack)
lwage_lad <- lad(lwage_model_6)
summary(lwage_lad)
stargazer(lwage_model_6, type = "text")

# 20
# We can simply sort data by each of variables in wage2 and see there aren't any
# suspicious values, so we may conclude there are no outliers.
