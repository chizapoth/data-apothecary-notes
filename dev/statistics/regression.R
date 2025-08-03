# regression analysis 
# model building, diagnostics, tests

library(car)
prestige <- carData::Prestige
data.table::setDT(prestige)

prestige

# linear regression ----

# x: education, income, women
# y: prestige 

plot(prestige)

# step 1, eda
# seems to have strong relationship between edu v pres, income v pres
# not so much women v pres

# check correlation 
cor(prestige$education, prestige$prestige) # 0.85
cor(prestige$income, prestige$prestige) # 0.71
cor(prestige$women, prestige$prestige) # -0.11

# step 2, build lm 

m1 <- lm(prestige ~ education + income + women, 
         data = prestige)

summary(m1)

plot(m1)


# model selection ----
# backward
m11 <- lm(prestige ~ education + income, 
          data = prestige)

summary(m11)


m10 <- lm(prestige ~ education, data = prestige)

# option 1: F-test
# anova to compare two models
anova(m1, m11)
anova(m1, m10)

# option 2: likelihood ratio test (not common in LR)
lmtest::lrtest(m1, m11) # not sig
lmtest::lrtest(m1, m10) # sig


# adding 'women' does not reduce the errors significantly
# while income is significant


AIC(m1, m11, m10) # m11 is the best

BIC(m1, m11, m10)

# collinearity  ----
?vif




# _______ ----
# logistic regression -----

mtcars
head(mtcars)
plot(mtcars$vs, mtcars$mpg)

mlr1 <- glm(vs ~ mpg + wt, family = 'binomial', data = mtcars)
summary(mlr1)

# z-values are the wald test statistics


mlr2 <- glm(vs ~ mpg, family = 'binomial', data = mtcars)
summary(mlr2)

# the null deviance are the same
# residual deviance slightly different
# we removed an insig variable so not surprising

anova(mlr2, mlr1, test = 'Chisq') # analysis of deviance


# goodness of fit
# pseudo r2
install.packages('pscl')
pscl::pR2(mlr1)
?pscl::pR2



# _______ -----
# poisson regression ----


# Load required library
library(MASS)

# Load data and inspect it
data(quine)
head(quine)

hist(quine$Days)

# Summarize the dependent variable (Days)
summary(quine$Days)

# Check the mean and variance of the count data (Days)
mean_days <- mean(quine$Days)
var_days <- var(quine$Days)

mean_days  # Mean of Days
var_days   # Variance of Days
# 16 vs 264

# Fit Poisson regression model
poisson_model <- glm(Days ~ Sex + Age, family = poisson, data = quine)
summary(poisson_model)

# Check goodness of fit (dispersion parameter)
# ssr / df
# df here is n-p
dispersion_poisson <- sum(residuals(poisson_model, type = "pearson")^2) / poisson_model$df.residual
dispersion_poisson  # If > 1, indicates overdispersion


# Fit Negative Binomial regression model
nb_model <- glm.nb(Days ~ Sex + Age, data = quine)
summary(nb_model)

# Compare AIC of the models
AIC(poisson_model, nb_model)

# Perform a likelihood ratio test
library(lmtest)
lrtest(poisson_model, nb_model)



# _______ -----
# cox regression ----

lung
head(lung)

coxm1 <- coxph(Surv(time, status) ~ age+sex, data = lung)

coxm2 <- coxph(Surv(time, status) ~ age, data = lung)

summary(coxm1)
summary(coxm2)

# model comparsion with lRT

anova(coxm1, coxm2, test = 'Chisq')

# log rank test, test survival curve differences
# sex is significant
survdiff(Surv(time, status) ~ sex, data = lung)
?survdiff


# test for proportional hazard assumption
cox.zph(coxm1)
# non significant, no covariate violated

# shoenfeld residual 
plot(cox.zph(coxm1))

# martingale residual 

res_mart <- residuals(coxm1, type = 'martingale')
plot(res_mart)

res_dev <- residuals(coxm1, type = 'deviance')
plot(res_dev)



# _________ ----
# anova ----

# Load data and inspect
data(mtcars)
head(mtcars)

# Convert 'cyl' (cylinder) into a factor
mtcars$cyl <- as.factor(mtcars$cyl)

boxplot(mpg ~ cyl, data = mtcars)

# Perform one-way ANOVA to see if mpg differs by cyl
anova_model <- aov(mpg ~ cyl, data = mtcars)
summary(anova_model)

# ANCOVA to add one continuous covariate
# Perform ANCOVA to compare mpg by cyl, adjusting for weight (wt)
ancova_model <- aov(mpg ~ cyl + wt, data = mtcars)
summary(ancova_model)

# Perform multiple linear regression with both cyl and wt as predictors
regression_model <- lm(mpg ~ cyl + wt, data = mtcars)
summary(regression_model)


# Define the MANCOVA model
# two responses: mgp and hp
# two covariates: cyl and wt
mancova_model <- manova(cbind(mpg, hp) ~ cyl + wt, data = mtcars)

# Summary of the MANCOVA model
summary(mancova_model)





