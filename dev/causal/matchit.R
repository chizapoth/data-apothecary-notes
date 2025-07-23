# install.packages('MatchIt')
library(MatchIt)

# load data
data <- read.csv('./dev/causal/ps_data.csv')
head(data)


# matchit ----

head(lalonde)
lalonde$treat |> table() # 0:429; 1:185

# fit a lr with all the variables
# set the method to NULL since we assess balance before matching

m0 <- matchit(treat ~ age + educ + race + married + 
                nodegree + re74 + re75,
              data = lalonde,
              method = NULL,
              distance = "glm")

summary(m0)

# check 'distance'
# std mean diff should be close to 0
# var ratio should be close to 1


# perform matching
# 1:1 nearest neighbor, appropriate for ATT
# each tr unit is paired with another with similar PS
# any other ct is left unmatched 

# 1. NN matching ----
# 1:1 NN PS matching w/o replacement
m1 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75,
                  data = lalonde,
                  method = "nearest", # set method
                  distance = "glm")


summary(m1)
# now 185:185 tr ct
# 244 ct unmatched

m1$weights # membership, 1 or 0
m1$weights |> table() # 244 unmatched, 370 (185:185 tr ct)

m1$subclass
# subclass is the membership pairs

# the balance is still not good (based on the distance and var ratio)

plot(m1, type = "jitter", interactive = FALSE)
plot(m1, type = "density", interactive = FALSE,
     which.xs = ~age + married + re75)




# 2. full matching ----
# Full matching on a probit PS
# (probably: instead of 0/1 weights, assign other weights)
m2 <- matchit(treat ~ age + educ + race + married + 
                    nodegree + re74 + re75,
                  data = lalonde,
                  method = "full",
                  distance = "glm",
                  link = "probit")
m2

# check balance
# the metrics look better
summary(m2, un = FALSE)

# Love plot 
plot(summary(m2))


# match data and analyse ----
# the full matching is good, so extract the matched data
m.data <- match_data(m2)

head(m.data)
# weights here can be above 1
# m.data$weights |> hist()


# install.packages('marginaleffects')
library("marginaleffects")

# plot(lalonde$re78)
# use lm
# add weights
fit <- lm(re78 ~ treat * (age + educ + race + married +
                            nodegree + re74 + re75),
          data = m.data,
          weights = weights)

?avg_comparisons
# put treatment =1, treated effects 
# ~subclass gets passed onto sandwich estimator
avg_comparisons(fit,
                variables = "treat",
                vcov = ~subclass,
                newdata = subset(treat == 1))





# my example ----





