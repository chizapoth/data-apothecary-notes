# causal diagrams
# psw (propensity score weighting)
# psm (propensity score matching)
# ---
# RCT
# g-methods
# IV (instrumental variables)

# install.packages('causaldata')
library(causaldata)
library(dplyr)


# 1556 x 67
?nhefs_complete
nhefs_uc <- dplyr::filter(nhefs_complete, censored == 0)
nhefs_uc

colnames(nhefs_uc)
# outcome: weight (wt82_71)
# exposure: quit smoking (qsmk)

nhefs_uc |> group_by(qsmk) |> 
  summarise(
    mean_weight_change = mean(wt82_71), 
    sd = sd(wt82_71), 
    .groups = 'drop'
  )


# regression -----
# add everything
lm( 
  wt82_71~ qsmk + sex +  
    race + age + I(age^2) + education + 
    smokeintensity + I(smokeintensity^2) + 
    smokeyrs + I(smokeyrs^2) + exercise + active + 
    wt71 + I(wt71^2), 
  data = nhefs_uc 
) |> summary()
  # tidy(conf.int = TRUE) |>
  # filter(term == "qsmk")

# coeff for qsmk is 3.46

# counterfactual: what if EVERYONE quits smoking vs
# what if NO ONE quits smoking 

# propensity model ----
# y: qsmk (model the propensity of exposure)

propensity_model <- glm( 
  qsmk ~ sex +  
    race + age + I(age^2) + education + 
    smokeintensity + I(smokeintensity^2) + 
    smokeyrs + I(smokeyrs^2) + exercise + active + 
    wt71 + I(wt71^2), 
  family = binomial(), 
  data = nhefs_uc
)

# a weight is estimated for each observation

propensity_model
pred_prob <- predict.glm(propensity_model, type = 'response')
pred_prob |> hist()


# create weight vector 
w <- data.frame(qsmk = nhefs_uc$qsmk, pred_prob)
head(w)
# for tx, weight is 1/prop
# for ct, weight is 1/(1-prop)
w$w <- 1/w$pred_prob
w$w[w$qsmk == 0] <- 1/(1 - w$pred_prob[w$qsmk == 0])
hist(w$w, breaks = 30)

# propensity score for qsmk == 1 and 0
pred_prob[which(nhefs_uc$qsmk == 1)] |> hist()
pred_prob[which(nhefs_uc$qsmk == 0)] |> hist()

ipw_model <- lm(wt82_71 ~ qsmk, 
                data = nhefs_uc, 
                weights = w$w)

ipw_model |> summary()
# 3.44 rather than 3.46



# by simulation -----

n <- 1000 
sim <- tibble(
  confounder = rbinom(n, 1, 0.5),
  # prob of exposure depends on confounder
  p_exposure = case_when(
    confounder == 1 ~ 0.75,
    confounder == 0 ~ 0.25
  ),
  exposure = rbinom(n, 1, p_exposure),
  outcome = confounder + rnorm(n)
)
sim |> head()
plot(sim$exposure, sim$outcome)
lm(outcome ~ exposure, data = sim)


sim |> 
  group_by(exposure) |> 
  summarise(avg_y = mean(outcome))
# 0.229, 0.653
sim |> 
  group_by(confounder) |> 
  summarise(avg_y = mean(outcome))

sim |> 
  group_by(confounder, exposure) |> 
  summarise(avg_y = mean(outcome))



# quartets ----
# install.packages('quartets')
library(ggplot2)
library(quartets)
# anscombe_quartet
# 4 datasets

causal_collider |> head()
causal_confounding |> head()
causal_mediator
causal_m_bias # ? 

ggplot(data = causal_collider, 
       aes(x = exposure, y = outcome)) + 
  geom_point() + 
  geom_smooth(method = lm)


cor(causal_collider$exposure, causal_collider$outcome)



# g-comp ----
# binary outcome

?greek_data
# l: prognostic factor
# a: treatment (heart transplant)
# y: outcome: death

greek_data |> head()

# lm even though outcome is 0/1
greek_model <- lm(y ~ a+l, data = greek_data)

# duplicate data, change levels
untreated_data <- greek_data |> mutate(a = 0)
treated_data <- greek_data |> mutate(a = 1)

# predict using dup data
pred_unt <- greek_model |> 
  predict(newdata = untreated_data)
pred_unt

pred_t <- greek_model |> 
  predict(newdata = treated_data)
pred_t


mean(pred_unt)
mean(pred_t)
# difference is 0

# a: treatment
# y: outcome
table(greek_data$a, greek_data$y)

# P(y=1|a=1) = 7/(6+7) = 7/13



