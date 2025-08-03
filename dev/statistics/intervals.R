# stan, rstanarm, brms and beyond
library(ggplot2)
library(patchwork)

# load data ----

mortality <- readRDS("data/mortality_2000_2023.rds")
mortality |> head()

# select only total
d <- mortality[age == '000_059']
d


# d <- mortality[age == '000_019']
#d <- mortality[age == '020_039']
#d <- mortality[age == '040_059']


# eda
# counts

q1 <- ggplot(d, aes(x = year, y = deaths_n, group))
q1 <- q1 + geom_line()
q1 <- q1 + geom_point(size = 2)
q1 <- q1 + geom_vline(xintercept = 2019.5, color = "red", lty = 2)
q1 <- q1 + theme_bw()
q1 <- q1 + scale_x_continuous(breaks = seq(2000, 2023, 2))
q1 <- q1 + labs(
  x = 'Year', 
  y = 'Number of deaths', 
  title = 'Number of death in Norway \n2000 - 2023'
)
q1 <- q1 + theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  axis.text.x = element_text(angle = 45, vjust = 0.5) 
)
q1

# per 100k
q2 <- ggplot(d, aes(x = year, y = deaths_vs_pop_per_100k))
q2 <- q2 + geom_line()
q2 <- q2 + geom_point(size = 2)
q2 <- q2 + geom_vline(xintercept = 2019.5, color = "red", lty = 2)
q2 <- q2 + theme_bw()
q2 <- q2 + scale_x_continuous(breaks = seq(2000, 2023, 2))
q2 <- q2 + labs(
  x = 'Year', 
  y = 'Number of deaths', 
  title = 'Number of death in Norway \n(per 100k population) 2000 - 2023'
)
q2 <- q2 + theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  axis.text.x = element_text(angle = 45, vjust = 0.5) 
)
q2


# put together
q1 + q2


# take pre 2019 data
dt <- d[year <= 2019, .(year, deaths_vs_pop_per_100k)]
dt

dnew <- data.frame(year = c(2020, 2021, 2022, 2023))

# model (lm) ----

m_linear <- lm(deaths_vs_pop_per_100k ~ year, 
               data = dt)

# summary(m_linear)

pred_freq_ci <- predict(m_linear, newdata = dnew, interval = 'confidence')
pred_freq_pi <- predict(m_linear, newdata = dnew, interval = 'prediction')


pred_freq_pi[,3] - pred_freq_pi[,2] # quite close

# verify with formula
n <- nrow(dt)

# option 1
fitted_val <- m_linear$fitted.values
mse <- sum((dt$deaths_vs_pop_per_100k - fitted_val)^2)/(n-2)
mse
# option 2
sum((m_linear$residuals)^2)/(n-2)

# option 3
summary(m_linear)$sigma^2

# option 4
dvmisc::get_mse(m_linear)


# t-val
tval <- qt(p=0.975, df=n-2)
mean_x <- mean(dt$year)

# sum(xi - xbar)^2
ssx <- sum((dt$year - mean_x)^2)

sd_confint <- sqrt(mse * (1/20+ ((dnew$year - mean_x)^2)/ssx))
sd_predint <- sqrt(mse * (1 + 1/20+ ((dnew$year - mean_x)^2)/ssx))


# point prediction
b0 <- coef(m_linear)[1]
b <- coef(m_linear)[2]
prednew <- b0 + b*dnew$year
prednew

# prediction interval
predint_linear <- data.frame(fit = prednew, 
                             lwr = prednew - tval*sd_predint, 
                             upr = prednew + tval*sd_predint)

predint_linear

# confidence interval
confint_linear <- data.frame(fit = prednew, 
                             lwr = prednew - tval*sd_confint, 
                             upr = prednew + tval*sd_confint)

confint_linear






# model (rstanarm) ----
# use death per 100k


m_bayes <- rstanarm::stan_glm(
  deaths_vs_pop_per_100k ~ year, 
  data = dt, 
  family = gaussian,
  iter = 2000,
  chains = 8,
  refresh = 0
)

m_bayes 
# intercept, slope, residual sd
# access simulations
sims <- as.matrix(m_bayes)
head(sims)

median <- apply(sims, 2, median)
median
mad_sd <- apply(sims, 2, mad) 
# median absolute deviation (similar to sd)
mad_sd

#hist(sims[,1])
#hist(sims[,2])
#hist(sims[,3])



# credible interval about the fit
cred <- rstanarm::posterior_interval(m_bayes, prob = 0.95)
cred

# equivalent to
sims <- as.matrix(m_bayes)
apply(sims, 2, quantile, probs = c(0.025, 0.5, 0.975))



# point predict
# uses median from the posterior sim
y_point_pred <- predict(m_bayes, newdata = dnew)
y_point_pred

a_hat <- coef(m_bayes)[1] # median
b_hat <- coef(m_bayes)[2] # median
a_hat + b_hat*dnew$year



# linear predictor with uncertainty via a's and b's
y_linpred <- rstanarm::posterior_linpred(m_bayes, newdata = dnew)
head(y_linpred)

# focus on one year: 2023
hist(y_linpred[, 4])

head(y_linpred[, 4])
y_linpred_byhand <- sims[,1] + dnew$year[4] * sims[,2]
y_linpred_byhand[1:6]


# posterior predictive dist (with sigma)
y_postpred <- rstanarm::posterior_predict(m_bayes, newdata = dnew)
head(y_postpred)


# by hand
# focus on one year: 2023
n_sim <- nrow(sims)
y_postpred_byhand <- y_linpred_byhand + rnorm(n_sim, 0, sims[,3])
hist(y_postpred_byhand)
hist(y_postpred[,4])

y_postpred[,4] |> summary()
y_postpred_byhand |> summary()


# mortality example -----
# use all years to predict
# posterior predictive distribution
d 
# from 2000-2023
pred_all <- rstanarm::posterior_predict(m_bayes, d)


pred_all_quantile <- apply(pred_all, 2, quantile, 
                    probs = c(0.025, 0.5, 0.975)) %>%
  t() %>%
  as.data.frame()





# make plots ----

pd <- copy(d)
head(pd)
pd <- cbind(pd, pred_all_quantile)

setnames(pd, old = '2.5%', new = 'exp_death_per100k_025')
setnames(pd, old = '50%', new = 'exp_death_per100k_50')
setnames(pd, old = '97.5%', new = 'exp_death_per100k_975')

# also compute the expected death overall

pd[, exp_death_025 := exp_death_per100k_025 * pop_jan1_n/100000]
pd[, exp_death_50 := exp_death_per100k_50 * pop_jan1_n/100000]
pd[, exp_death_975 := exp_death_per100k_975 * pop_jan1_n/100000]

pd[, alert := fcase(
  deaths_vs_pop_per_100k > exp_death_per100k_975, "Higher than expected",
  default = "Expected"
)]

pd[, type := fcase(
  year <= 2019, paste0("Baseline (2000-2019)"),
  default = "Pandemic years (2020-2023)"
)]




q <- ggplot(pd, aes(x = year))
q <- q + geom_ribbon(mapping = aes(ymin = exp_death_per100k_025, 
                                   ymax = exp_death_per100k_975), 
                     alpha = 0.3)
q <- q + geom_line(mapping = aes(y = exp_death_per100k_50, lty = type), linewidth = 1)
q <- q + geom_point(mapping = aes(y = deaths_vs_pop_per_100k, color = alert), size = 3)
q <- q + geom_vline(xintercept = 2019.5, color = "red", lty = 2)
q <- q + expand_limits(y=0)
q <- q + scale_y_continuous("Number of death per 100k", expand = expansion(mult = c(0, 0.1)))
q <- q + scale_x_continuous("Year", breaks = seq(2000, 2023, 2))
q <- q + scale_linetype_discrete(NULL)
q <- q + scale_color_brewer(NULL, palette = "Set1", direction = -1)
q <- q + theme_bw()
q <- q + theme(
  axis.text = element_text(size = 11),
  axis.title = element_text(size = 12), 
  plot.title = element_text(size = 15), 
  axis.text.x = element_text(angle = 45, vjust = 0.5) 
)
q <- q + theme(legend.position = "bottom", legend.direction = "horizontal")
q <- q + theme(legend.box = "horizontal", legend.margin = margin(2, 2, 2, 2))
q


