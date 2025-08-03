# install.packages('datarium')
# install.packages('rstatix')
library(datarium)
library(rstatix)
selfesteem

# make long format
selfesteem <- selfesteem %>%
  gather(key = "time", value = "score", t1, t2, t3) %>%
  convert_as_factor(id, time)

selfesteem

# compute mean (ignore subject)
selfesteem %>%
  group_by(time) %>%
  get_summary_stats(score, type = "mean_sd")

library(ggplot2)
library(ggpubr)
ggboxplot(selfesteem, x = "time", y = "score", add = "point")


# indep one way anova
?anova_test

anova_test(score ~ time, data = selfesteem)

# rm one way anova
anova_test(dv = score, wid = id, within = time, data = selfesteem)

aov(score ~ time + Error(id), data = selfesteem) |> summary()

# posthoc (pairwise t-test)
selfesteem %>%
  pairwise_t_test(
    score ~ time, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
# adjusted p is p from each pair times number of hypothesis
library(mmrm)
fit <- mmrm(formula = score ~ time + us(time | id), 
            data = selfesteem)
fit

lm(score ~ time, data = selfesteem)

library(lme4)
??lmer
fit2 <- lmer(formula = score ~ time + (1|id), 
            data = selfesteem)
anova(fit2)
fit2



