# joint modeling: longitudinal + survival 

# install.packages('JM')
library(JM)

prothro |> head()
?prothro

# prothrobin measurements
# time: time when meas is taken
# treatment / placebo

table(prothro$id) # not equal 
table(prothro$treat) # 1498 placebo, 1470 prednisone (total meas)
# how many events
sum(prothro$event) # 292 out of all subjects



prothro[prothro$id == 558, ]

# Mixed-effects model fit
# random effect: 
lmeFit.p1 <- lme(log(pro) ~ time + time:treat, 
                 data = prothro,
                 random = ~ time | id)  

# Cox survival model fit
# death time
survFit.p1 <- coxph(Surv(Time, death) ~ treat, 
                    data = prothros, x = TRUE)  
survFit.p1
summary(survFit.p1)

# Joint model
jointFit.p1 <- jointModel(lmeFit.p1, 
                          survFit.p1, 
                          timeVar = "time",
                          method = "piecewise-PH-aGH")

# We are interested in producing predictions of survival probabilities for Patient 155
# 10 measurements, last point dead
dataP155 <- prothro[prothro$id == 155, ]
len_id <- nrow(dataP155)


# We can plot the data
sfit3 <- survfitJM(jointFit.p1, newdata = dataP155[1:3, ]) 
sfit3
sfit4 <- survfitJM(jointFit.p1, newdata = dataP155[1:4, ]) 
par(mfrow=c(1,2))
plotfit3 <- plot(sfit3, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 155")
plotfit4 <- plot(sfit4, estimator="mean", include.y = TRUE, conf.int=0.95, fill.area=TRUE, col.area="lightblue", main="Patient 155")





