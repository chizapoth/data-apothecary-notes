# multiple imputation 

# mini_nhanes data
d <- read_csv('data/mini_nhanes.csv', col_names = T)
d

# MASS::whiteside

library(mice)
# also test the small dataset from mice pkg
nhanes

md.pattern(nhanes)
# 27 missing in total
# by col: 8 for hyp, 9 for bmi, 10 for chl
# by row: n missing numbers

# mean imputation ----
# only run once, since it's just the mean
imp <- mice(data = nhanes, 
            method = 'mean', 
            m = 1, 
            maxit = 1)

imp # this is not the imputed value
imp$imp$bmi

nhanes
nhanes_imp <- complete(imp)
nhanes_imp


# compare summary stat (should be the same mean)
colMeans(nhanes, na.rm = T)
colMeans(nhanes_imp)




# impute with sample

imps <- mice(data = nhanes, 
            method = 'sample', 
            m = 1, 
            maxit = 1)
imps$imp$bmi


# regression impute ----

## without uncertainty ----

impr0 <- mice(nhanes, method = 'norm.nob', m=1, maxit = 1)
impr0

nhanes_impr0 <- complete(impr0)
nhanes_impr0

# check imputed
impr0$imp$bmi

# check summary
colMeans(nhanes_impr0)


## via prediction -----

impr <- mice(nhanes, method = 'norm.predict', m=1, maxit=1)
impr

nhanes_impr <- complete(impr)
nhanes_impr

# check summary
# slight changes
colMeans(nhanes_impr)


# try fit a model 

fit_0 <- lm(age ~ bmi, data = nhanes)
summary(fit_0)
# -0.07, p=0.15
# use the complete data
fit_impr <- lm(age ~ bmi, data = nhanes_impr)
summary(fit_impr)
# -0.1, p=0.003


## bayesian linear reg ----

impb <- mice(nhanes, method = 'norm', m=1, maxit=1)
impb


nhanes_impb <- complete(impb)
nhanes_impb

colMeans(nhanes_impb)



## bootstrap ----

impbt <- mice(nhanes, method = 'norm.boot', m=1, maxit=1)
impbt

nhanes_impbt <- complete(impbt)
nhanes_impbt

colMeans(nhanes_impbt)



# (univar) pred mean match ---- 


## pmm -----
imp_pmm <- mice(nhanes, method = 'pmm', m=1, maxit=1)
imp_pmm

# imputations for bmi
imp_pmm$imp$bmi

# first completed data matrix
complete(imp_pmms, 1)

complete(imp_pmms, 2)





## midastouch ----

imp_pmms <- mice(nhanes, method = 'midastouch', m=1, maxit=1)
imp_pmms

# imputations for bmi
imp_pmms$imp$bmi

# first completed data matrix
complete(imp_pmms, 1)

complete(imp_pmms, 2)




