# unit test
# install.packages('covr')

# identify how much is test-covered
covr::package_coverage(path = '~/Documents/GitHub/cstime/')



library(testthat)

# example ----

# a function that standardizes
create_feature <- function(x){
  (x-mean(x))/sd(x)
}


# use it 
# generate some data
raw <- rnorm(100, mean = 25, sd = 5)
# standardize them
d_st <- create_feature(raw)
d_st

# issues with our data
d <- c(1, 2, 3, NA_real_)
create_feature(d)


# write a simple test ----
# types of tests:
# lt: greater or less
# identical
# error


test <- function(){
  test_that('Standardized variable is transformed correctly', {
    # some numbers
    rn <- rnorm(100, 10, 5)
    
    # expect mean of transformed data is within 0.005 of zero
    expect_lt(
      abs(mean(create_feature(rn))), 0.005
    )
  })
}

test()


# a test that fails ----
# expect error 
create_feature(c(1, 2, 3 / 0)) 
# this produces NaNs, but does not produce error message

test <- function(){
  test_that("Inifinity causes an error", {
    expect_error(
      create_feature(c(1, 2, 3 / 0)),
      "`x` must not contain any infinite values"
    )
  })
}


test()

# refactor ----

library(rlang)
# rlang::abort()

create_feature <- function(x) {
  
  # separate arguments, remove na
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  
  # zero sd
  if (isTRUE(sigma == 0)) abort("`x` must have a non-zero standard deviation.")
  # cant have only one
  if (isTRUE(length(unique(x)) == 1L)) abort("`x` must have more than one unique element.")
  # nan 
  if (isTRUE(any(is.nan(x)))) abort("`x` must not contain any `NaN` values.")
  # infinite
  if (isTRUE(any(is.infinite(x)))) abort("`x` must not contain any infinite values.")
  
  (x - mu) / sigma
}

# test again
test()
# passed, since we made the expect error explicit
create_feature(c(1, 2, 3 / 0)) 


# create a df, where group1 only has one value
df <- data.frame(group = c(1, 2, 2, 3, 3), 
                 value = 1:5)

group_by(df, group) |> 
  dplyr::mutate(
  value_std = create_feature(value)
)

# the error message says x must have more than 1 unique element





