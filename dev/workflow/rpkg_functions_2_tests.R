# test both fsim and print.SimulatedResults

# due to randomness, better test fail

test_that("Can not accept negative sd", {
  # when there is an error message, use expect error
  expect_error(
    simd <- fsim(n1 = 10, n2 = 10, mean1 = 0, mean2 = 5, sd1 = 0, sd2 = 1),
    # expected output
    "Standard deviation need to be greater than 0"
  )
})


# maybe we DO NOT really need to test the print method
test_that("Length of print output is 2", {
  # length is equala to 2
  expect_equal(
    length(print( # we are testing the print method
      simd <- fsim(n1 = 10, n2 = 10, mean1 = 0, mean2 = 5, sd1 = 1, sd2 = 1))
      ),
    # 2 elements
    2
  )
})

