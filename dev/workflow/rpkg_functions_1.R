#' Example function for a simple data simulation, with a class attribute: SimulationResult
#'
#' @param n1 Sample size for group 1
#' @param n2 Sample size for group 2
#' @param mean1 Mean for group 1
#' @param mean2 Mean for gorup 2
#' @param sd1 Standard deviation for group 1
#' @param sd2 Standard deviation for group 2
#'
#' @return A list of results
#' @export
#'
#' @examples simd <- fsim(n1 = 10, n2 = 10, mean1 = 0, mean2 = 5, sd1 = 1, sd2 = 1)
fsim <- function(n1, n2, mean1, mean2, sd1, sd2){

  # put args in a list
  result <- list(n1 = n1,
              n2 = n2,
              mean1 = mean1,
              mean2 = mean2,
              sd1 = sd1,
              sd2 = sd2)

  # error: negative sd
  if(any(c(sd1, sd2)<=0)){
    rlang::abort("Standard deviation need to be greater than 0")
  }


  simd <- data.frame(
    group = c(rep(1, n1), rep(2, n2)),
    values = c(rnorm(n = n1, mean = mean1, sd = sd1),
               rnorm(n = n2, mean = mean2, sd = sd2))
  )

  # attach to result
  result$data <- simd

  # set class attribute
  result <- structure(result, class = 'SimulationResult')
  return(result)
}


# simd <- fsim(n1 = 10, n2 = 10, mean1 = 0, mean2 = 5, sd1 = 1, sd2 = 1)
# simd <- fsim(n1 = 10, n2 = 10, mean1 = 0, mean2 = 5, sd1 = 1, sd2 = 1)

# generics: print method

#' Print method
#'
#' @description
#' Generic function to print a `SimulationResult` object
#'
#' @param x a \code{SimulationResult} object to print
#' @param ... further arguments to pass from other methods
#'
#' @return something printed
#' @export
#'
#' @examples
#' simd <- fsim(n1 = 10, n2 = 10, mean1 = 0, mean2 = 5, sd1 = 1, sd2 = 1)
#' print(simd)
print.SimulationResult <- function(x, ...){
  # x is the input list
  args <- list(n1 = x$n1,
               n2 = x$n2,
               mean1 = x$mean1,
               mean2 = x$mean2,
               sd1 = x$sd1,
               sd2 = x$sd2)
  print(list(
    args = format(args),
    data = x$data
  ), ...)
}

# print(x = simd)




