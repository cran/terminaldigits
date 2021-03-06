#' Test of uniformity of terminal digits
#'
#' The `td_uniformity` function tests the uniformity of terminal digits via
#' Pearson's chi-squared test of goodness-of-fit. Rather than relying on the asymptotic
#' approximation to the chi-squared distribution, `td_unformity` uses the `chisq_gof`
#' function from the `discretefit` package to simulate the distribution under the null.
#'
#' @param x a numeric vector
#' @param decimals an integer specifying the number of decimals. This can be zero if the terminal digit is
#'     not a decimal.
#' @param reps a positive integer specifying the number of Monte Carlo simulations. The default
#'     is set to 10,000.
#' @param tolerance sets an upper bound for rounding errors when evaluating
#'    whether a statistic for a simulation is greater than or equal to the
#'    statistic for the observed data. The default is identical to the tolerance
#'    set for simulations in the `chisq.test` function from the `stats`
#'    package in R.
#'
#' @return A list containing the following components:
#'
#' \item{statistic}{the value of the test statistic}
#' \item{p_value}{the simulated p-value for the test}
#' \item{method}{a character string describing the test}
#' \item{data.name}{a character string give the name of the data}
#'
#'
#'
#' @export
#'
#' @examples
#'
#' td_uniformity(decoy$weight, decimals = 2, reps = 2000)
#'
#'
#' @useDynLib terminaldigits
#' @importFrom Rcpp sourceCpp


td_uniformity <- function(x,
                          decimals,
                          reps = 10000,
                          tolerance = 64 * .Machine$double.eps)  {

  if(!class(x) %in% c("numeric", "integer" )) {stop("The vector `x` must be numeric")}

  if(reps <= 0) {
    stop("The 'reps' parameter requires a positive integer")

  }

  DNAME <- deparse(substitute(x))

  x <- x[!is.na(x)]
  x <- x[!is.infinite(x)]

  a <- int_dec(x, decimals = decimals)$dec

  observed <- tabulate(a + 1, nbins = 10)
  expected <- rep(0.1, 10)

  out <- discretefit::chisq_gof(observed, expected, reps = reps, tolerance = tolerance)

  val <- list(p.value = out$p.value,
              statistic = out$statistic,
              method = "Pearson's chi-squared GOF test for uniformity of terminal digits",
              data.name = DNAME,
              class = "htest")

  class(val) <- "htest"

  return(val)
}
