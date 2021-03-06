% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/td_simulate.R
\name{td_simulate}
\alias{td_simulate}
\title{Monte Carlo simulations for independence of terminal digits}
\usage{
td_simulate(
  distribution,
  duplicates = 0,
  n,
  parameter_1,
  parameter_2 = NULL,
  decimals,
  significance = 0.05,
  reps = 500,
  simulations = 300,
  tolerance = 64 * .Machine$double.eps
)
}
\arguments{
\item{distribution}{A string specifying the distribution from which to
draw data for simulations. Options include "normal", "uniform",
and "exponential".}

\item{duplicates}{A number between 0 and 1 specifying the proportion of
data to be comprised by duplicates. The default value is 0. This is
appropriate for testing type I errors. For testing power, a value
greater than 0 should be entered. For example, entering '0.05' would
ensure that for each simulation, 5\% of the data would be comprised by
duplicates.}

\item{n}{An integer specifying the number of observes to draw from the distribution.}

\item{parameter_1}{A numeric value specifying the mean for the normal distribution,
the lower bound of interval for the uniform distribution, or the rate for the
exponential distribution.}

\item{parameter_2}{A numeric value specifying the standard deviation for the normal
distribution or the upper bound of the interval for the uniform distribution.}

\item{decimals}{an integer specifying the number of decimals (including 0)
to which the values drawn from the distribution should be truncated.}

\item{significance}{a number between 0 and 1 defining the level for
statistical significance. The default is set to 0.05.}

\item{reps}{an integer specifying the number of Monte Carlo simulations to
implement under the null for each draw. The default is set to 500 but
this is only appropriate for initial exploration.}

\item{simulations}{an integer specifying the number of Monte Carlo
simulations to perform, i.e. the number of draws from the specified
distribution to be tested. The default is set to 300 but this is only
appropriate for initial exploration.}

\item{tolerance}{sets an upper bound for rounding errors when evaluating
whether a statistic for a simulation is greater than or equal to the
statistic for the observed data. The default is identical to the tolerance
set for simulations in the \code{chisq.test} function from the \code{stats}
package in R.}
}
\value{
A list containing the following components:

\item{method}{method employed}
\item{distribution}{the distribution}
\item{Chisq}{proportion of p-values less than or equal to defined
significance level for Pearson's chi-squared test of independence}
\item{G2}{proportion of p-values less than or equal to defined
significance level for log-likelihood ratio test of independence}
\item{FT}{proportion of p-values less than or equal to defined
significance level for Freeman-Tukey test of independence}
\item{RMS}{proportion of p-values less than or equal to defined
significance level for root-mean-squared test of independence}
\item{O}{proportion of p-values less than or equal to defined
significance level for occupancy test of independence}
\item{AF}{proportion of p-values less than or equal to defined
significance level for average frequency test of independence}
}
\description{
The \code{td_simulate} function performs Monte Carlo simulations to assess
type I errors and power for tests of independence of terminal digits for
various truncated continuous distributions.
}
\details{
Monte Carlo simulations for the null hypothesis are implemented for contingency
tables with fixed margins using algorithm ASA 144 (Agresti, Wackerly, and
Boyett, 1979; Boyett 1979).
}
\examples{

td_simulate(distribution = "normal",
n = 50,
parameter_1 = 100,
parameter_2 = 1,
decimals = 1,
reps = 100,
simulations = 100)

}
\references{
Agresti, A., Wackerly, D., & Boyett, J. M. (1979). Exact conditional tests
for cross-classifications: approximation of attained significance levels.
Psychometrika, 44(1), 75-83.

Boyett, J. M. (1979). Algorithm AS 144: Random r × c tables with
given row and column totals. Journal of the Royal Statistical Society.
Series C (Applied Statistics), 28(3), 329-332.
}
