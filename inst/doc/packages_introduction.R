## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(terminaldigits)
library(dplyr)
library(gt)
library(ggplot2)

## -----------------------------------------------------------------------------
td_uniformity(decoy$weight, decimals = 2, reps = 2000)

## ----echo=FALSE---------------------------------------------------------------
Rows <- c(1, 2, "Total")
Zero <- c(0, 1, 1)
One <- c(2, 1, 3)
Two <- c(1, 0, 1)
Three <- c(2, 0, 2)
Four <- c(0, 1, 1)
Total <- c(5, 3, 8)

data.frame(Rows, Zero, One, Two, Three, Four, Total) %>%
  gt() %>%
  tab_header(
    title = "Contingecy table for toy data set"
  ) %>%
  tab_spanner(label = "Terminal Digit",
              columns = c(Zero:Four)) %>%
  cols_label(
    Rows = "Preceding Digits",
    Zero = "0",
    One = "1",
    Two = "2",
    Three = "3",
    Four = "4")

## -----------------------------------------------------------------------------

td_independence(decoy$weight, decimals = 2, reps = 2000)


## ----echo=FALSE, warning=FALSE, fig.align='center'----------------------------

d_mean <- mean(decoy$weight, na.rm = TRUE)
d_sd <- sd(decoy$weight, na.rm = TRUE)

decoy %>%
  ggplot(aes(weight)) +
  geom_histogram(bins = 20,
                 aes(y = ..density..)) +
  stat_function(fun = dnorm,
                args = list(mean = d_mean,
                            sd = d_sd),
                col = "#1b98e0",
                size = 1) +
  ylab(NULL) +
  xlab("Grams") +
  labs(title = "Sanitizer Weight") +
  theme_minimal() +
   theme(plot.title = element_text(hjust = 0.5),
      legend.title = element_blank())


## -----------------------------------------------------------------------------
td_simulate("normal", n = 3235, 
            parameter_1 = 54, 
            parameter_2 = 14, 
            decimals = 2,
            significance = 0.05,
            reps = 100,
            simulations = 100)

## -----------------------------------------------------------------------------
td_simulate("normal", n = 3235, 
            parameter_1 = 54, 
            parameter_2 = 14, 
            duplicates = 0.02,
            decimals = 2,
            significance = 0.05,
            reps = 100,
            simulations = 100)

