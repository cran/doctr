## ---- echo = FALSE, message = FALSE--------------------------------------
library(tidyverse)
library(doctr)
data(mpg)

## ------------------------------------------------------------------------
# Creating aritificial versions of the dataset
mpg_jan <- mpg
mpg_feb <- sample_n(mpg, 100)

# Comparing mpg_jan and mpg_feb
comparison <- compare(mpg_jan, mpg_feb)

## ------------------------------------------------------------------------
# Getting summary of comparison
issues(comparison)

## ------------------------------------------------------------------------
# Get results for 1st column
issues(comparison, i = 3, verbose = TRUE)

# Get results for fl column
issues(comparison, i = "hwy", verbose = TRUE)

## ------------------------------------------------------------------------
mpg_jan %>% compare(mpg_feb, ci = 0.5) %>% issues()

