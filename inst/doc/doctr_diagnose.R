## ---- echo = FALSE, message = FALSE--------------------------------------
library(tidyverse)
library(doctr)
data(mpg)

## ------------------------------------------------------------------------
# Runninng exams on table
diagnostics <- diagnose(mpg)

## ------------------------------------------------------------------------
# Getting summary of diagnostics
issues(diagnostics)

## ------------------------------------------------------------------------
# Manually breaking mpg
mpg2 <- mpg %>%
  mutate(year = as.Date(year, origin = "1970-01-01"))

# Getting summary of diagnostics
mpg2 %>% diagnose() %>% issues(verbose = TRUE)

## ------------------------------------------------------------------------
exams <- guess_exams(mpg)

## ---- echo = FALSE-------------------------------------------------------
knitr::kable(exams)

## ------------------------------------------------------------------------
# Setting some arbritraty maximum and minimum values
exams$max_val[8] <- 30
exams$min_val[9] <- 15

# Setting least frequent class
exams$least_frec_cls[10] <- 0.2

# Setting maximum unique classes
exams$max_unq[1] <- 10

# Use custom exams to diagnose table
mpg %>% diagnose(exams) %>% issues()

## ------------------------------------------------------------------------
# Use custom exams to diagnose table
diagnostics <- diagnose(mpg, exams)

# Get results for 1st column
issues(diagnostics, i = 1, verbose = TRUE)

# Get results for fl column
issues(diagnostics, i = "fl", verbose = TRUE)

