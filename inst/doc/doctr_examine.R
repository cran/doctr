## ---- echo = FALSE, message = FALSE--------------------------------------
library(tidyverse)
library(doctr)
data(mpg)

## ------------------------------------------------------------------------
# Converting class to factor
mpg$class <- as.factor(mpg$class)

## ------------------------------------------------------------------------
# Creating the EDA
eda <- examine(mpg)

## ------------------------------------------------------------------------
# Getting report of numeric variables
report_num(eda)

# Getting report of text variables
report_txt(eda)

# Getting report of factor variables
report_fct(eda)

## ------------------------------------------------------------------------
# Creating the EDA (grouped by the class variable)
eda <- examine(mpg, group = "class")

## ------------------------------------------------------------------------
# Getting report of numeric variables for compact cars
report_num(eda, group = "compact")

# Getting report of text variables for SUVs
report_txt(eda, group = "suv")

# Getting report of factor variables for midsize cars
report_fct(eda, group = "midsize")

