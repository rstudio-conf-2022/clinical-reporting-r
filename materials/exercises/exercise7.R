# Exercise 7 - Make tables

# Loading the packages we will need 
library(tfrmt)
library(haven)
library(dplyr)

# Load ARDs
ae_tbl <- read_xpt("datasets/ARD/ae.xpt") 
pft_tbl <- read_xpt("datasets/ARD/pft.xpt") 
primary_tbl <- read_xpt("datasets/ARD/model.xpt")


# A) ----------------------------------------------------------------------
# Using the tfrmt code from exercise 2C and the AE ARD, make the AE tables
# (hint: you might need to change some parameters)


# B) ----------------------------------------------------------------------
# Using the tfrmt code from exercise 3A and the PFT ARD, make the summary of
# PTFs table. Because the name of the groups don't match what we thought they
# would be when making the mocks, we will need to use `update_group` to change
# the names from our previous tfrmt.

sigdig_df <- tribble(
  ~pfts,                            ~sigdig,
  "FEV1 (L)",                         3,
  "Predicted Normal FEV1",            1,
  "Percent Predicted FEV1 (%)",       1,
  "Peak Expiratory Flow",             0,
  "Percent Predicted PEF (%)", 1
) %>% 
  mutate(visit = ".default")



# C) ----------------------------------------------------------------------
# Using the tfmrt code from 3B and the primary analysis ARD, create the final
# summary table


