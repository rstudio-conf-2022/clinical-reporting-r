# Exercise 6 - Convert stats analysis into an ARD 

library(tidyverse)
library(nlme)
library(emmeans)
library(haven)
library(broom)

# Prepared code to build model & extract lsmeans  

# Import and prepare data
adpft <- read_xpt("datasets/ADAM/adpft.xpt") %>%  
  mutate(VISIT = factor(VISIT, levels = c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12"))) %>% 
  filter(MITTFL == "Y",PARAMCD == "FEV1", VISITNUM > 2, TRTA != "Total") 

# MMRM model
mod <- gls(CHG ~ BASE + TRTA + VISIT + BASE:VISIT + TRTA:VISIT,
           data = adpft,
           # unstructured covariance matrix
           correlation = corSymm(form = ~as.numeric(VISIT)|USUBJID), # unstructured corr matrix
           weights = varIdent(form = ~1|VISIT), # allow distinct variance for each visit
           na.action = "na.omit")

# Extract lsmeans and contrasts
emm <- mod %>% emmeans(~TRTA|VISIT)  
emm_diff <- emm %>% contrast(method = "pairwise", adjust = "none")


# A) ----------------------------------------------------------------------
# Using `emm`, extract the lsmeans and standard errors to make a
# long and skinny dataset where there is one numeric value per row

# i) Use the `tidy()` function to turn the emmeans output (`emm`) into a dataset 


# ii) Create a long and skinny dataset with only the estimate and standard error
# for each treatment and visit. (Note: it will be easier later if the name of the
# test goes to "param" and the value goes to "value")



# B) ----------------------------------------------------------------------
# Using `emm_diff`, extract the difference of the lsmeans, lower
# bound of 95% CI, upper bound of 95% CI, p-value to make a long and skinny
# dataset where there is one numeric value per row

# i) Use the `tidy()` function to turn the contrast output into a dataset 
# HINT: option `conf.int = TRUE` is needed to get confidence intervals 


# ii) Selecting the visit, contrast, lsmean, 95% CI and p-value, create a long
# and skinny dataset where there is one numeric value per row. (Note: it will be
# easier later if the name the test goes to "param" and the value goes to
# "value")


# C) ----------------------------------------------------------------------
# Combine the datasets from (A) and (B) to make an ARD with columns for visit,
# pfts label, treatment/treatment contrast label, value label, parameter,
# value, and order.

# i) Combine the datasets from (A) and (B) making sure there is only a single
# column representing the treatment/treatment contrast label.


# ii) Create the pft label column, which should always equal "FEV1 (L)". Then
# create an order column to represent the order of the visits. And finally add
# the value labels such that: 
#     - Confidence intervals  -> "[95% CI]"
#     - p values  -> "p-value"
#     - standard errors -> "(SE)"
#     - LS Means of differences treatments -> "Difference"
#     - LS Means of treatments -> "Adjusted Mean"

