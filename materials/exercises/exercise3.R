# Exercise 3 - Making Complex Mock Shells

# Loading the packages we will need 
library(tibble)
library(tidyr)
library(dplyr)
library(tfrmt)
library(stringr)

# Giving you some helpful variables to make it easier 
visit = c("Baseline", "Week 4", "Week 8", "Week 12")

# A) ----------------------------------------------------------------------
# Create a mock shell for Summary for of PFTs (Pulmonary Function Test) by Visit.
# (See handout for example mock)
#
# Using the provided dummy dataset, the table should have a row for every
# test, visit, and descriptive statistic, as well as a column for each treatment and
# the total. All visits, tests, and descriptive statistics have been provided, as
# well as a base rounding value for each test. The rounding for each statistic
# (param) should be as follows:
#  - min, max: 0 + base rounding 
#  - mean, median, q1, q3: 1 + base rounding 
#  - sd: 2 + base rounding 
#  - n, missing: integer 

# Base significance of each parameter 
sigdig_df <- tribble(
  ~pfts,                            ~sigdig,
  "FEV1 (L)",                         3,
  "Percent Predicted FEV1 (%)",       1,
  "Peak Expiratory Flow",             0,
  "Percent Predicted Peak Expiratory Flow (%)", 1
) %>% 
  mutate(visit = ".default")

# Data to use to make the mock 
pfts = c("FEV1 (L)",
         "Percent Predicted FEV1 (%)",
         "Peak Expiratory Flow",
         "Percent Predicted Peak Expiratory Flow (%)")
trt = c("GSK123456", "Placebo", "Total")
param = c("n", "mean", "sd","median", "q1", "q3", "min", "max", "missing")
data <- crossing(visit, pfts, trt, param) %>% 
  mutate(
    label = case_when(
    param %in% c("mean", "sd") ~ "Mean (SD)",
    param %in% c("q1", "q3") ~ "Q1, Q3",
    param %in% c("min", "max") ~ "Min, Max",
    param == "n" ~ "n",
    TRUE ~ str_to_sentence(param)
  ),
  ord1 = case_when(pfts == "FEV1 (L)" ~ 1,
                          pfts == "Predicted Normal FEV1" ~ 2,
                          pfts == "Percent Predicted FEV1 (%)" ~ 3,
                          pfts == "Peak Expiratory Flow" ~ 4,
                          TRUE ~ 5),
         ord2 = case_when(visit == "Baseline" ~ 1,
                          visit == "Week 4" ~ 2,
                          visit == "Week 8" ~ 3,
                          visit == "week 12" ~ 4))

# i) Using `param_set`, update the defaults so the min/max and q1/q3 parameters
# will presented together on the same line (separated by a comma). Min and max
# have +0 rounding and the quantiles have +1 rounding
?param_set()

# ii) Using the set of parameters from (i) create a tfrmt using `tfrmt_sigdig()`
# and the `sigdig_df` data.
# The `group` and `label` argument inputs should take column names from `data`.
?tfrmt_sigdig()

# iii) Add another tfrmt layer to (ii) to add the names of the `column` and
# `param` columns that match the given data. Also use the `row_grp_plan` to
# indent the grouping and label columns. Print this mock with `data`. 


# iv) Finally, supply the sorting columns to `sorting_cols` and use `col_plan`
# to drop them from the table. Print this mock with `data`. It should match the
# mock in your handout.


# B) ----------------------------------------------------------------------
# Create a mock shell for the primary analysis of FEV1. 
# 
# The table should have a row for every visit, with spanning columns
# corresponding to the treatment groups as well as their difference.
# Specifically, the following columns are needed:
#   - Within treatment group: Adjusted mean, (SE)
#   - Difference between treatment groups: Difference, [95% CI], p-value
# The rounding for each statistic should be as follows:
#   - Adjusted mean, difference, 95% CI: 4 decimal places
#   - SE: 5 decimal places
#   - p-value: 3 decimal places, unless if <0.001 or >0.99, in which case "<0.001" or ">0.99" should be displayed

# Data to use to make the mock
data <- tribble(
  ~span_col,             ~lower_col,      ~param,
  "GSK123456",           "Adjusted Mean", "estimate",
  "GSK123456",           "(SE)",          "std.error",
  "Placebo",             "Adjusted Mean", "estimate",
  "Placebo",             "(SE)",          "std.error",
  "GSK123456 - Placebo", "[95% CI]",      "conf.high",
  "GSK123456 - Placebo", "[95% CI]",      "conf.low",
  "GSK123456 - Placebo", "Difference",    "estimate",
  "GSK123456 - Placebo", "p-value",       "p.value"
) %>% 
  crossing(VISIT = visit[-1])%>% 
  mutate(pfts = "FEV1 (L)",
         ord1 = case_when(VISIT == "Week 4" ~ 1,
                          VISIT == "Week 8" ~ 2,
                          VISIT == "Week 12" ~ 3)
  ) 

# i) Using the partially completed tfrmt, add formatting for the confidence
# intervals (conf.low, conf.high) and p-values (p.value). Then print the mock
# using the `data`

pft_tfrmt <- tfrmt(
  group = "pfts",
  label = "VISIT",
  param = "param",
  column = c("span_col", "lower_col"),
  sorting_cols = "ord1",
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", estimate = frmt("xx.xxxx")),
    frmt_structure(group_val = ".default", label_val = ".default", std.error = frmt("(xx.xxxxx)"))
    ## Add new frmt_structures here
  )
)

pft_tfrmt %>% 
  print_mock_gt(.data = data)

# ii) Using col_plan fix the column order so it is GSK123456, Placebo and then the
# difference. Also, drop the order column. Print this mock with `data`. 



# iii) Change the group location from spanning to indented. Print this mock with
# `data`.

