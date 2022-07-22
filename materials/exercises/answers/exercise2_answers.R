# Exercise 2 - Making Mock Shells
#
# Loading the packages we will need 
library(tibble)
library(tidyr)
library(tfrmt)

# A) ----------------------------------------------------------------------
# Using the `tfrmt_n_pct()` template included in {tfrmt}, create the foundation
# of a tfrmt for an AE table and print it. Our goal is to have treatment
# across the top with AEs going vertically. (See handout for example mock)
ae_tfrmt_base <- tfrmt_n_pct()

print_mock_gt(ae_tfrmt_base)

# B) ----------------------------------------------------------------------
# Modify the tfrmt from (A) by adding the system organ class ("bodysys") and
# preferred term ("ae_term") as `group` and `label` to create nesting like most
# AE tables. Print the updated tfrmt to preview the new mock shell.
ae_tfrmt_base %>% 
  tfrmt(group = bodysys,
        label = ae_term) %>% 
  print_mock_gt()

# C) (bonus) --------------------------------------------------------------
# BONUS: By default, the tfrmt displays system organ class as a row spanner
# label. Modify the tfrmt to indent the preferred terms under the system organ
# classes in a single column. (hint: look at the `row_grp_plan` function). Print
# the updated tfrmt to preview the new mock shell
ae_tfrmt_base %>% 
  tfrmt(group = bodysys,
        label = ae_term,
        row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented"))) %>% 
  print_mock_gt()

# D) (bonus) --------------------------------------------------------------
# EXTRA BONUS: Create a dataset containing a few sample body systems and AE
# terms to supply to `print_mock_gt`. The dataset will need a column for system
# organ class ("bodysys"), preferred term ("ae_term"), treatment (column
# variable), and parameter (param variable). The tfrmt from (B) may need to be
# updated to include the column and param variables.
data <- tribble(
  ~bodysys, ~ae_term,
  "Cardio Vascular" , "MI",
  "Cardio Vascular" , "MI", 
  "Cardio Vascular" , "High Blood Pressure",
  "Cardio Vascular" , "High Blood Pressure", 
  "Neurological", "Headache", 
  "Neurological", "Headache"
) %>% 
  crossing(param = c("n", "pct"), 
           col1 = c("GSK123456", "Placebo", "Total"))


ae_tfrmt_base %>% 
  tfrmt(group = bodysys,
        label = ae_term,
        column = col1,
        param = param,
        title = "AE Mock",
        row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented"))) %>% 
  print_mock_gt(.data = data)
