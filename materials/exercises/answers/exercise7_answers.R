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

tfrmt_n_pct(n = "distinct_n", pct = "distinct_pct") %>%
  tfrmt(
    param = param,
    label = row_label1,
    column = col1, 
    group = row_label2, 
    values = value,
    sorting_cols = c(ord1, ord2),
    row_grp_plan = row_grp_plan(label_loc = element_row_grp_loc(location = "indented")),
    col_plan = col_plan(-Total, everything(), -starts_with("ord"))
  ) %>% 
  print_to_gt(ae_tbl)


# B) ----------------------------------------------------------------------
# Using the tfrmt code from exercise 3A and the PFT ARD, make the summary of
# PTFs table. Because the name of the groups don't match what we thought they
# would be when making the mocks, we will need to use `update_group` to change
# the names from our previous tfrmt.

sigdig_df <- tribble(
  ~pfts,                            ~sigdig,
  "FEV1 (L)",                         3,
  "Percent Predicted FEV1 (%)",       1,
  "Peak Expiratory Flow",             0,
  "Percent Predicted PEF (%)", 1
) %>% 
  mutate(visit = ".default")

tfrmt_sigdig(sigdig_df,
             group = vars(pfts, visit),
             label = label,
             param_set("{min}, {max}" = c(1, 1), 
                       "{q1}, {q3}" = c(1, 1), 
                       "missing" = NA)) %>% 
  update_group(row_label2 = pfts, row_label3 = visit) %>%
  tfrmt(
    column = col1,
    param = "param",
    values = value, 
    group = vars(row_label2, row_label3),
    label = row_label4,
    sorting_cols = c(ord1, ord2),
    row_grp_plan = row_grp_plan(
      label_loc = element_row_grp_loc(location = "indented")
    ),
    col_plan = col_plan(-starts_with("ord"))
  ) %>% 
  print_to_gt(pft_tbl)


# C) ----------------------------------------------------------------------
# Using the tfmrt code from 3B and the primary analysis ARD, create the final
# summary table

tfrmt(
  group = "pfts",
  column = c(span_col, lower_col),
  param = param, 
  label = VISIT,
  values = "value",
  sorting_cols = ord1,
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("[{conf.low}, {conf.high}]",
                                frmt("x.xxxx"))),
    frmt_structure(group_val = ".default", label_val = ".default", estimate = frmt("x.xxxx")),
    frmt_structure(group_val = ".default", label_val = ".default", std.error = frmt("x.xxxxx")),
    frmt_structure(group_val = ".default", label_val = ".default", 
                   p.value = frmt_when(
                     "<0.001" ~ "<0.001",
                     ">0.99" ~ ">0.99",
                     TRUE ~ frmt("x.xxx")
                   )
    )
  ),
  col_plan = col_plan(
    -contains("GSK123456 100 mg - Placebo"),
    Difference,
    everything(),
    -ord1,
  ),
  row_grp_plan = row_grp_plan(
    label_loc = element_row_grp_loc(location = "indented")
  )
) %>% 
  print_to_gt(primary_tbl)


