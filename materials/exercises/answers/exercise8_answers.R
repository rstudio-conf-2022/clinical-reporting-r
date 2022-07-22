# Exercise 8
#
# a) Using the provided post-processing of pft_tbl, fill in the missing code to 
# create a plot of Percent Predicted PEF (%) by visit, with a point presenting the median value
# and a line between the upper and lower quartiles. 
#
# b) You would like to submit your model findings in a paper to "Best Studies
# Quarterly", a world renown journal. However, they require pvalues to be formatted such that;
# - Pvalues greater than .5 are listed as ">.5",
# - Pvalues between .5 and .05 are rounded to 3 digits, 
# - Pvalues between 0.05 and .0001 are rounded to 4 digits
# - Pvalues less than .0001 are displayed as "<.0001*".
# This is a departure from your organizations default table formatting. But since you use ARD's and
# tfrmt, you can layer on this additional formatting requirement. Using the provided tfrmt from Exercise 7c,
# add a new layer to produce the correct format.
# Save this table as a word document.
#

library(tidyverse)
library(haven)
library(tfrmt)
library(gtExtra)

pft_tbl <- read_xpt("datasets/ARD/pft.xpt") 
primary_tbl <- read_xpt("datasets/ARD/model.xpt")

# A) ----------------------------------------------------------------------

plot_dat <- pft_tbl %>% 
  filter(
    param %in% c("median","q1","q3"),
    col1 %in% c("GSK123456 100 mg","Placebo"),
    row_label2 %in% c("Percent Predicted PEF (%)")
  ) %>% 
  mutate(
    row_label3 = factor(row_label3, c("BASELINE","WEEK 4","WEEK 8","WEEK 12"))
  ) %>% 
  select(-row_label4, - row_label2) %>% 
  pivot_wider(
    names_from = param,
    values_from = value
  ) %>% 
  rename(
    treatment = col1,
    VISIT = row_label3
  )

ggplot(plot_dat) +
  geom_linerange(
    aes(
      ymin = q1,
      ymax = q3,
      x = VISIT,
      color = treatment
    ),
    position = position_dodge(width = .3)
  ) + 
  geom_point(
    aes(
      y = median,
      x = VISIT ,
      color = treatment
    ),
    position = position_dodge(width = .3)
  ) +
  theme_bw() + 
  scale_color_manual(
    values = c("blue","green")
  ) + 
  labs(
    y = "Percent Predicted PEF (%)",
    x = NULL
  ) +
  ggtitle("Percent Predicted PEF (%) by Visit")

# B) ----------------------------------------------------------------------

model_tfrmt <- tfrmt(
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
    everything(),
    -ord1,
  ),
  row_grp_plan = row_grp_plan(
    label_loc = element_row_grp_loc(location = "indented")
  )
)

model_tfrmt %>% 
  tfrmt(
    ## add your custom body_plan here
    body_plan = body_plan(
      ## add your custom body_plan here
      frmt_structure(group_val = ".default", label_val = ".default", 
                     p.value = frmt_when(
                       ">.5" ~ ">.5",
                       ">.05" ~ frmt("x.xxx"),
                       ">=.0001" ~ frmt("x.xxxx"),
                       "<.0001" ~ "<0.0001*"
                     )
      )
    )
  ) %>% 
  print_to_gt(
    .data = primary_tbl
  )
