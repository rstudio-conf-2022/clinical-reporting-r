## Demo for Art of the Possible

library(tidyverse)
library(haven)

pft_tbl <- read_xpt("datasets/ARD/pft.xpt")
primary_tbl <- read_xpt("datasets/ARD/model.xpt")

## From an ARD, create a plot

plot_dat <- pft_tbl %>% 
  filter(
    row_label2 == "FEV1 (L)",
    param %in% c("mean", "sd", "n"),
    col1 %in% c("GSK123456 100 mg", "Placebo")
  ) %>% 
  mutate(row_label3  = as.numeric(factor(row_label3 , levels = c("BASELINE", "WEEK 4", "WEEK 8", "WEEK 12")))) %>% 
  select(
    Visit = row_label3,
    treatment = col1,
    param,
    value
  ) %>% 
  pivot_wider(
    names_from = param,
    values_from = value
  ) %>% 
  mutate(
    se = sd/sqrt(n)
  )

ggplot(
  data = plot_dat,
  aes(
    x = Visit,
    color = treatment,
    group = treatment
    )
  ) +
  
  geom_point(
    data = filter(plot_dat, Visit != 1),
    aes(y = mean),
    size = 3
  ) +
  geom_line(
    aes(y = mean),
    size = 1
  ) +
  geom_errorbar(
    aes(ymin = mean + se, 
        ymax = mean - se),
    width = 0.1,
    size = 1
  ) +
  
  geom_label(
    data = filter(plot_dat, Visit == 4),
    aes(
      label = treatment,
      x = Visit + 0.2,
      y = mean + 0.08
    ),
    hjust = 1
  ) +
  
  # ggplot styling
  scale_color_manual(values = c("#4DAF4A", "#377EB8")) +
  theme_bw() +
  labs(y = "FEV1 (L)\nChange from\nBaseline:\nMean ± SE") +
  theme_bw() +
  theme(legend.position = "none",) +
  scale_x_continuous(
    limits = c(1, 4.3),
    expand = c(0, 0),
    breaks = c(2, 3, 4),
    labels = c("WEEK 4", "WEEK 8", "WEEK 12")
  )

  
## Build on Exercise 7C Primary Results Table
library(tfrmt)
library(gt)
library(gtExtras)

# This is a tfrmt answer from 7c
primary_results_tfrmt <- tfrmt(
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
) %>% 
  tfrmt(
  # new formatting
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", 
                   p.value = frmt_when(
                     ## styling
                     "<0.001" ~ frmt("x.xxx", scientific = "x10^xx"),
                     ">0.99" ~ ">0.99",
                     TRUE ~ frmt("x.xxx")
                   )
    )
  ),
  # new col_plan
  col_plan = col_plan(
    pfts,
    VISIT,
    contains("Placebo - GSK123456 100 mg"),
  )
) 

primary_gt <- print_to_gt(primary_results_tfrmt, primary_tbl)

## Add styling as preferred
primary_gt_styled <- primary_gt %>% 
  gtExtras::gt_theme_guardian() %>% 
  gt::tab_style(
    style = cell_text(
      color = "red",
      style = "italic"
    ),
    locations = cells_body(
      columns = contains('p-value'),
      rows = grepl("WEEK 12", x = VISIT)
      )
  )

primary_gt_styled

## Save gt as an html:
# Many other supported outputs (?gtsave)

primary_gt_styled %>% 
  gtsave(
    "Primary_Results.html"
  )

