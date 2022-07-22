library(haven)
library(tfrmt)

# We can see this is the ARD we made in exercise 5A, but I have added some
# sorting columns
demog <- read_xpt("datasets/ARD/demog.xpt") 
View(demog)

# This is a tfrmt I made before 
pre_made_tfrmt <- tfrmt(
  # specify columns in the data
  group = row_label2,
  label = row_label3,
  column = col1, 
  param = param,
  values = value,
  sorting_cols = c(ord1, ord2),
  # specify value formatting 
  body_plan = body_plan(
    frmt_structure(group_val = ".default", label_val = ".default", 
                   frmt_combine("{n} {pct}", 
                                n = frmt("xxx"),
                                pct = frmt_when("==100" ~ "",
                                                "==0" ~ "",
                                                TRUE ~ frmt("(xx.x %)")))),
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{mean} ({sd})",
                                mean = frmt("xx.x"),
                                sd = frmt("xx.xx"))),
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{min}, {max}",
                                min = frmt("xx"),
                                max = frmt("xx"))),
    frmt_structure(group_val = ".default", label_val = ".default",
                   frmt_combine("{q1}, {q3}",
                                q1 = frmt("xx.x"),
                                q3 = frmt("xx.x"))),
    frmt_structure(group_val = ".default", label_val = c("n", "Missing"), frmt("xxx")),
    frmt_structure(group_val = ".default", label_val = c("Median"), frmt("xxx.x"))
  ),
  # # Specify column alignment plan
  col_style_plan = col_style_plan(
    element_col(align = c(".",","," "), col = c("GSK123456 100 mg", "Placebo", "Total")),
    element_col(width = 30, col = c(row_label3))
  ),
  # # Specify row group plan
  row_grp_plan = row_grp_plan(
    row_grp_structure(group_val = ".default", element_block = element_block(post_space = "  ")),
    label_loc = element_row_grp_loc(location = "indent")
  ), 
  col_plan = col_plan(-starts_with("ord"))
) 


print_to_gt(pre_made_tfrmt, demog)
