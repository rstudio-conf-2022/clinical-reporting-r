library(metacore)
library(tidyverse)
library(admiral)
library(haven)
library(metatools)

# Here we will make the ADPFT dataset together 

metacore<- spec_to_metacore("specs/specs.xlsx", where_sep_sheet = FALSE)

adsl <- read_xpt("datasets/ADAM/adsl.xpt")
re <- read_xpt("datasets/SDTM/re.xpt")

# import metadata/data ---------------------------------------------------------

print(metacore)

# It has the 3 datasets we will be using for our tables 
metacore$ds_spec
# We can see the metadata from the spec document is now in this structure in R 
head(metacore$var_spec)

# Let's specify a dataset 
adpft_spec <- metacore %>% 
  select_dataset("ADPFT")

# Next, we can use this smaller metacore object to automate some of the steps in
# creating ADPFT

# Get all the predecessors -------------------------------------------------

# the second step is to pull in the predecessor variables 
pred_vars <-  build_from_derived(adpft_spec, 
                                 ds_list = list("adsl" = adsl,"re" = re), 
                                 keep = FALSE) 

head(pred_vars)

# Now we can see what variables we need to create 
check_variables(pred_vars, adpft_spec)

# Create new variables ----------------------------------------------------

# metatools has two different functions to let you leverage codes/decodes from
# your metadata: create_cat_var and create_var_from_codelist. 
#     - create_cat_var() is used to make a categorical variable from a continuous one 
#     - create_var_from_codelist is used to create a new variable based on the code/decode pair

# the categories needed 
get_control_term(adpft_spec, PARAMN)

pred_vars %>% 
  select(PARAM)

decode_adpft<- pred_vars %>% 
  create_var_from_codelist(adpft_spec, PARAM, PARAMN) 

decode_adpft %>% 
  select(USUBJID, PARAM, PARAMN)


# The next variables we need all require baseline to calculate 
check_variables(decode_adpft, adpft_spec)

# first, lets identify the baseline values 
base_id <- decode_adpft %>% 
  mutate(ABLFL = if_else(VISIT == "BASELINE", "Y", ""))

base_id %>% 
  select(USUBJID, PARAM, VISIT, ABLFL)

# {admiral} contains functions that help do common calculations such as bmi,
# date imputations, and flag creation. Here, we can use {admiral} to make a new
# baseline column based on the baseline flag

adpft_base <- base_id %>% 
  derive_var_base(
    by_vars = vars(USUBJID, PARAMCD),
    source_var = AVAL,
    new_var = BASE,
    filter = ABLFL == "Y"
  )

# Alternatively, we could have used the tidyverse and joined it on 
adpft_base <- base_id %>% 
  filter(VISIT == "BASELINE") %>% 
  select(USUBJID, PARAMCD, BASE = AVAL) %>% 
  left_join(base_id, ., by = c("USUBJID", "PARAMCD"))

# Finally, we can make a variable for change from baseline 
adpft <- adpft_base %>% 
  mutate(CHG = AVAL - BASE)

  
check_variables(adpft, adpft_spec)



