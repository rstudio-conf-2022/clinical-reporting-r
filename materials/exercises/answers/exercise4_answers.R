# Exercise 4 - SDTM to ADaM data 

# Loading the packages we will need 
library(metacore)
library(tidyverse)
library(admiral)
library(haven)
library(metatools)
library(lubridate)

# Read in the metadata 
metacore<- spec_to_metacore("specs/specs.xlsx", where_sep_sheet = FALSE)

# Read the data in 
dm <- read_xpt("datasets/SDTM/dm.xpt")
vs <- read_xpt("datasets/SDTM/vs.xpt")
ex <- read_xpt("datasets/SDTM/ex.xpt")
sv <- read_xpt("datasets/SDTM/sv.xpt")
ae <- read_xpt("datasets/SDTM/ae.xpt")
suppae <- read_xpt("datasets/SDTM/suppae.xpt")
re <- read_xpt("datasets/SDTM/re.xpt")


# A) ----------------------------------------------------------------------
# Following the creation of the other variables in `adsl_decode`, use the
# specifications to make the ACOUNTRY and RACEN.
adsl_spec <- metacore %>% 
  select_dataset("ADSL")

adsl_pred <- build_from_derived(adsl_spec, 
                                ds_list = list("dm" = dm), 
                                keep = TRUE) %>% 
  filter(ARMCD %in% c("A", "P"))

adsl_decode <- adsl_pred %>% 
  create_cat_var(adsl_spec, AGE, AGEGR1, AGEGR1N) %>% 
  create_var_from_codelist(adsl_spec, SEX, SEXN)%>% 
  create_var_from_codelist(adsl_spec, ETHNIC, ETHNICN) %>% 
  create_var_from_codelist(adsl_spec, ARMCD, TRT01PN) %>% 
  create_var_from_codelist(adsl_spec, ACTARMCD, TRT01AN) %>%
  create_var_from_codelist(adsl_spec, ARMCD, TRT01P) %>% 
  create_var_from_codelist(adsl_spec, ACTARMCD, TRT01A) %>% 
  # ANSWER 
  create_var_from_codelist(adsl_spec, COUNTRY, ACOUNTRY) %>% 
  create_var_from_codelist(adsl_spec, RACE, RACEN)
  

# B) ----------------------------------------------------------------------
# Add baseline values for height, weight and BMI to adsl (should be called
# HEIGHTBL, WEIGHTBL, and BMIBL respectively). These values should be extracted
# and calculated from the `vs` dataset where `VSBLFL` is "Y" and added to the
# `adsl_decode` dataset to make `adsl_bl`

# ANSWER 
baseline_vals <- vs %>% 
  filter(VSTESTCD %in% c("HEIGHT", "WEIGHT") & VSBLFL == "Y") %>% 
  select(USUBJID, VSTESTCD, VSORRES) %>% 
  mutate(VSTESTCD = paste0(VSTESTCD, "BL")) %>% 
  pivot_wider(names_from = VSTESTCD, values_from = VSORRES) %>% 
  mutate(BMIBL = compute_bmi(HEIGHTBL, WEIGHTBL)) 

adsl_bl <- left_join(adsl_decode, baseline_vals, by = "USUBJID")

# Additional for creating the first and last datetime of treatment
adsl_ex <- adsl_bl %>%
  derive_vars_merged_dtm(
    dataset_add = ex,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))) & nchar(EXSTDTC) >= 10,
    new_vars_prefix = "TRTS",
    dtc = EXSTDTC,
    time_imputation = "FIRST",
    order = vars(TRTSDTM, EXSEQ),
    mode = "first",
    by_vars = vars(STUDYID, USUBJID),
    flag_imputation = "none"
  ) %>%
  derive_vars_merged_dtm(
    dataset_add = ex,
    filter_add = (EXDOSE > 0 |
                    (EXDOSE == 0 &
                       str_detect(EXTRT, "PLACEBO"))),
    new_vars_prefix = "TRTE",
    dtc = EXENDTC,
    time_imputation = "last",
    order = vars(EXSEQ),
    mode = "last",
    by_vars = vars(STUDYID, USUBJID),
    flag_imputation = "none"
  )

# C) ----------------------------------------------------------------------
# Create a safety flag. "Y" for all subject who received a dose or who were on
# placebo. This can be derived from the `ex` dataset where the participants have
# either an exposure (`EXDOSE` > 0) or are placebo. Add this to the code for
# `adsl_ex` to create `adsl`.
 
adsl <- adsl_ex  %>% 
  derive_var_merged_exist_flag(
    dataset_add = sv,
    by_vars = vars(STUDYID, USUBJID),
    new_var = MITTFL,
    condition = (VISITDY >=56)
  ) %>% 
  mutate(RANDFL = if_else(ACTARMCD %in% c("P", "A"), "Y", NA_character_)) %>% 
  #ANSWER
  derive_var_merged_exist_flag(
    dataset_add = ex,
    by_vars = vars(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (EXDOSE > 0 | (EXDOSE == 0 & str_detect(EXTRT, "Placebo")))
  )

# Finish up the dataset by applying labels and checking control terms/variables
adsl <- adsl %>% 
  order_cols(adsl_spec) %>% 
  set_variable_labels(adsl_spec) %>% 
  check_ct_data(adsl_spec) %>%  
  check_variables(adsl_spec)


# D) ----------------------------------------------------------------------
# Make `ADAE` by first pulling together the predecessors, and then making
# `ASTDT` and `AENDT` by extracting the date from `AESTDTC` and `AEENDTC`
# respectively

adae_spec <- metacore %>% 
  select_dataset("ADAE")

#Combine AE and SUPPAE so all predecessor are actually in the ae dataset 
ae_tot <- combine_supp(ae, suppae)
 
# create ADAE
adae<- build_from_derived(adae_spec, 
                          ds_list = list("adsl" = adsl,"ae" = ae_tot), 
                          keep = TRUE) %>% 
  mutate(ASTDT = as_date(AESTDTC),
         AENDT = as_date(AEENDTC)) 

# Finish up and check ADAE
adae <- adae %>% 
  order_cols(adae_spec) %>% 
  set_variable_labels(adae_spec) %>% 
  check_ct_data(adae_spec) %>%  
  check_variables(adae_spec)


