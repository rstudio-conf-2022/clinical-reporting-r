# Exercise 5 - Creating ARDs 

library(Tplyr)
library(haven)
library(dplyr)

# Read in datasets 
adsl <- read_xpt("datasets/ADAM/adsl.xpt")
adae <- read_xpt("datasets/ADAM/adae.xpt") 
adpft <- read_xpt("datasets/ADAM/adpft.xpt") 


# A) ----------------------------------------------------------------------
# Add layers for ethnicity and baseline BMI to the demography table
demog <- adsl %>% 
  tplyr_table(TRT01A, where= SAFFL =="Y") %>%
  add_total_group() %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex", "n (%)") )
  ) %>% 
  add_layer(
    group_desc(target_var = AGE, by = "Age (years)") 
  )  %>%
  add_layer(
    group_count(target_var = AGEGR1, by = vars("Age (years)", "n (%)") )
          ) %>% 
  add_layer(
    group_count(target_var = RACE, by = vars("Race", "n (%)"))
  ) %>% 
  add_layer(
    group_desc(target_var = HEIGHTBL, by = "Height (cm)") 
  ) %>% 
  add_layer(
    group_desc(target_var = WEIGHTBL, by = "Weight (kg)") 
  ) %>% 
  get_numeric_data()


# B) ----------------------------------------------------------------------
# Calculate the number and percentage of *unique* subjects with at least one AE
# by AEBODSYS, AETERM, and treatment (hint: you will need to use multiple target
# variables in `group_count`)
?set_distinct_by

tplyr_table(adae, TRT01A) %>%
  set_pop_data(adsl)

# C) ----------------------------------------------------------------------
# Calculate the number and percentage of *unique* subjects with any AE
# by adding an additional count layer to the code from 5B. Also add a total
# treatment group. 

# D) ----------------------------------------------------------------------
# For the safety population, calculate the following descriptive statistics for
# each pulmonary function tests at each visit and each treatment:
#     "n"        
#     "Mean (SD)"
#     "Median"   
#     "Q1, Q3"   
#     "Min, Max" 
#     "Missing" 

adpft %>% 
  tplyr_table(treat_var = TRTA, where= SAFFL =="Y") 

# E) (bonus) ----------------------------------------------------------------
# Get the same descriptive statistics as in (D) for the change from baseline of each
# pulmonary function test by visit and treatment

