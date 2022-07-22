library(Tplyr)
library(haven)
library(dplyr)

# Here we will start the ARD for the demography table 

# Read in dataset 
adsl <- read_xpt("datasets/ADAM/adsl.xpt")

# The first step in building a cake is having a base to put it on. In {Tplyr} you use `tplyr_table` to set that base. 
adsl %>% 
  tplyr_table(treat_var = TRT01A, where= SAFFL =="Y") 

# Now we can add our first layer! Lets start with SEX, which has two categories, so will be a count layer
adsl %>% 
  tplyr_table(treat_var = TRT01A, where= SAFFL =="Y") %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex", "n (%)") )
  )

# To check the layers we can use `get_numeric_data` 
adsl %>% 
  tplyr_table(treat_var = TRT01A, where= SAFFL =="Y") %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex", "n (%)") )
  ) %>% 
  get_numeric_data()

# This looks great, but the mock has a "total" column so we need to add that in.
# To do this, we will add a special layer, `add_total_group`
adsl %>% 
  tplyr_table(treat_var = TRT01A, where= SAFFL =="Y") %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex", "n (%)") )
  ) %>% 
  add_total_group() %>% 
  get_numeric_data()


# Now, we can move on to the next layer, AGE. AGE is continuous so we will need
# `group_desc`
adsl %>% 
  tplyr_table(treat_var = TRT01A, where= SAFFL =="Y") %>% 
  add_total_group() %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex", "n (%)") )
  ) %>% 
  add_layer(
    group_desc(target_var =AGE, by = "Age (Years)")
  ) %>% 
  get_numeric_data() %>% 
  tail(18)

# Something that will be important for the exercises is passing the full
# population to {Tplyr}. For example, the AE table is based on an AE dataset
# that doesn't have all subjects in it. So you will need add the total
# population from a different source to get the correct total N.
adsl %>% 
  tplyr_table(treat_var = TRT01A, where= SAFFL =="Y") %>% 
  set_pop_data(adsl) %>% 
  add_total_group() %>% 
  add_layer(
    group_count(target_var = SEX, by = vars("Sex", "n (%)") )
  ) %>% 
  get_numeric_data() 
