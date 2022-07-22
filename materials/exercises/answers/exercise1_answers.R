# Exercise 1 - Warm-up 

library(tidyverse)
library(haven)

# A) ----------------------------------------------------------------------
# Using {haven}, read in the ADSL ("/datasets/ADAM/ADSL") and ADAE data ("/datasets/ADAM/ADAE")
adae <- read_xpt("datasets/ADAM/adae.xpt")
adsl <- read_xpt("datasets/ADAM/adsl.xpt") 

# B) ----------------------------------------------------------------------
# Using the {tidyverse}:

# (i) Use ADSL to calculate “Big N” for each treatment (TRT01A)

bign <- adsl %>% 
  group_by(TRT01A) %>% 
  summarise(N = n())

# (ii) Generate counts of distinct subject (USUBJID) AE’s by 
#      - system organ class (AEBODSYS) and 
#      - reported term (AETERM), and by 
#      - treatment (TRT01A)

counts <- adae %>% 
  group_by(TRT01A, AEBODSYS, AETERM) %>% 
  distinct(USUBJID) %>% 
  summarise(distinct_n = n())

# (iii) Merge on/join the Big N values and calculate percentages (pct) of each
# reported term by system organ class and treatment

all <- counts %>% 
  right_join(bign) %>% 
  mutate(distinct_pct = distinct_n/N)
