# Install script 
#Gernearal Packages 
gen_pakages <- c("tidyverse",
                 "haven",
                 "gt",
                 "gtExtras",
                 "devtools",
                 "lme4",
                 "emmeans",
                 "broom",
                 "pbkrtest",
                 "lmerTest") 
#Check which packages you need 
needed_pkgs <- gen_pakages[which(!gen_pakages %in% installed.packages())]
install.packages(needed_pkgs)

library(devtools)
# Pharmaverse packages 
install_github("GSK-Biostatistics/tfrmt")
install_github("pharmaverse/metatools@dev")
install_github("atorus-research/Tplyr@gh_issue_53_desc")
install_github("atorus-research/metacore@dev")
install.packages("admiral")
