#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#           main script
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#


# libraries
install.packages("tidyverse")
library("tidyverse")

#run this is conflicts occure in loading tidyverse
# library(conflicted)  
# library(tidyverse)
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
 
# Read the CSV files
file_path_dt <-("data/Proxy_models_ggc_gmba_dem.csv")
file_path_diff <- ("data/dt_difference.csv")
delta_t <- read_delim(file_path_dt,locale = locale(decimal_mark = "."))|>janitor::clean_names()
delta_t_diff <- read_delim(file_path_diff,locale = locale(decimal_mark = "."))|>janitor::clean_names()


  