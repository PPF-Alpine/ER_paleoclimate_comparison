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


#----------------------------------------------------------#
# 1. Load packages -----
#----------------------------------------------------------#


# libraries
# install.packages("tidyverse") # SF you dont have to install again after you have done it
library("tidyverse")

## SF: WHen calling a function, always specify the package by dplyr:: e.g. that way you wont have to specify this here
#run this if conflicts occur in loading tidyverse
# library(conflicted)  
# library(tidyverse)
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
 

#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#

# Read the CSV files
file_path_dt <-("data/Proxy_models_ggc_gmba_dem.csv") # SF: why create something that you don't use further?
file_path_diff <- ("data/dt_difference.csv")
delta_t <- read_delim(file_path_dt,locale = locale(decimal_mark = "."))|>janitor::clean_names() # SF: I cant run this without delimitator
delta_t_diff <- read_delim(file_path_diff,locale = locale(decimal_mark = "."))|>janitor::clean_names()


# SF: suggestion
# delta_t <- read_delim(file_path_dt, delim = ",", locale = locale(decimal_mark = ".")) |> janitor::clean_names()
# delta_t_diff <- read_delim(file_path_diff, delim = ",", locale = locale(decimal_mark = "."))|>janitor::clean_names()
### Also delta_t and  delta_t_diff are close to having the same data and structure, why not one file?