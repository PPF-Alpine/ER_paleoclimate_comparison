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

# Run "required_packages.R" to install and load the necessary packages  

#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#

# Read the CSV files
file_path_dt <-("data/Proxy_models_ggc_gmba_dem.csv")
file_path_diff <- ("data/dt_difference.csv")
delta_t <- read_delim(file_path_dt, delim = ",", locale = locale(decimal_mark = "."))|>janitor::clean_names() 
delta_t_diff <- read_delim(file_path_diff, delim = ",", locale = locale(decimal_mark = "."))|>janitor::clean_names()

### Also delta_t and  delta_t_diff are close to having the same data and structure, why not one file?






