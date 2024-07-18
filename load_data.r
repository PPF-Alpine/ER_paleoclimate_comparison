#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#                 load data
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#

# Run "required_packages.R" to install and load the necessary packages  

#----------------------------------------------------------#
# Load data - TEMPERATURE
#----------------------------------------------------------#
# Read the CSV files
file_path_dt <-("data/delta_t.csv")
file_path_diff <- ("data/delta_t_diff.csv")
delta_t <- read_delim(file_path_dt, delim = ",", locale = locale(decimal_mark = "."))|>janitor::clean_names() 
delta_t_diff <- read_delim(file_path_diff, delim = ",", locale = locale(decimal_mark = "."))|>janitor::clean_names()

# > Go to "visualisation_temp.R" to continue with visualisation of temperature data
# or to "significance.R" to continue with the significance testing

#----------------------------------------------------------#
# Load data - TREELINES
#----------------------------------------------------------#
# Read the Excel file
file_path_tree <- "data/zonal_tree_all.xls"
file_path_xy <- "data/mountains_xy.xls"
mountains_xy <- read_excel(file_path_xy)

# > Go to "visualisation_treelines.R" to continue with visualisation of treeline data