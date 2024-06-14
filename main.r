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

# read the GMBA .shp
file_path_gmba <-("data/GMBA.shp")
gmba <- sf::st_read(paste(file_path_gmba, sep = "/"))|>
  rename(mountain_range = MapName)|>
  rename(area_size = Area)|>
  mutate(area_size = round(area_size, 0))|>
  mutate(log_area = log(area_size))



