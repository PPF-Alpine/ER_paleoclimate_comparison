#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#           exploratory data analysis
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#

#----------------------------------------------------------#
#       summarise the data
#----------------------------------------------------------#
#Descriptive functions
names(delta_t) 
names(delta_t_diff)
typeof(delta_t$mean_dt)
head(delta_t)
glimpse(delta_t)
glimpse(delta_t_diff)

#----------------------------------------#
#       Descriptive statistics
#     get NA, min, max, sd, mean
#----------------------------------------#

# create vector for all columns with dT values
models_dt <- c("mean_dt", "beyer_dt", "chelsa_dt", "ecoclimate_dt", "paleopgem_dt", "worldclim25m_dt", "worldclim30s_dt", "ggc_dt")

# create a for loop to get the min, max, mean and sd value for each of these columns, ignore NA values
for (model in models_dt) {
  print(model)
  print(paste("Min:", min(delta_t[[model]], na.rm = TRUE)))
  print(paste("Max:", max(delta_t[[model]], na.rm = TRUE)))
  print(paste("Mean:", mean(delta_t[[model]], na.rm = TRUE)))
  print(paste("SD:", sd(delta_t[[model]], na.rm = TRUE)))
}

# get na values in these columns
for (model in models_dt) {
  print(model)
  print(sum(is.na(delta_t[[model]])))
}

#repeat for dT difference
models_dt_diff <- c("beyer_diff_mean", "chelsa_diff_mean", "ecoclimate_diff_mean", "paleopgem_diff_mean", "worldclim25m_diff_mean", "worldclim30s_diff_mean", "ggc_diff_mean")
for (model in models_dt_diff) {
  print(model)
  print(paste("Min:", min(delta_t_diff[[model]], na.rm = TRUE)))
  print(paste("Max:", max(delta_t_diff[[model]], na.rm = TRUE)))
  print(paste("Mean:", mean(delta_t_diff[[model]], na.rm = TRUE)))
  print(paste("SD:", sd(delta_t_diff[[model]], na.rm = TRUE)))
}

#get na values in these columns
for (model in models_dt_diff) {
  print(model)
  print(sum(is.na(delta_t_diff[[model]])))
}

#----------------------------------------#
#   look into proxy data (mean_dt)
#     plot outliers 
#----------------------------------------#
summary(delta_t$mean_dt) # max value is very high. Any positive value is suspicious (lgm logically would be colder), so lets check which ones are >0
delta_t %>%
  filter(mean_dt > 0) 
# show the mean_dt value that is >0
delta_t %>%
  filter(mean_dt > 0) %>%
  select(mean_dt)

# Determine how many proxies in delta_t are "within" and "outside" mr AND have variable name mean_dt
temp_data %>% 
  filter(model == "mean_dt") %>% 
  group_by(in_mr) %>% 
  summarise(n = n())

# plot on a map to see where these values are located, use the column lat and long
# plot the outline of the world in the background
world <- map_data("world")
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
  geom_point(data = delta_t %>%
               filter(mean_dt > 0), aes(x = long, y = lat), color = "red") +
  theme_minimal()

# show the value of the point.
ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey") +
  geom_point(data = delta_t %>%
               filter(mean_dt > 0), aes(x = long, y = lat), color = "red") +
  geom_label(data = delta_t %>%
               filter(mean_dt > 0), aes(x = long, y = lat, label = mean_dt),
             color = "black", nudge_x = 0.5, check_overlap=TRUE) +
  theme_minimal()

#----------------------------------------#
#   Remove outliers from mean_dt
#----------------------------------------#
# remove the outliers from the dataset with a value higher than 7
delta_t_no_outliers <- delta_t %>%
  filter(mean_dt <= 7)
delta_t_no_outliers %>% filter(mean_dt>0) %>% select(mean_dt) #check if it worked

#repeat for delta_t_diff
delta_t_diff_no_outliers <- delta_t_diff %>%
  filter(mean_dt <= 7)
delta_t_diff_no_outliers %>% filter(mean_dt>0) %>% select(mean_dt) #check if it worked


## run this if you want to continue without outliers. 
#delta_t <- delta_t_no_outliers
#delta_t_diff <- delta_t_diff_no_outliers


