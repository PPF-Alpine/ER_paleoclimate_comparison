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

summary(delta_t$mean_dt)

## Summary statistics
# Filter the dataset to include only rows within mountain ranges, and one only outside mountain ranges
delta_t_mountain <- delta_t %>%
  filter(in_mr == 1)

delta_t_lowlands <- delta_t %>%
  filter(in_mr == 0)

delta_t_diff_mountain <- delta_t_diff %>%
  filter(in_mr == 1)

delta_t_diff_lowlands <- delta_t_diff %>%
  filter(in_mr == 0)

#Transpose data to make plotting easier
delta_t_long <- delta_t|>
  dplyr::select(mean_dt, beyer_dt, chelsa_dt, ecoclimate_dt,paleopgem_dt,worldclim25m_dt,worldclim30s_dt,ggc_dt,gmted2010) |>
  pivot_longer(cols = -gmted2010,
               names_to = "Model",
               values_to = "Values")

delta_t_mr_long <- delta_t_mountain|>
  dplyr::select(mean_dt, beyer_dt, chelsa_dt, ecoclimate_dt,paleopgem_dt,worldclim25m_dt,worldclim30s_dt,ggc_dt,gmted2010) |>
  pivot_longer(cols = -gmted2010,
               names_to = "Model",
               values_to = "Values")

delta_diff_long <- delta_t_diff|>
  dplyr::select(beyer_diff_mean, chelsa_diff_mean, ecoclimate_diff_mean,paleopgem_diff_mean,worldclim25m_diff_mean,worldclim30s_diff_mean,ggc_diff_mean,gmted2010) |>
  pivot_longer(cols = -gmted2010,
               names_to = "Model",
               values_to = "Values")

delta_t_diff_mr_long <- delta_t_diff_mountain|>
  dplyr::select(beyer_diff_mean, chelsa_diff_mean, ecoclimate_diff_mean,paleopgem_diff_mean,worldclim25m_diff_mean,worldclim30s_diff_mean,ggc_diff_mean,gmted2010) |>
  pivot_longer(cols = -gmted2010,
               names_to = "Model",
               values_to = "Values")

#----------------------------------------------------------#
#       Plotting  summary data
#----------------------------------------------------------#

## scatterplots
# plot beyer mean and colour according to whether it is in or outside a mountain range
ggplot(delta_t_diff, aes(x = beyer_diff_mean, y = gmted2010, color = factor(in_mr))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("yellow", "purple")) +
  labs(y = "elevation", x = "beyer_diff_mean") +
  theme_minimal()

# now plot for all models
ggplot(delta_t_diff, aes(y = gmted2010, color = factor(in_mr))) +
  geom_point(aes(x = beyer_diff_mean), size = 3) +
  geom_point(aes(x = chelsa_diff_mean), size = 3) +
  geom_point(aes(x = ecoclimate_diff_mean), size = 3) +
  geom_point(aes(x = paleopgem_diff_mean), size = 3) +
  geom_point(aes(x = worldclim25m_diff_mean), size = 3) +
  geom_point(aes(x = worldclim30s_diff_mean), size = 3) +
  geom_point(aes(x = ggc_diff_mean), size = 3) +
  scale_color_manual(values = c("yellow", "purple")) +
  labs(x = "difference model-proxy", y = "elevation") +
  theme_minimal()

## Plot boxplots for spread of each model
# dT
ggplot(delta_t_mr_long, aes(x = Model, y = Values, fill = Model)) +
  geom_boxplot() +
  scale_fill_manual(values = c("mean_dt" = "red", "other_models" = "blue")) +  # Specify colors for "mean_dt" and other models
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT present-lgm for proxies within mountain ranges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(delta_t_diff_mr_long, aes(x = Model, y = Values, fill = Model)) +
  geom_boxplot() +
  scale_fill_manual(values = c("other_models" = "blue")) + 
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT diff (model-proxy value) within mountain ranges") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




