# libraries
install.packages("tidyverse")
library("tidyverse")

# library(conflicted)  
# 
# library(tidyverse)
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
 
# Read the CSV files
file_path_dt <-("data/Proxy_models_ggc_gmba_dem.csv")
file_path_diff <- ("data/dt_difference.csv")
delta_t <- read_delim(file_path_dt,locale = locale(decimal_mark = "."))|>janitor::clean_names()
delta_t_diff <- read_delim(file_path_diff,locale = locale(decimal_mark = "."))|>janitor::clean_names()


#Descriptive functions
names(delta_t)
names(delta_t_diff)
typeof(delta_t$mean_dt)

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

#Transpose data
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

### create regression lines
# Filter data for in_mr = 1 and fit linear regression
regression_1 <- lm(gmted2010 ~ beyer_diff_mean + chelsa_diff_mean + ecoclimate_diff_mean +
                     paleopgem_diff_mean + worldclim25m_diff_mean + worldclim30s_diff_mean + ggc_diff_mean,
                   data = filter(delta_t_diff, in_mr == 1))

# Filter data for in_mr = 0 and fit linear regression
regression_0 <- lm(gmted2010 ~ beyer_diff_mean + chelsa_diff_mean + ecoclimate_diff_mean +
                     paleopgem_diff_mean + worldclim25m_diff_mean + worldclim30s_diff_mean + ggc_diff_mean,
                   data = filter(delta_t_diff, in_mr == 0))
# Fit linear regression over all points
regression_all <- lm(gmted2010 ~ beyer_diff_mean + chelsa_diff_mean + ecoclimate_diff_mean +
                       paleopgem_diff_mean + worldclim25m_diff_mean + worldclim30s_diff_mean + ggc_diff_mean,
                     data = delta_t_diff)
# Plotting
ggplot(delta_t_diff, aes(y = gmted2010, color = factor(in_mr))) +
  geom_point(aes(x = beyer_diff_mean)) +
  geom_smooth(data = filter(delta_t_diff, in_mr == 1), aes(x = beyer_diff_mean), method = "lm", se = FALSE, color = "purple") +
  geom_smooth(data = filter(delta_t_diff, in_mr == 0), aes(x = beyer_diff_mean), method = "lm", se = FALSE, color = "yellow") +
  scale_color_manual(values = c("yellow", "purple")) +
  labs(x = "difference Beyer-proxy", y = "elevation") +
  theme_minimal()

# Function to create plot with linear regression
create_plot <- function(data, x_var, y_var) {
  ggplot(data, aes_string(x = y_var, y = x_var, color = "factor(in_mr)")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_color_manual(values = c("yellow", "purple")) +
    labs(x = y_var, y = "elevation") +  
    theme_minimal()
}

# List to store plots
plots <- list()

# Columns to iterate over
columns <- c("beyer_diff_mean", "chelsa_diff_mean", "ecoclimate_diff_mean",
             "paleopgem_diff_mean", "worldclim25m_diff_mean",
             "worldclim30s_diff_mean", "ggc_diff_mean")

# Create plots for each column
for (col in columns) {
  plots[[col]] <- create_plot(delta_t_diff, "gmted2010", col)  # Flipped x and y axes
}

# Arrange plots in a grid
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)

  