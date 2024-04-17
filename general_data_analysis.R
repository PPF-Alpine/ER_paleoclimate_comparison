#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#           general data analysis
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#


## create regression lines
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