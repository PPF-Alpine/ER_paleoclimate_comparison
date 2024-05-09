#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#               visualisation
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#
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
ggplot(delta_t_mountain_long, aes(x = Model, y = Values, fill = Model)) +
  geom_boxplot() +
  scale_fill_manual(values = c("mean_dt" = "red", "other_models" = "blue")) +  # Specify colors for "mean_dt" and other models
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT present-lgm for proxies within mountain ranges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(delta_t_diff_mountain_long, aes(x = Model, y = Values, fill = Model)) +
  geom_boxplot() +
  scale_fill_manual(values = c("other_models" = "blue")) + 
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT diff (model-proxy value) within mountain ranges") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create violinplot of each model
ggplot(delta_t_diff_mountain_long, aes(x = Model, y = Values, fill = Model)) +
  geom_violin() +
  scale_fill_manual(values = c("mean_dt" = "red", "other_models" = "blue")) +  # Specify colors for "mean_dt" and other models
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT (present-lgm) difference (model-proxy) WITHIN mountains") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(delta_t_diff_lowlands_long, aes(x = Model, y = Values, fill = Model)) +
  geom_violin() +
  scale_fill_manual(values = c("mean_dt" = "red", "other_models" = "blue")) +  # Specify colors for "mean_dt" and other models
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT (present-lgm) difference (model-proxy) OUTSIDE mountains") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



