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

# split violinplot with on the left side delta_t_diff_mountain_long and on the right side delta_t_diff_lowlands_long, for each model
ggplot(delta_t_diff_mountain_long, aes(x = Model, y = Values, fill = Model)) +
  geom_violin(position = "dodge") +
  geom_violin(data = delta_t_diff_lowlands_long, position = "dodge") +
  scale_fill_manual(values = c("mean_dt" = "red", "other_models" = "blue")) + 
  labs(x = "Model", y = "ΔT") +
  ggtitle("ΔT (present-lgm) difference (model-proxy) WITHIN and OUTSIDE mountains") +
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### test
ggplot(dat_long, aes(x = condition, y = rt, fill = language)) +
  introdataviz::geom_split_violin(alpha = .4, trim = FALSE) +
  geom_boxplot(width = .2, alpha = .6, fatten = NULL, show.legend = FALSE) +
  stat_summary(fun.data = "mean_se", geom = "pointrange", show.legend = F, 
               position = position_dodge(.175)) +
  scale_x_discrete(name = "Condition", labels = c("Non-word", "Word")) +
  scale_y_continuous(name = "Reaction time (ms)",
                     breaks = seq(200, 800, 100), 
                     limits = c(200, 800)) +
  scale_fill_brewer(palette = "Dark2", name = "Language group") +
  theme_minimal()