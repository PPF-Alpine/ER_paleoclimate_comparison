#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#           visualisation temperature
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#
### test examples from psyteachr: https://psyteachr.github.io/msc-conv/vis.html#raincloud-plots-with-multiple-factors

# how to install the introdataviz package
devtools::install_github("psyteachr/introdataviz")

#----------------------------------------------------------#
#       Prepare data
#----------------------------------------------------------#
temp_data <- delta_t %>% 
  select(mean_dt, beyer_dt, chelsa_dt, ecoclimate_dt,paleopgem_dt,worldclim25m_dt,worldclim30s_dt,ggc_dt,lat, gmted2010, in_mr) %>% 
  pivot_longer(cols = -c(in_mr,lat, gmted2010), names_to = "model", values_to = "values")%>% 
  mutate(in_mr=recode(in_mr, `0`="outside", `1`="within")) %>%
  filter(in_mr %in% c("outside", "within"))

temp_diff_data <- delta_t_diff %>% 
  select(beyer_diff_mean, chelsa_diff_mean, ecoclimate_diff_mean,paleopgem_diff_mean,worldclim25m_diff_mean,worldclim30s_diff_mean,ggc_diff_mean,lat, gmted2010, in_mr) %>% 
  pivot_longer(cols = -c(in_mr,lat, gmted2010), names_to = "model", values_to = "values") %>% 
  mutate(in_mr=recode(in_mr, `0`="outside", `1`="within")) %>%
  filter(in_mr %in% c("outside", "within"))

#----------------------------------------------------------#
#      Histograms
#----------------------------------------------------------#
# temp_data
ggplot(temp_data, aes(values, fill = in_mr)) + 
  geom_histogram(alpha = .5, position = "identity", bins = 60) + 
  ggtitle("ΔT distribution all models") + 
  labs(x = "ΔT (°C)", y = "Frequency") + 
  theme_minimal()

# temp_diff_data
ggplot(temp_diff_data, aes(values, fill = in_mr)) + 
  geom_histogram(alpha = .5, position = "identity", bins = 60) + 
  ggtitle("ΔT difference (model-proxy) distribution all models") + 
  labs(x = "ΔT (°C)", y = "Frequency") + 
  geom_vline(xintercept = 0, color = "red") + # Add vertical red line at x = 0
  theme_minimal()

# chelsa
ggplot(filter(temp_diff_data, model == "chelsa_diff_mean"), aes(values, fill = in_mr)) + 
  geom_histogram(alpha = .5, position = "identity", bins = 60) + 
  ggtitle("ΔT difference (model-proxy) distribution chelsa") + 
  labs(x = "ΔT (°C)", y = "Frequency") + 
  # include stat function to add normal density
  stat_function(fun = dnorm, args = list(mean = mean(filter(temp_diff_data, model == "chelsa_diff_mean")$values), 
                                         sd = sd(filter(temp_diff_data, model == "chelsa_diff_mean")$values)), 
                geom = "line", col = "blue") +
  geom_vline(xintercept = 0, color = "red") + # Add vertical red line at x = 0
  theme_minimal()

#paleo-pgem
ggplot(filter(temp_diff_data, model == "paleopgem_diff_mean"), aes(values, fill = in_mr)) + 
  geom_histogram(alpha = .5, position = "identity", bins = 60) + 
  ggtitle("ΔT difference (model-proxy) distribution paleo-pgem") + 
  labs(x = "ΔT (°C)", y = "Frequency") + 
  # include stat function to add normal density
  stat_function(fun = dnorm, args = list(mean = mean(filter(temp_diff_data, model == "paleopgem_diff_mean")$values), 
                                         sd = sd(filter(temp_diff_data, model == "paleopgem_diff_mean")$values)), 
                geom = "line", col = "blue") +
  geom_vline(xintercept = 0, color = "red") + # Add vertical red line at x = 0
  theme_minimal()

#----------------------------------------------------------#
#      Scatterplots
#----------------------------------------------------------#
# temp_data to latitude
ggplot(temp_data, aes(lat, values, color = in_mr)) + 
  geom_point(alpha = .5, position = "identity") + 
  ggtitle("ΔT latitudinal distribution all models") + 
  labs(x = "Latitude", y = "ΔT (°C)") + 
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  geom_smooth(method=lm)
  theme_minimal()

# temp_diff_data to latitude
ggplot(temp_diff_data, aes(lat, values, color = in_mr)) + 
  geom_point(alpha = .5, position = "identity") + 
  ggtitle("ΔT difference (model-proxy) latitudinal distribution all models") + 
  labs(x = "Latitude", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  #geom_smooth(method=lm)
  theme_minimal()

# chelsa
ggplot(filter(temp_diff_data, model == "chelsa_diff_mean"), aes(lat, values, color = in_mr)) + 
  geom_point(alpha = .5, position = "identity") + 
  ggtitle("ΔT difference (model-proxy) latitudinal distribution chelsa") + 
  labs(x = "Latitude", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  #geom_smooth(method=lm)
  theme_minimal()

# paleo pgem
ggplot(filter(temp_diff_data, model == "paleopgem_diff_mean"), aes(lat, values, color = in_mr)) + 
  geom_point(alpha = .5, position = "identity") + 
  ggtitle("ΔT difference (model-proxy) latitudinal distribution paleopgem") + 
  labs(x = "Latitude", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  #geom_smooth(method=lm)
  theme_minimal()

#----------------------------------------------------------#
#      Boxplots
#----------------------------------------------------------#
# temp_data, all models, within mountains
ggplot(filter(temp_data, in_mr == "within"), aes(model, values, fill = model)) + 
  geom_boxplot(alpha = .5, position = "identity") + 
  ggtitle("ΔT distribution within mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  theme_minimal()

# temp_diff_data, all models, within mountains 
ggplot(filter(temp_diff_data, in_mr == "within"), aes(model, values, fill = model)) + 
  geom_boxplot(alpha = .5, position = "identity") + 
  #geom_jitter(width = 0.2, alpha = 0.5) + 
  ggtitle("ΔT difference (model-proxy) distribution within mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal()

# temp_diff_data, all, within and outside 
ggplot(temp_diff_data, aes(model, values, fill = in_mr)) + 
  geom_boxplot(alpha = .5, position = "dodge") + 
  ggtitle("ΔT difference (model-proxy) distribution within and outside mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal black line at y = 0
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  theme_minimal()


#----------------------------------------------------------#
#      violinplots
#----------------------------------------------------------#

# temp_diff_data, all models, within mountains 
ggplot(filter(temp_diff_data, in_mr == "within"), aes(model, values, fill = model)) + 
  geom_violin(alpha = .5, position = "identity") + 
  #geom_jitter(width = 0.2, alpha = 0.5) + 
  ggtitle("ΔT difference (model-proxy) distribution within mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal()

# temp_diff_data, all, within and outside 
ggplot(temp_diff_data, aes(model, values, fill = in_mr)) + 
  geom_violin(alpha = .5, position = "dodge") + 
  ggtitle("ΔT difference (model-proxy) distribution within and outside mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal black line at y = 0
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  theme_minimal()


#----------------------------------------------------------#
#      violin-boxplots
#----------------------------------------------------------#
# temp_diff_data, all models, within mountains, violinplot and a boxplot layered on top of it
ggplot(filter(temp_diff_data, in_mr == "within"), aes(model, values, fill = model)) + 
  geom_violin(alpha = .5, position = "identity") + 
  geom_boxplot(alpha = .5, position = "identity", width = 0.3) + 
  ggtitle("ΔT difference (model-proxy) distribution within mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal red line at y = 0
  theme_minimal()


# temp_diff_data, all, within and outside 
# ERROR: BOXPOTS ARE NOT CENTERED AND CENTERING FUNCTION IS NOT SUPPORTED IN GEOM_BOXPLOT
ggplot(temp_diff_data, aes(model, values, fill = in_mr)) + 
  geom_violin(alpha = .5, position = "dodge") +
  geom_boxplot(alpha = .5, position = "dodge", width = 0.3) +
  ggtitle("ΔT difference (model-proxy) distribution within and outside mountain ranges") + 
  labs(x = "Model", y = "ΔT (°C)") + 
  geom_hline(yintercept = 0, color = "red", linetype = "solid") +  # Add horizontal black line at y = 0
  scale_fill_manual(values = c("within" = "orange", "outside" = "blue")) +  # Specify colors for "within" and "outside"
  theme_minimal()















