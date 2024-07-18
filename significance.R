#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#            significance testing
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#
#create a variable containing data for all the models (not proxy and not ggc)
test_temp <- delta_t_diff %>% 
  select(beyer_diff_mean, chelsa_diff_mean, ecoclimate_diff_mean,paleopgem_diff_mean,worldclim25m_diff_mean,lat, gmted2010, in_mr) %>% 
  pivot_longer(cols = -c(in_mr,lat, gmted2010), names_to = "model", values_to = "values") %>% 
  mutate(in_mr=recode(in_mr, `0`="outside", `1`="within")) %>%
  filter(in_mr %in% c("outside", "within"))

# perform a paired t-test on the data in temp_data to test if the mean temperature is different between within and outside groups  or mr
welch_t_test <- t.test(values ~ in_mr, data = test_temp, paired = FALSE)

# Print results
print(welch_t_test)

#run again, for each model individually
t_test_beyer <- t.test(beyer_diff_mean ~ in_mr, data = delta_t_diff, paired = FALSE)
t_test_chelsa <- t.test(chelsa_diff_mean ~ in_mr, data = delta_t_diff, paired = FALSE)
t_test_ecoclimate <- t.test(ecoclimate_diff_mean ~ in_mr, data = delta_t_diff, paired = FALSE)
t_test_paleopgem <- t.test(paleopgem_diff_mean ~ in_mr, data = delta_t_diff, paired = FALSE)
t_test_worldclim <- t.test(worldclim25m_diff_mean ~ in_mr, data = delta_t_diff, paired = FALSE)

w_test_beyer <- wilcox.test(beyer_diff_mean ~ in_mr, data = delta_t_diff, paired = FALSE)
print(w_test_beyer)

# print all ttests
print(t_test_beyer)
print(t_test_chelsa)
print(t_test_ecoclimate)
print(t_test_paleopgem)
print(t_test_worldclim)

# Subset data for "within" group
within_values <- test_temp$values[test_temp$in_mr == "within"]

# Subset data for "outside" group
outside_values <- test_temp$values[test_temp$in_mr == "outside"]

# Perform Wilcoxon Signed Rank Test
wilcox_test_result <- wilcox.test(within_values, outside_values, paired = FALSE)

# Print results
print(wilcox_test_result)

#----------------------------------------------------------#
#      significance regression
#----------------------------------------------------------#
# Test the significance of latitude against delta_t_diff and within/outside against delta_t_diff
# Create an empty list to store the summaries
model_summaries <- list()

# Loop through each level of the model factor
for (m in levels(filtered_data$model)) {
  # Filter the data for the current model
  model_data <- filtered_data[filtered_data$model == m, ]
  
  # Fit the linear model for the current model
  model_fit <- lm(values ~ lat + in_mr, data = model_data)
  
  # Store the summary of the model in the list
  model_summaries[[m]] <- summary(model_fit)
}

# To view the summary for a specific model, you can use:
model_summaries[["ggc_diff_mean"]]


#----------------------------------------------------------#
#      Cohens D
#----------------------------------------------------------#

# calculate cohens D for each model, within vs outside from test_temp
# the method is a bit arbitrary, because I did it manually

# get the number of obervations (=same for all models)
n_within <- length(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "beyer_diff_mean"])
n_outside <- length(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "beyer_diff_mean"])

# BEYER
mean_beyer_within <- mean(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "beyer_diff_mean"], na.rm = TRUE)
sd_beyer_within <- sd(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "beyer_diff_mean"], na.rm = TRUE)
mean_beyer_outside <- mean(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "beyer_diff_mean"], na.rm = TRUE)
sd_beyer_outside <- sd(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "beyer_diff_mean"], na.rm = TRUE)
#pooled_sd_beyer <- sqrt(((sd_beyer_within^2 + sd_beyer_outside^2) / 2))
pooled_sd_beyer <- sqrt(((n_within - 1) * sd_beyer_within^2 + (n_outside - 1) * sd_beyer_outside^2) / (n_within + n_outside - 2))
cohens_d_beyer <- (mean_beyer_within - mean_beyer_outside) / pooled_sd_beyer
print(cohens_d_beyer)


# CHELSA
mean_chelsa_within <- mean(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "chelsa_diff_mean"], na.rm = TRUE)
sd_chelsa_within <- sd(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "chelsa_diff_mean"], na.rm = TRUE)
mean_chelsa_outside <- mean(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "chelsa_diff_mean"], na.rm = TRUE)
sd_chelsa_outside <- sd(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "chelsa_diff_mean"], na.rm = TRUE)
pooled_sd_chelsa <- sqrt(((n_within - 1) * sd_chelsa_within^2 + (n_outside - 1) * sd_chelsa_outside^2) / (n_within + n_outside - 2))
#pooled_sd_chelsa <- sqrt(((sd_chelsa_within^2 + sd_chelsa_outside^2) / 2))
cohens_d_chelsa <- (mean_chelsa_within - mean_chelsa_outside) / pooled_sd_chelsa
print(cohens_d_chelsa)

# ECOCLIMATE
mean_ecoclimate_within <- mean(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "ecoclimate_diff_mean"], na.rm = TRUE)
sd_ecoclimate_within <- sd(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "ecoclimate_diff_mean"], na.rm = TRUE)
mean_ecoclimate_outside <- mean(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "ecoclimate_diff_mean"], na.rm = TRUE)
sd_ecoclimate_outside <- sd(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "ecoclimate_diff_mean"], na.rm = TRUE)
#pooled_sd_ecoclimate <- sqrt(((sd_ecoclimate_within^2 + sd_ecoclimate_outside^2) / 2))
pooled_sd_ecoclimate <- sqrt(((n_within - 1) * sd_ecoclimate_within^2 + (n_outside - 1) * sd_ecoclimate_outside^2) / (n_within + n_outside - 2))
cohens_d_ecoclimate <- (mean_ecoclimate_within - mean_ecoclimate_outside) / pooled_sd_ecoclimate
print(cohens_d_ecoclimate)

# WORLDCLIM
mean_worldclim_within <- mean(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "worldclim25m_diff_mean"], na.rm = TRUE)
sd_worldclim_within <- sd(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "worldclim25m_diff_mean"], na.rm = TRUE)
mean_worldclim_outside <- mean(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "worldclim25m_diff_mean"], na.rm = TRUE)
sd_worldclim_outside <- sd(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "worldclim25m_diff_mean"], na.rm = TRUE)
#pooled_sd_worldclim <- sqrt(((sd_worldclim_within^2 + sd_worldclim_outside^2) / 2))
pooled_sd_worldclim <- sqrt(((n_within - 1) * sd_worldclim_within^2 + (n_outside - 1) * sd_worldclim_outside^2) / (n_within + n_outside - 2))
cohens_d_worldclim <- (mean_worldclim_within - mean_worldclim_outside) / pooled_sd_worldclim
print(cohens_d_worldclim)

# PALEOPGEM
mean_paleopgem_within <- mean(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "paleopgem_diff_mean"], na.rm = TRUE)
sd_paleopgem_within <- sd(test_temp$values[test_temp$in_mr == "within" & test_temp$model == "paleopgem_diff_mean"], na.rm = TRUE)
mean_paleopgem_outside <- mean(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "paleopgem_diff_mean"], na.rm = TRUE)
sd_paleopgem_outside <- sd(test_temp$values[test_temp$in_mr == "outside" & test_temp$model == "paleopgem_diff_mean"], na.rm = TRUE)
pooled_sd_paleopgem <- sqrt(((sd_paleopgem_within^2 + sd_paleopgem_outside^2) / 2))
#pooled_sd_paleopgem <- sqrt(((n_within - 1) * sd_paleopgem_within^2 + (n_outside - 1) * sd_paleopgem_outside^2) / (n_within + n_outside - 2))
cohens_d_paleopgem <- (mean_paleopgem_within - mean_paleopgem_outside) / pooled_sd_paleopgem
print(cohens_d_paleopgem)

