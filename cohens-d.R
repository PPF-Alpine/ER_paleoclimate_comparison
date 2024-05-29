#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#                 Cohen's D
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#

# calculate cohens D for each model, within vs outside from test_temp
# the method is a bit arbitrary, because I did not get the desired results from the package function, so I did it manually

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

