#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#                   t-test
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


# Welch Two Sample t-test

# t = -7.9642 >> means that the size of the first group (outside) is bigger than the second group (within)
# df = 829.07 >> 
# p-value = 5.475e-15 >> extremely small, indicating that the difference is significant
# alternative hypothesis: true difference in means between group outside and group within is not equal to 0
# 95 percent confidence interval:
#   -2.257197 -1.364581 >> means that you can be 90% confident that the difference falls between this range
# sample estimates:
#   mean in group outside  mean in group within 
# -5.927359             -4.116470 
