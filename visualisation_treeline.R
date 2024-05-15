#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#           visualisation treeline
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#

#----------------------------------------------------------#
#       Prepare data
#----------------------------------------------------------#
# Read the Excel file
file_path_tree <- "data/zonal_tree_all.xls"

# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path_tree)

# read each sheet and assign it to a variable
for (i in seq_along(sheet_names)) {
  assign(sheet_names[i], read_excel(file_path_tree, sheet = sheet_names[i]))
}

# Add a new column to each data frame to indicate the source
zonal_treeline_chelsa$source <- "chelsa"
#zonal_treeline_lr_lgm$source <- "lr"
zonal_treeline_beyer$source <- "beyer"
zonal_treeline_ecoclimate$source <- "ecoclimate"
zonal_treeline_pgem$source <- "pgem"
zonal_treeline_ggc$source <- "ggc"
zonal_treeline_worldclim$source <- "worldclim"

#combine all data frames
zonal_treeline_all <- bind_rows(zonal_treeline_chelsa, zonal_treeline_beyer, zonal_treeline_ecoclimate, zonal_treeline_pgem,zonal_treeline_worldclim, zonal_treeline_ggc)

# Filter the data to include only the levels of "Level_03" that are present in all sources
mr_in_all <- zonal_treeline_all %>%
  group_by(Level_03) %>%
  filter(all(c("chelsa","ecoclimate", "beyer", "pgem", "ggc", "worldclim") %in% source)) %>% 
  ungroup()

mr_in_both <- zonal_treeline_all %>%
  group_by(Level_03) %>%
  filter(all(c("chelsa", "ggc") %in% source)) %>% 
  ungroup()


#----------------------------------------------------------#
#      pointplot with range
#----------------------------------------------------------#
# Create the boxplots ACCIDENTALLY VIS THE SPREAD OF MIN MAX SD MEAN
# reshaped_data <- zonal_treeline_all %>%
#   select(Level_03, MIN, MAX, MEAN, STD) %>%
#   pivot_longer(cols = c(MIN, MAX, MEAN, STD), names_to = "Statistic", values_to = "Value")
# 
# ggplot(reshaped_data, aes(x = Level_03, y = Value, fill = Statistic)) +
#   geom_boxplot() +
#   labs(x = "Mountain Range", y = "Value") +
#   facet_wrap(~ Statistic, scales = "free") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))

# all
ggplot(zonal_treeline_all, aes(y = Level_03, x = MEAN, color = source)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("All mountain ranges with alpine biome") +
  theme_minimal()

# all
ggplot(mr_in_all, aes(y = Level_03, x = MEAN, color = source)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("All mountain ranges with data for all models") +
  theme_minimal()

# chelsa
ggplot(zonal_treeline_chelsa, aes(y = Level_03, x = MEAN)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("Chelsa") +
  theme_minimal()

# ggc
ggplot(zonal_treeline_ggc, aes(y = Level_03, x = MEAN)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("ggc") +
  theme_minimal()

# chelsa and ggc 
ggplot(mr_in_both %>% filter(source %in% c("chelsa", "ggc")), aes(y = Level_03, x = MEAN, color = source)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Elevation (m)") +
  ggtitle("Chelsa and ggc") +
  theme_minimal()


#----------------------------------------------------------#
#      barplot
#----------------------------------------------------------#

# Chelsa
ggplot(zonal_treeline_chelsa, aes(x = MEAN, y = reorder(Level_03, MEAN))) +
  geom_bar(stat = "identity") +
  labs(x = "Mean elevation (m)", y = "mountain range") +
  theme_minimal()


# combined plot (all mountain ranges)
ggplot(zonal_treeline_all, aes(x = MEAN, y = reorder(Level_03, MEAN), fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Mean elevation (m)", y = "mountain range") +
  ggtitle("Mean treeline elevation of all mountain ranges with alpine biome") +
  theme_minimal()

# combined plot filtered data (only mountain ranges present in all sources)
ggplot(mr_in_all, aes(x = MEAN, y = reorder(Level_03, MEAN), fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Mean elevation (m)", y = "Level 03") +
  ggtitle("All mountain ranges with data for all models") +
  theme_minimal()

# Calculate the difference in mean values between "chelsa" and "ggc"
mean_diff <- mr_in_both %>%
  filter(source %in% c("ggc", "chelsa")) %>%
  group_by(Level_03) %>%
  summarise(mean_diff = diff(MEAN)) %>%
  mutate(color = ifelse(mean_diff > 0, "blue", "red"))  # Assign colors based on the sign of the difference

# Create the bar plot
ggplot(mean_diff, aes(x = mean_diff, y = Level_03, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(x = "Difference of means in elevation Chelsa-ggc", y = "Mountain Range") +
  ggtitle("Mean difference in treeline elevation between Chelsa and ggc") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability

#----------------------------------------------------------#
#      boxplot
#----------------------------------------------------------#
# spread of mean accross all mountain ranges, compared for each model
ggplot(zonal_treeline_all, aes(x = source, y = MEAN, fill = source)) +
  geom_boxplot() +
  labs(x = "Model", y = "Mean elevation") +
  ggtitle("Spread of mean treeline elevation across mountain ranges") +
  theme_minimal()





