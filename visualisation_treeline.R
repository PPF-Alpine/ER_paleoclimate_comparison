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
# Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(file_path_tree)

# read each sheet and assign it to a variable
for (i in seq_along(sheet_names)) {
  assign(sheet_names[i], read_excel(file_path_tree, sheet = sheet_names[i]))
}

# Add a new column to each data frame to indicate the source
zonal_treeline_chelsa$source <- "chelsa"
zonal_treeline_beyer$source <- "beyer"
zonal_treeline_ecoclimate$source <- "ecoclimate"
zonal_treeline_pgem$source <- "pgem"
zonal_treeline_ggc$source <- "ggc"
zonal_treeline_worldclim$source <- "worldclim"

#combine all data frames
zonal_treeline_all <- bind_rows(zonal_treeline_chelsa, zonal_treeline_beyer, zonal_treeline_ecoclimate, zonal_treeline_pgem,zonal_treeline_worldclim, zonal_treeline_ggc)

# # export zonal_treeline_all as csv
# write.csv(zonal_treeline_all, "data/zonal_treeline_all.csv")

#----------------------------------------------------------#
#       Filter data
#----------------------------------------------------------#
# Filter the data to include only the levels of "Level_03" that are present in all sources
mr_in_all <- zonal_treeline_all %>%
  group_by(Level_03) %>%
  filter(all(c("chelsa","ecoclimate", "beyer", "pgem", "ggc", "worldclim") %in% source)) %>% 
  ungroup()

mr_in_both <- zonal_treeline_all %>%
  group_by(Level_03) %>%
  filter(all(c("chelsa", "ggc") %in% source)) %>% 
  ungroup()

# calculate range 
range_treelines <- zonal_treeline_all %>%
  group_by(Level_03) %>%
  summarise(
    Min = min(MIN),
    Max = max(MAX),
    STD = mean(STD),
    Range = Max - (Min), # Calculate the range
    Models = toString(unique(source))
  )
## save ranges as csv
#write.csv(range_treelines, "data/range_treelines.csv")

#Add lat long data from mountains_xy to zonal_treeline_all based on their common names in MapName and Level_03 respectively
zonal_treeline_all <- left_join(zonal_treeline_all, mountains_xy[, c("MapName", "lat", "long")], by = c("Level_03"="MapName"))

#----------------------------------------------------------#
#      latitude plot with range and errorbar
#----------------------------------------------------------#
# Define a vector with your desired colors
colors <- c("chelsa" = "#41afaa", "beyer" = "#466eb4", "ecoclimate" = "#e6a532", "pgem" = "#af4b91", "worldclim" = "#00a0e1", "ggc" = "#d7642c")

legend_names <- c("chelsa" = "CHELSA", "beyer" = "BEYER", "ecoclimate" = "ECOCLIMATE", "pgem" = "PALEO-PGEM", "worldclim" = "WORLDCLIM", "ggc" = "GGC")

order <- c("chelsa", "worldclim","beyer", "ecoclimate", "pgem", "ggc")
zonal_treeline_all$source <- factor(zonal_treeline_all$source, levels = order)

#shapes <- c("chelsa" = 16, "beyer" = 17, "ecoclimate" = 18, "pgem" = 15, "worldclim" = 20, "ggc" = 8)

# Create the plot with the specified legend order
ggplot(zonal_treeline_all, aes(x = lat, y = MEAN, group = source)) +
  geom_point(aes(color = source)) +  # Plot the mean values with color
  geom_line(aes(color = source)) +  # Draw a line through the mean values of each source with color
  geom_errorbar(aes(ymin = MIN, ymax = MAX, color = source), width = 0.2) +  # Add colored vertical error bars for standard deviation
  scale_color_manual(values = colors, labels = legend_names) +  # Manually assign colors and legend names
  labs(x = "Latitude", y = "Mean elevation (m)", color = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(), #element_text(size = 12)) #  removes text from titles
        panel.spacing = unit(1, "lines"),  # Adjust the amount of space as needed
        panel.background= element_rect(fill = "grey99"), # Add a grey background to the plot
        panel.border = element_rect(colour="white", fill=NA))

# Create a data frame with unique Level_03 and corresponding latitudes
unique_levels <- unique(zonal_treeline_all[, c("Level_03", "lat")])

# Order the unique levels by latitude
ordered_levels <- unique_levels$Level_03[order(unique_levels$lat)]

# Create the ordered factor for Level_03
zonal_treeline_all$Level_03_ordered <- factor(zonal_treeline_all$Level_03, levels = ordered_levels)

# Update the ggplot code to use 'Level_03_ordered' for the x-axis
ggplot(zonal_treeline_all, aes(x = Level_03_ordered, y = MEAN, group = source)) +
  geom_point(aes(color = source)) +  # Plot the mean values with color
  geom_line(aes(color = source)) +  # Draw a line through the mean values of each source with color
  geom_errorbar(aes(ymin = MIN, ymax = MAX, color = source), width = 0.2) +  # Add colored vertical error bars
  scale_color_manual(values = colors, labels = legend_names) +  # Manually assign colors and legend names
  labs(x = "Mountain Name", y = "Mean elevation (m)", color = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate the x-axis text for better readability
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background= element_rect(fill = "grey99"),
        panel.border = element_rect(colour="white", fill=NA))


#----------------------------------------------------------#
#      pointplot with range
#----------------------------------------------------------#

# elevation vs mountains ordered by lat on x axis all models
ggplot(zonal_treeline_all, aes(x = Level_03_ordered, y = MEAN, group = source)) +
  geom_point(aes(color = source)) +  # Plot the mean values with color
  #geom_line(aes(color = source)) +  # Draw a line through the mean values of each source with color
  geom_errorbar(aes(ymin = MIN, ymax = MAX, color = source), width = 0.2) +  # Add colored vertical error bars
  scale_color_manual(values = colors, labels = legend_names) +  # Manually assign colors and legend names
  labs(x = "Mountain Name", y = "Mean elevation (m)", color = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate the x-axis text for better readability
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background= element_rect(fill = "grey99"),
        panel.border = element_rect(colour="white", fill=NA))

#Create lines to show which models have data for which mountains
ggplot(zonal_treeline_all, aes(x = Level_03_ordered, y = source)) +
  geom_point(aes(color = source)) +  # Plot the mean values with color
  geom_line(aes(color = source)) +  # Draw a line through the mean values of each source with color
  scale_color_manual(values = colors, labels = legend_names) +  # Manually assign colors and legend names
  labs(x = "Mountain Name", color = "Source") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate the x-axis text for better readability
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background= element_rect(fill = "white"),
        panel.border = element_rect(colour="white", fill=NA))+
  scale_y_discrete(expand = c(1, 0))

# all flipped orientation
ggplot(zonal_treeline_all, aes(y = Level_03, x = MEAN, color = source)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  scale_color_manual(values = colors, labels = legend_names)
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("All mountain ranges with alpine biome") +
  theme_minimal()+
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        legend.title = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(), #element_text(size = 12)) #  removes text from titles
        panel.spacing = unit(1, "lines"),  # Adjust the amount of space as needed
        panel.background= element_rect(fill = "grey99"), # Add a grey background to the plot
        panel.border = element_rect(colour="white", fill=NA))

# all mountains with data for all models
ggplot(mr_in_all, aes(y = Level_03, x = MEAN, color = source)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("All mountain ranges with data for all models") +
  theme_minimal()

# all mountains with data for chelsa
ggplot(zonal_treeline_chelsa, aes(y = Level_03, x = MEAN)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("Chelsa") +
  theme_minimal()

# all mountains with data for ggc
ggplot(zonal_treeline_ggc, aes(y = Level_03, x = MEAN)) +
  geom_point() +  # Plot the mean values
  geom_errorbarh(aes(xmin = MIN, xmax = MAX), height = 0.2) +  # Add horizontal error bars for the range
  labs(y = "Mountain Range", x = "Mean elevation (m)") +
  ggtitle("ggc") +
  theme_minimal()

# all mountains with data for chelsa and ggc 
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
# spread of mean across all mountain ranges, compared for each model
ggplot(zonal_treeline_all, aes(x = source, y = MEAN, fill = source)) +
  geom_boxplot() +
  labs(x = "Model", y = "Mean elevation") +
  ggtitle("Spread of mean treeline elevation across mountain ranges") +
  theme_minimal()





