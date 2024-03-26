# libraries
# install.packages("dplyr")
# install.packages("ggplot2")
# library("dplyr")
# library("ggplot2")

# Read the CSV file
delta_t <- read.csv("./Proxy_models_ggc_gmba_dem.csv", sep = ";")

# Rename the columns
colnames(delta_t)[which(names(delta_t) %in% c("Mean_DT", "beyer_dt", "chelsa_dt", "ecoclimate_dt", "paleopgem_dt", "worldclim30s_dt", "worldclim25m_dt", "ggc_dt"))] <- c("proxy", "beyer", "chelsa", "ecoclimate", "paleopgem", "worldclim30s", "worldclim25m", "ggc")


# Create a copy of the original data frame
delta_t_diff <- delta_t


# Plot the data
ggplot(delta_t_diff, aes(x = GMTED2010)) +
    geom_point(aes(y = chelsa)) +
    geom_point(aes(y = ecoclimate)) +
    geom_point(aes(y = paleopgem)) +
    geom_point(aes(y = worldclim30s)) +
    geom_point(aes(y = worldclim25m)) +
    geom_point(aes(y = ggc)) +
    xlab("Elevation") +
    ylab("Temperature Differences") +
    theme_minimal()

