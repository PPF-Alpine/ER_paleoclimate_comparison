# libraries
install.packages("tidyverse")
library("tidyverse")
 
# Read the CSV file
file_path_dt <-("C:\\Users\\elren9761\\OneDrive - University of Bergen\\Documents\\PhD\\Analyses\\visualisation_dT\\Proxy_models_ggc_gmba_dem.csv")
file_path_diff <- ("C:\\Users\\elren9761\\OneDrive - University of Bergen\\Documents\\PhD\\Analyses\\visualisation_dT\\dt_difference.csv")
delta_t <- read_delim(file_path_dt,locale = locale(decimal_mark = ","))|>janitor::clean_names()
delta_t_diff <- read_delim(file_path_diff,locale = locale(decimal_mark = ","))|>janitor::clean_names()

#Transpose data for absolute values and difference values
delta_t_long <- delta_t|>
  dplyr::select(mean_dt, beyer_dt, chelsa_dt, ecoclimate_dt,paleopgem_dt,worldclim25m_dt,worldclim30s_dt,ggc_dt,gmted2010, in_mr) |>
  pivot_longer(cols = -gmted2010, 
               names_to = "Model", 
               values_to = "Values")

delta_diff_long <- delta_t_diff|>
  dplyr::select(mean_dt, beyer_diff_mean, chelsa_diff_mean, ecoclimate_diff_mean,paleopgem_diff_mean,worldclim25m_diff_mean,worldclim30s_diff_mean,ggc_diff_mean,gmted2010, in_mr) |>
  pivot_longer(cols = -gmted2010, 
               names_to = "Model", 
               values_to = "Values")

# visualise the absolute values of dt against elevation
ggplot(delta_diff_long,aes(x=Values, y=gmted2010,color=Model))+
  geom_point()+
  scale_y_continuous(breaks=c(0,min(delta_long$Values),
                              max(delta_long$Values)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
