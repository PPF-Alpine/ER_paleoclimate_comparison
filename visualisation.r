# libraries
install.packages("tidyverse")
library("tidyverse")
 
# Read the CSV file
file_path <-("C:\\Users\\elren9761\\OneDrive - University of Bergen\\Documents\\PhD\\Analyses\\visualisation_dT\\delta_t_diff.csv")
delta_t_diff <- read_delim(file_path,locale = locale(decimal_mark = ","))|>janitor::clean_names()

delta_long <- delta_t_diff|>
  dplyr::select(beyer_dt, chelsa_dt, ecoclimate_dt,paleopgem_dt,worldclim25m_dt,worldclim30s_dt,ggc_dt,gmted2010) |>
  pivot_longer(cols = -gmted2010, 
               names_to = "Model", 
               values_to = "Values")

# View the first few rows of the dataframe
print(head(delta_t_diff))


ggplot(delta_long,aes(x=gmted2010, y=Values,color=Model))+
  geom_point()+
  scale_y_continuous(breaks=c(0,min(delta_long$Values),
                              max(delta_long$Values)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))