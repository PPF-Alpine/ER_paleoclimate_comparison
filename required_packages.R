
# Install necessary packages if not already installed
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plotly)) install.packages("plotly")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
if (!require(effsize)) install.packages("effsize")

# Load the packages
library("readxl")
library("ggplot2")
library("tidyverse")
library("plotly")
library("dplyr")
library(effsize)


#run this if conflicts occur in loading tidyverse
# library(conflicted)  
# library(tidyverse)
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")


