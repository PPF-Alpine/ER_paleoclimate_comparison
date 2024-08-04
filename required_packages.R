#----------------------------------------------------------#
#
#           Paleoclimate comparison
#
#              Required packages
#                 
#
#           Author: Eline Rentier 
#                   2024
#
#----------------------------------------------------------#
# Install necessary packages if not already installed
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plotly)) install.packages("plotly")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(dplyr)) install.packages("dplyr")
if (!require(sf)) install.packages("sf")
if (!require(ggbeeswarm)) install.packages("ggbeeswarm")
if (!require(see)) install.packages("see")


# Load the packages
library("readxl")
library("ggplot2")
library("tidyverse")
library("plotly")
library("dplyr")
library("sf")
library("ggbeeswarm")
library("see")

#run this if conflicts occur in loading tidyverse
# library(conflicted)  
# library(tidyverse)
# conflict_prefer("filter", "dplyr")
# conflict_prefer("lag", "dplyr")
