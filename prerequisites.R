#### Prerequisites ####
# Locations
project_home <- getwd()
data_dir     <- file.path(project_home, "data")

#### Packages ####

# Reading and manipulating
library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)

# Output: Plotting
library(ggplot2)

# Output: Others
library(DT)
