############ Analysis #################
# OLS, interpretation and residual testing in this script.

######## Libraries and functions ########

library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)

######## Loading in ########
# All files in the data folder
list_of_files <- list.files(paste0(getwd(), "/data"))

# Loading the RIIO dataset
riio_data <- read.xlsx(paste0(getwd(), "/data/",list_of_files[grepl("RIIO", list_of_files)]), sheetName = "Dataset")

# Choosing the three data series and making the dataset tidy
riio_data_clean <- riio_data %>% filter(Selection %in% c("Interruptions", "CML performance")) %>%
  # Interruption satisfaction was not measured in 2014/15
  filter(Year != "2014/15") %>%
  # Dropping unnecessary columns
  select(-Schedule, -Section, -Category, -Units, -Industry.Average, -xllookup) %>% select(1:16) %>%
  # Pivoting longer
  pivot_longer(cols = 3:16, names_to = "DNO") %>%
  # Changing the 'value' column to numeric
  mutate(value = as.numeric(value)) %>%
  # Pivot wider to ensure each row is a single observation (i.e. one year and one DNO)
  pivot_wider(names_from = Selection, values_from = value) %>%
  # Changing the CML variable to its reciprocal and adding a numerical year variable
  mutate(oneover = 1/`CML performance`, year_num = as.numeric(substr(Year,1,4)))

# This dataset is saved
write.csv(riio_data_clean,file = "code_data/riio_data_clear.csv")

######## OLS ###############

