######## Data descriptions #########
# The two key datasets are read in and are characterised


######## Libraries and functions ########

library(xlsx)
library(dplyr)
library(tidyr)

# The business datasets are all separate, these have to be called in separately
load_bpe <- function(f_loc){
  df_load <- try(read.xlsx(paste0(getwd(), "/data/",f_loc), sheetName = "Table 8",
            startRow = 9, endRow = 26), silent = TRUE)
  df_load <- df_load %>% 
    # First, removing all the all-NA columns
    select_if(function(x) !(all(is.na(x)))) %>%
    # Second, remove all the all-NA rows
    na.omit() %>%
    # Third, renaming all columns
    `colnames<-` (c("Region", "Population", "No_employees", "Employees_1_49", 
                    "Employees_5_249", "Employees_250_", "All_sizes")) %>%
    # No need to keep the 'All sizes' column
    select(-All_sizes) %>% 
    # There is also no need to keep values for the UK overall, or England and Northern Ireland
    filter(!(Region %in% c("United Kingdom", "England", "Northern Ireland"))) %>%
    # Add a year specifier
    mutate(Year = paste0(as.character(as.numeric(substr(f_loc,6,9))-1),"/",substr(f_loc,6,9))) %>%
    # Making sure that all values are double
    mutate(Population = as.double(Population))
    # The function uses the xlsx package which uses Java. Memory needs to be reset
    gc()
    return(df_load)
}

######## Loading in ########
# All files in the data folder
list_of_files <- list.files(paste0(getwd(), "/data"))

# Loading the RIIO dataset
riio_data <- read.xlsx(paste0(getwd(), "/data/",list_of_files[grepl("riio", list_of_files)]), sheetName = "Dataset")

# Choosing the three data series and making the dataset tidy
riio_data_clean <- riio_data %>% filter(Selection %in% c("Interruptions", "CI performance", "CML performance")) %>%
            # Interruption satisfaction was not measured in 2014/15
            filter(Year != "2014/15") %>%
            # Dropping unnecessary columns
            select(-Schedule, -Section, -Category, -Units, -Industry.Average, -xllookup) %>% select(1:16) %>%
            # Pivoting longer
            pivot_longer(cols = 3:16, names_to = "DNO")

# Loading the Business metrics dataset. The function already cleans these
bpe_data <- lapply(list_of_files[grepl("BPE", list_of_files)], load_bpe)
bpe_data_clean <- bind_rows(bpe_data)

