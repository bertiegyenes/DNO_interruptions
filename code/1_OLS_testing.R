############ Analysis #################
# OLS, interpretation and residual testing in this script.

######## Libraries and functions ########

library(xlsx)
library(dplyr)
library(tidyr)
library(broom)
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
# Two models are estimated

# First, one value for all DNOs
model_all_dno <- lm(riio_data_clean$Interruptions ~ riio_data_clean$oneover + riio_data_clean$year_num)

# Second, including dummy variables for DNOs
model_each_dno <- lm(riio_data_clean$Interruptions ~ riio_data_clean$oneover + riio_data_clean$year_num +
                       riio_data_clean$DNO)

# Constructing a table with the key features of the overall regression
ols_features <- data.frame(modelname = c("Model 1", "Model 2"),
                           adjusted_r_squared = c(summary(model_all_dno)$adj.r.squared,summary(model_each_dno)$adj.r.squared),
                           residual_st_error = c(summary(model_all_dno)$sigma,summary(model_each_dno)$sigma),
                           f_statistic = c(summary(model_all_dno)$fstatistic[1],summary(model_each_dno)$fstatistic[1]),
                           f_statistic_df1 = c(summary(model_all_dno)$fstatistic[2],summary(model_each_dno)$fstatistic[2]),
                           f_statistic_df2 = c(summary(model_all_dno)$fstatistic[3],summary(model_each_dno)$fstatistic[3]),
                           f_statistic_pvalue = c(pf(summary(model_all_dno)$fstatistic[1],
                                                     summary(model_all_dno)$fstatistic[2],
                                                     summary(model_all_dno)$fstatistic[3], lower.tail = FALSE),
                                                  pf(summary(model_each_dno)$fstatistic[1],
                                                     summary(model_each_dno)$fstatistic[2],
                                                     summary(model_each_dno)$fstatistic[3], lower.tail = FALSE)),
                           aic_values = c(glance(model_all_dno)$AIC,glance(model_each_dno)$AIC)) %>% t()
write.csv(ols_features, "code_data/ols_features.csv")

######## Residuals ##########




