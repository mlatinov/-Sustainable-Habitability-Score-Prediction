
#### Libraries ####
library(tidyverse)
library(janitor)

# Load the data 
data_train <- read.csv("Data/train.csv")
data_test <- read_csv("Data/test.csv")

# Clean function 
clean_data <- function(data) {
  
  # Clean the data
  data <- data %>%
    
    # Remove emty spaces with NA
    mutate(across(everything(), ~ if_else(. == "", NA, .))) %>%
    
    # Apply transformations ONLY to character columns
    mutate(across(where(is.character), ~ str_replace_all(., " ", "_") |> str_to_lower())) %>%
    mutate(across(where(is.character), ~ str_replace_all(., "-+", "_"))) %>%
    mutate(across(where(is.character), ~ str_replace_all(., "_+", "_"))) %>%
    
    # Replace a specific value in Property_Type
    mutate(Property_Type = as.factor(if_else(Property_Type == "#r%$g&867", "unknown", Property_Type))) %>%
    
    # Convert only character columns to factors
    mutate(across(where(is.character), ~ as.factor(.))) %>%
    
    # Clean column names 
    clean_names()
}

# Apply Cleaning
data_train_clean <- clean_data(data = data_train)
data_test_clean <- clean_data(data = data_test)

# Save 
saveRDS(data_test_clean,file = "clean_test")
saveRDS(data_train_clean,file = "clean_train")




