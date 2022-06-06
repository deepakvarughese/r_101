library(tidyverse)
library(janitor)
library(here)
library(rio)


df <- import(here("Data", "dirty_data.xlsx"))


## Here we see that there is a header that we do not need and that R should use line 2 as the header

df_clean <- import(here("Data", "dirty_data.xlsx")) %>%    
  row_to_names(1, remove_rows_above = TRUE) %>%  
  clean_names %>% 
  remove_empty %>% 
  remove_constant()

df_clean$hire_date <- as.integer(df_clean$hire_date)
  
df_clean <- df_clean %>% 
  mutate(hire_date = excel_numeric_to_date(hire_date))

  



